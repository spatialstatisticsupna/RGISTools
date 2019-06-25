#' Computes derived variables from Landsat-8 multispectral bands
#'
#' \code{ls8FolderToVar} calculates an index using the bands from Landsat-8 multispectral
#' images. The images are specified by a path to the storing folder (resulting from
#' the \code{\link{lsMosaic}} function). The function returns a \code{RasterStack} with the time-series of the index.
#'
#' The function requires to define \code{src} and \code{fun} attributes. \code{src} defines the path to
#' the result of \code{\link{lsMosaic}}, with all bands of Landsat-8 for a region of interest. \code{fun} defines
#' the variable of interest using any of the functions in the packages starting with \code{var} (\code{\link{varNDVI}},
#'  \code{\link{varEVI}}, ...)
#'
#' @param src path to the folder with the Landsat multispectral image.
#' @param fun is a function defined by the package for computing indexes.
#' All functions in the package starting with three characters
#' 'var' are acceptable functions. Custom functions can be also
#' implemented \code{var} are acceptable functions.
#' @param getStack logical argument. If \code{TRUE}, returns the 
#' time-series of images as a \code{RasterStack}, otherwise as Hard Drive Devide (HDD).
#' @param overwrite logical argument. If \code{TRUE} overwrites the 
#' existing images with the same name.
#' @param ... argument to allow function nestering
#' \itemize{
#'   \item \code{AppRoot} the directory of the resulting time series
#' }
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of navarre for the example
#' data(ex.navarre)
#' # asign the folder where the example will be run
#' src <- "Path_for_downloading_folder"
#' # download Landsat-8 images
#' lsDownload(satellite = "ls8",
#'            username = "username",
#'            password = "password",
#'            startDate = as.Date("01-01-2018","%d-%m-%Y"),
#'            endDate = as.Date("20-01-2018","%d-%m-%Y"),
#'            extent = ex.navarre,
#'            untar = TRUE,
#'            AppRoot = src)
#' # asign the folder with the Landsat-8 images untared
#' tif.src <- file.path(src, "untar")
#' # mosaic the Landsat-8 images
#' lsMosaic(tif.src,
#'          AppRoot = src,
#'          out.name = "Navarre")
#' # asign src as the path to mosaiced folder
#' src2 <- file.path(src, "Navarre")
#' # generate NDVI images of Navarre
#' src3 <- file.path(src1, "Navarre_Variables")
#' dir.create(src3)
#' ls8FolderToVar(src2,
#'                fun = varNDVI,
#'                AppRoot = src3,
#'                overwrite = T)
#'                
#' flist <- list.files(file.path(src3,"EVI"),
#'                     pattern = "\\.tif$",
#'                     full.names = TRUE,
#'                     recursive = TRUE)
#' 
#' files.raster <- stack(flist)
#' spplot(files)
#' }
ls8FolderToVar<-function(src,fun,getStack=FALSE,overwrite=FALSE,...){
  AppRoot=defineAppRoot(...)
  vartype<-gsub("var","",as.character(match.call()[c("fun")]))
  if(!getStack){
    AppRoot<-file.path(AppRoot,vartype)
    dir.create(AppRoot,showWarnings = F,recursive=T)
    print(vartype)
  }
  ls.list<-list.files(src,full.names = T)
  rstack<-NULL
  result<-NULL
  for(imgfd in ls.list){
    message(paste0("Calculating ",vartype," at date ",genGetDates(imgfd),"."))
    ls7bands<-getRGISToolsOpt("LS8BANDS")
    ls.img<-list.files(imgfd,full.names = T,pattern = "\\.tif$")
    funString<-"result<-fun("
    for(arg in formalArgs(fun)){
      band<-ls7bands[names(ls7bands)%in%arg]
      if(length(band)==0)
        next
      eval(parse( text=paste0(arg,"<-raster('",ls.img[grepl(band,ls.img)],"')") ))
      funString<-paste0(funString,arg,"=",arg,",")
    }
    funString<-paste0(substr(funString,1,nchar(funString)-1),")")
    eval(parse(text=funString))
    if(getStack){
      if(is.null(rstack)){
        names(result)<-paste0(vartype,"_",format(genGetDates(imgfd),"%Y%j"))
        rstack<-result
      }else{
        result<-extend(result,rstack)
        names(result)<-paste0(vartype,"_",format(genGetDates(imgfd),"%Y%j"))
        rstack<-extend(rstack,result)
        rstack<-addLayer(rstack,result)
      }
    }else{
      writeRaster(result,paste0(AppRoot,"/",vartype,"_",format(genGetDates(imgfd),"%Y%j"),".tif"),overwrite=overwrite)
    }
  }
  if(getStack){
    return(rstack)
  }else{
    message(paste0(vartype," images saved in HDD"))
    message(paste0("File dir: ",AppRoot))
  }
}
