#' Creates variables from Landsat 7 multispectral bands
#'
#' \code{ls7FolderToVar} calculates an index using the bands from Landsat multispectral
#' images. The images are specified by a path to the storing folder (resulting from
#' the \code{lsMosaic} function). The function returns a rasterStack with the time-series of the index.
#'
#' The function requires to define \code{src} and \code{fun} attributes. \code{src} defines the path to
#' the result of \code{\link{lsMosaic}}, with all bands of Landsat-7 for a region of interest. \code{fun} defines
#' the variable of interest using any of the functions in the packages starting with \code{var} (\code{\link{varNDVI}},
#'  \code{\link{varEVI}}, ...)
#'
#' @param src path to the folder with the Landsat multispectral image.
#' @param fun is a function with the calculation of the index.
#' All functions in the package starting with three characters
#' 'var' are acceptable functions. Custom functions can be also
#' implemented \code{var} are acceptable functions.
#' @param getStack logical argument. If \code{TRUE}, returns the time-series as a 
#' raster or otherwise as Hard Drive Devide (HDD).
#' @param overwrite logical argument. If \code{TRUE} overwrites the existing images with the same name.
#' @param ... argument to allow function nestering.
#' \itemize{
#'   \item \code{AppRoot} the directory of the resulting time series.
#' }
#'
#' @examples
#' \dontrun{
#' #load a spatial polygon object of navarre for the example
#' data(ex.navarre)
#' #asign the folder where the example will be run
#' src<-"Path_for_downloading_folder"
#' #download landsat7 images
#' search<-lsDownload(satellite="ls7",
#'                    username="username",
#'                    password="password",
#'                    startDate=as.Date("01-01-2018","%d-%m-%Y"),
#'                    endDate=as.Date("20-01-2018","%d-%m-%Y"),
#'                    extent=ex.navarre,
#'                    untarDir="untar",
#'                    AppRoot=src)
#' #asign the folder with the Landsat 7 images untared
#' tif.src<-file.path(src,"untar")
#' #mosaic the Landsat7 images
#' lsMosaic(tif.src,
#'          AppRoot=src,
#'          out.name="Navarre")
#' #asign src as the path to mosaiced folder
#' src<-file.path(src,"Navarre")
#' #generate NDVI images of Navarre
#' ls7FolderToVar(src,
#'                fun=varNDVI,
#'                AppRoot=file.path(dirname(src)),
#'                overwrite = T)
#' }
ls7FolderToVar<-function(src,fun,getStack=FALSE,overwrite=FALSE,...){
  AppRoot=defineAppRoot(...)
  if(!getStack){
    vartype<-gsub("var","",as.character(match.call()[c("fun")]))
    AppRoot<-file.path(AppRoot,vartype)
    dir.create(AppRoot,showWarnings = FALSE,recursive=TRUE)
    print(vartype)
  }

  ls.list<-list.files(src,full.names = TRUE)
  rstack<-stack()
  result<-raster()
  for(imgfd in ls.list){
    message(paste0("Calculating ",vartype," at date ",genGetDates(imgfd),"."))
    ls7bands<-getRGISToolsOpt("LS7BANDS")
    ls.img<-list.files(imgfd,full.names = TRUE,pattern = "\\.tif$")
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
      rstack<-addLayer(rstack,result)
    }else{
      writeRaster(result,paste0(AppRoot,"/",vartype,"_",format(genGetDates(imgfd),"%Y%j"),".tif"),overwrite=overwrite)
    }
  }
  if(getStack){
    return(result)
  }else{
    message(paste0(vartype," images saved in HDD"))
    message(paste0("File dir: ",AppRoot))
  }

}
