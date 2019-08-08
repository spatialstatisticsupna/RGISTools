#' Compute derived variables from Landsat-8 multispectral bands
#'
#' \code{ls8FolderToVar} calculates an index using the bands from Landsat-8 multispectral
#' images. The images are specified by a path to the storing folder (resulting from
#' the \code{\link{lsMosaic}} function). The function returns a \code{RasterStack} with the index time series.
#'
#' The function requires to define \code{src} and \code{fun} input attributes. \code{src} defines the path to
#' the result of \code{\link{lsMosaic}}, with all bands of Landsat-8 for a region of interest. \code{fun} defines
#' the variable of interest using any of the functions in the packages starting with \code{var} prefix (\code{\link{varNDVI}},
#'  \code{\link{varEVI}}, ...)
#'
#' @param src path to the folder with the Landsat multispectral image.
#' @param fun is a function defined for computing indexes.
#' All functions created in RGISTools starting with
#' 'var' are avaliable functions. Custom functions can be also
#' implemented \code{var} are acceptable functions.
#' @param getStack logical argument. If \code{TRUE}, returns the 
#' time-series of images as a \code{RasterStack}, otherwise the images are saved in the Hard Drive Devide (HDD).
#' @param overwrite logical argument. If \code{TRUE}, overwrites the 
#' existing images with the same name.
#' @param verbose logical argument. If \code{TRUE}, the function prints running stages and warnings.
#' @param ... argument for function nestering
#' \itemize{
#'   \item \code{AppRoot} the directory of the resulting time series
#' }
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # assign the main output directory
#' src <- "Path_for_downloading_folder"
#' # download Landsat-8 images
#' lsDownload(satellite = "ls8",
#'            username = "username",
#'            password = "password",
#'            startDate = as.Date("01-01-2018","%d-%m-%Y"),
#'            endDate = as.Date("18-01-2018","%d-%m-%Y"),
#'            pathrow = list(c(200, 31), c(200, 30)),
#'            untar = TRUE,
#'            AppRoot = src)
#' # assign the folder with the Landsat-8 untared images
#' src.ls8 <-file.path(src,"Landsat8")
#' tif.src <- file.path(src.ls8, "untar")
#' # mosaic the Landsat-8 images
#' lsMosaic(src = tif.src,
#'          AppRoot = src.ls8,
#'          out.name = "Navarre",
#'          extent = ex.navarre,
#'          gutils = TRUE)
#' # assign src as the path to mosaiced folder
#' src2 <- file.path(src.ls8, "Navarre")
#' # generate NDVI images of Navarre
#' src3 <- file.path(src.ls8, "Navarre_Variables2")
#' dir.create(src3)
#' ls8FolderToVar(src2,
#'                fun = varNDVI,
#'                AppRoot = src3,
#'                overwrite = TRUE)
#'                
#' flist <- list.files(file.path(src3,"NDVI"),
#'                     pattern = "\\.tif$",
#'                     full.names = TRUE,
#'                     recursive = TRUE)
#' 
#' file.raster <- raster(flist[1])
#' spplot(files.raster)
#' 
#' }
ls8FolderToVar<-function(src,fun,getStack=FALSE,overwrite=FALSE,verbose=FALSE,...){
  AppRoot=defineAppRoot(...)
  function.arg<-list(...)
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
    ls8bands<-getRGISToolsOpt("LS8BANDS")
    ls.img<-list.files(imgfd,full.names = T,pattern = "\\.tif$")
    out.file.name<-paste0(AppRoot,"/",vartype,"_",format(genGetDates(imgfd),"%Y%j"),".tif")
    if(overwrite|(!file.exists(out.file.name))){
      funString<-"result<-fun("
      #band load and asignation
      funargs<-formalArgs(fun)
      for(arg in funargs){
        band<-ls8bands[names(ls8bands)%in%arg]
        if(length(band)==0)
          next
        eval(parse( text=paste0(arg,"<-raster('",ls.img[grepl(band,ls.img)],"')") ))
        funString<-paste0(funString,arg,"=",arg,",")
      }
      # arguments asignation
      arguments<-as.list(match.call())
      arguments<-arguments[names(arguments)%in%funargs&
                             (!names(arguments)%in%names(ls8bands))]
      for(arg in names(arguments)){
        funString<-paste0(funString,arg,"=function.arg$",arg,",")
      }
      # complete the function
      funString<-paste0(substr(funString,1,nchar(funString)-1),")")
      if(verbose){print(paste0("Function for evaluation: \n",funString))}
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
        writeRaster(result,out.file.name,overwrite=overwrite)
      }
    }else{
      message(paste0("File exists!\nFile: ",out.file.name))
    }
  }
  if(getStack){
    return(rstack)
  }else{
    message(paste0(vartype," images saved in HDD"))
    message(paste0("File dir: ",AppRoot))
  }
}
