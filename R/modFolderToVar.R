#' Computes derived variables from Modis multispectral bands
#'
#' \code{\link{modFolderToVar}} calculates an index using the bands from Modis multispectral images.
#' The images are specified by a path to the storing folder (resulting from the \code{\link{modMosaic}} function).
#' The function returns a \code{RasterStack} with the time series of the index.
#'
#' The function requires the definition of src and fun arguments. The argument \code{src}
#' contains the path to the folder with the multispectral images. It can be easily
#' defined as the path resulting from \code{\link{modMosaic}}. The fun argument is a function with
#' the calculation of the index based on spectral bands. There are some pre-programmed
#' indices in \code{RGISTools}. Functions with the pre-programmed indexes start with var
#' (\code{\link{varNDVI}}, \code{\link{varEVI}}). The user can define its own functions.
#'
#' @param src path to the folder with the Modis multispectral images.
#' @param fun is a function defined by the package for computing indexes.
#' All functions in the package starting with three characters
#' 'var' are acceptable functions. Custom functions can be also implemented.
#' @param getStack logical argument. If \code{TRUE}, returns the time-series as a raster or otherwise as Hard Drive Devide (HDD).
#' @param overwrite logical argument. If \code{TRUE} overwrites the existing images with the same name.
#' @param ... argument to allow function nestering:
#' \itemize{
#'   \item \code{AppRoot} the directory of the resulting time series.
#' }
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of navarre for the example
#' data(ex.navarre)
#' # asign the folder where the example will be run
#' src <- "Path_for_downloading_folder"
#' # download Modis images
#' modDownload(product = "MOD09GA",
#'             startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'             endDate = as.Date("03-01-2018", "%d-%m-%Y"),
#'             username = "username",
#'             password = "password",
#'             AppRoot = src,
#'             hdfdir = "hdf",
#'             tiffdir = "tif",
#'             collection = 6,
#'             extent = ex.navarre)
#' # asign the folder with the Sentinel images untared
#' src1 <- file.path(src, "MOD09GA")
#' tif.src <- file.path(src, "tif")
#' #mosaic the Modis images
#' modMosaic(tif.src,
#'           AppRoot = src1,
#'           out.name = "Navarre")
#' # asign src as the path to mosaicked folder
#' src2 <- file.path(src1, "Navarre")
#' # generate NDVI images of Navarre
#' src3 <- file.path(src1, "Variables")
#' dir.create(src3)
#' modFolderToVar(src2,
#'                fun = varEVI,
#'                AppRoot = src3),
#'                overwrite = T)
#'                
#' flist <- list.files(file.path(src3,"EVI"),
#'                     pattern = "\\.tif$",
#'                     full.names = TRUE,
#'                     recursive = TRUE)
#' 
#' files.raster <-stack(flist)
#' spplot(files)
#' }
modFolderToVar<-function(src,fun,getStack=FALSE,overwrite=FALSE,...){
  AppRoot=defineAppRoot(...)
  vartype<-gsub("var","",as.character(match.call()[c("fun")]))
  if(!getStack){
    AppRoot<-file.path(AppRoot,vartype)
    dir.create(AppRoot,showWarnings = FALSE,recursive=TRUE)
    print(vartype)
  }
  mod.list<-list.files(src,full.names = TRUE)
  result<-NULL
  rstack<-NULL
  for(imgfd in mod.list){
    message(paste0("Calculating ",vartype," at date ",genGetDates(imgfd),"."))
    modbands<-paste0(getRGISToolsOpt("MOD09BANDS"),".tif")
    mod.img<-list.files(imgfd,full.names = TRUE,pattern = "\\.tif$")
    funString<-"result<-fun("
    for(arg in formalArgs(fun)){
      band<-modbands[names(modbands)%in%arg]
      if(length(band)==0)
        next
      eval(parse( text=paste0(arg,"<-raster('",mod.img[grepl(tolower(band),mod.img)],"')") ))
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
    message(paste0(vartype," images saved in HDD."))
    message(paste0("File dir: ",AppRoot))
  }
}
