#' Compute a remote sensing index from a time series of MODIS multispectral 
#' images
#'
#' \code{modFolderToVar} computes a remote sensing index from the spectral bands
#' of a time series of MODIS images. The images are specified by the path to
#' the folder that stores the imagery (resulting from the \code{\link{modMosaic}} 
#' function). The function returns a \code{RasterStack} with a time series of 
#' images with the index.
#'
#' The function requires the definition of the \code{src} and \code{fun} 
#' arguments. The \code{src} is usually the path resulting from 
#' \code{\link{modMosaic}}. The \code{fun} argument can be any function from
#' this package beginning with “var” (\code{\link{varNDVI}}, \code{\link{varEVI}},
#' etc.). Custom functions can also be implemented. If \code{fun = varRGB}, then
#' the argument \code{getStack} must be equal to \code{FALSE} and the 
#' red-gree-blue (RGB) images must be imported afterwards.
#'
#' @param src path to the folder with the MODIS multispectral images.
#' @param AppRoot the directory of the outcoming time series.
#' @param fun is a \code{function} that computes the remote sensing index.
#' @param getStack logical argument. If \code{TRUE}, returns the time series of
#' images as a \code{RasterStack}, otherwise the images are saved in the Hard
#' Drive Device (HDD).
#' @param overwrite logical argument. If \code{TRUE}, it overwrites the existing
#' images with the same name.
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param ... arguments for nested functions.
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # main output directory
#' src <- paste0(tempdir(),"/Path_for_downloading_folder")
#' print(src)
#' # download MOD09 images
#' modDownload(product = "MOD09GA",
#'             startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'             endDate = as.Date("03-01-2018", "%d-%m-%Y"),
#'             username = "username",
#'             password = "password",
#'             AppRoot = src, # output folder for tif images
#'             extract.tif = TRUE, 
#'             collection = 6,
#'             extent = ex.navarre)
#' # assign src.mod as the output folder from modMosaic
#' src.mod <- file.path(src, "Modis", "MOD09GA") # output directory
#' src.tif <- file.path(src.mod, "tif") # input directory
#' # mosaic the MODIS images
#' modMosaic(src.tif,
#'           AppRoot = src.mod,
#'           out.name = "Navarre")
#' # path to the folder with the mosaicked images
#' src.navarre <- file.path(src.mod, "Navarre")
#' # generate NDVI images of Navarre
#' src.variables <- file.path(src.mod, "Variables")
#' dir.create(src.variables)
#' modFolderToVar(src = src.navarre,
#'                fun = varEVI,
#'                scfun = getRGISToolsOpt("MOD09SCL"),
#'                AppRoot = src.variables,
#'                overwrite = TRUE)
#' # import mosaicked images (.tif) to the environment in `R'
#' flist <- list.files(file.path(src.variables,"EVI"),
#'                     pattern = "\\.tif$",
#'                     full.names = TRUE,
#'                     recursive = TRUE)
#' 
#' files.raster <- lapply(flist,raster)
#' spplot(files.raster[[1]],at=seq(-1,2.5))
#' }
modFolderToVar<-function(src,AppRoot,fun,getStack=FALSE,overwrite=FALSE,verbose=FALSE,...){
  function.arg<-list(...)
  src<-pathWinLx(src)
  AppRoot<-pathWinLx(AppRoot)
  vartype<-gsub("var","",as.character(match.call()[c("fun")]))
  if(!getStack){
    AppRoot<-file.path(AppRoot,vartype)
    dir.create(AppRoot,showWarnings = FALSE,recursive=TRUE)
    message(vartype)
  }
  mod.list<-list.files(src,full.names = TRUE)
  result<-NULL
  rstack<-NULL
  for(imgfd in mod.list){
    message(paste0("Calculating ",vartype," at date ",genGetDates(imgfd),"."))
    modbands<-getRGISToolsOpt("MOD09BANDS")
    mod.img<-list.files(imgfd,full.names = TRUE,pattern = "\\.tif$")
    out.file.name<-paste0(AppRoot,"/",vartype,"_",format(genGetDates(imgfd),"%Y%j"),".tif")
    if(overwrite|(!file.exists(out.file.name))){
      funString<-"result<-fun("
      #band load and asignation
      funargs<-formalArgs(fun)
      for(arg in funargs){
        band<-modbands[names(modbands)%in%arg]
        if(length(band)==0)
          next
        eval(parse( text=paste0(arg,"<-raster('",mod.img[grepl(tolower(band),mod.img)],"')") ))
        funString<-paste0(funString,arg,"=",arg,",")
      }
      # arguments asignation
      arguments<-as.list(match.call())
      arguments<-arguments[names(arguments)%in%funargs&
                           (!names(arguments)%in%names(modbands))]
      for(arg in names(arguments)){
        funString<-paste0(funString,arg,"=function.arg$",arg,",")
      }
      # complete the function
      funString<-paste0(substr(funString,1,nchar(funString)-1),")")
      if(verbose){message(paste0("Function for evaluation: \n",funString))}
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
    message(paste0(vartype," images saved in HDD."))
    message(paste0("File dir: ",AppRoot))
  }
}
