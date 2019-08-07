#' Compute derived variables from Modis multispectral bands
#'
#' \code{\link{modFolderToVar}} calculates an index using the bands from Modis multispectral images.
#' The images are specified by a path to the storing folder (resulting from the \code{\link{modMosaic}} function).
#' The function returns a \code{RasterStack} with the index time series.
#'
#' The function requires the definition of \code{src} and \code{fun} arguments. The argument \code{src}
#' specifies the path to the output folder with the multispectral images, where a new folder with the 
#' name of the product is created.
#' This function works with the path to the folder containing the GTiff layers derived from any \code{MOD09} product, or 
#' the path to the folder with the result of  \code{\link{modMosaic}}.
#' The \code{fun} argument is a function with
#' the calculation of an index based on the spectral bands. There are some pre-programmed
#' indixes in \code{RGISTools}. Functions with the pre-programmed indexes start with var prefix
#' (\code{\link{varNDVI}}, \code{\link{varEVI}}). The user can define its own functions.
#'
#' @param src path to the input folder with the Modis multispectral images.
#' @param fun is a function defined for computing indexes.
#' All the functions starting with the "var" prefix are available 
#' functions. Customized functions can also be implemented.
#' @param getStack logical argument. If \code{TRUE}, returns the time series as a \code{RasterStack}, otherwise the result 
#' is saved in the Hard Drive Device (HDD).
#' @param overwrite logical argument. If \code{TRUE}, it overwrites the existing images with the same name.
#' @param ... argument for function nestering:
#' \itemize{
#'   \item \code{AppRoot} directory of the output time series.
#' }
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # assign the main output directory
#' src <- "Path_for_downloading_folder"
#' # download Modis images
#' modDownload(product = "MOD09GA",
#'             startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'             endDate = as.Date("03-01-2018", "%d-%m-%Y"),
#'             username = "username",
#'             password = "password",
#'             AppRoot = src, # output folder for tif images
#'             extract.tif = TRUE, 
#'             collection = 6,
#'             extent = ex.navarre)
#' # assign src.mod as the outut folder for ModMosaic
#' src.mod <- file.path(src, "Modis", "MOD09GA") # output directory
#' src.tif <- file.path(src.mod, "tif") # input directory
#' # mosaic the Modis images
#' modMosaic(src.tif,
#'           AppRoot = src.mod,
#'           out.name = "Navarre")
#' # assign src as the path to mosaicked folder
#' src.navarre <- file.path(src.mod, "Navarre")
#' # generate NDVI images of Navarre
#' src.variables <- file.path(src.mod, "Variables")
#' dir.create(src.variables)
#' modFolderToVar(src.navarre,
#'                fun = varEVI,
#'                AppRoot = src.variables,
#'                overwrite = T)
#' # import tif mosaicked images to R environment
#' flist <- list.files(file.path(src.variables,"EVI"),
#'                     pattern = "\\.tif$",
#'                     full.names = TRUE,
#'                     recursive = TRUE)
#' 
#' files.raster <-stack(flist)
#' spplot(files.raster,at=seq(-1,2.5))
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
    modbands<-getRGISToolsOpt("MOD09BANDS")
    mod.img<-list.files(imgfd,full.names = TRUE,pattern = "\\.tif$")
    out.file.name<-paste0(AppRoot,"/",vartype,"_",format(genGetDates(imgfd),"%Y%j"),".tif")
    if(overwrite|(!file.exists(out.file.name))){
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
