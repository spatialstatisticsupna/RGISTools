#' Compute a remote sensing index from a time series of Landsat-7 images 
#'
#' \link{ls7FolderToVar} computes a remote sensing index from the 
#' spectral bands of a time series of Landsat-7 images. The images are specified
#' by the path to the folder that stores the imagery (resulting from the 
#' \code{\link{lsMosaic}} function). The function returns a \code{RasterStack}
#' with the time series of images of the remote sensing index.
#'
#' The function requires the definition of the \code{src} and \code{fun} 
#' arguments. The \code{src} is usually the path resulting from 
#' \code{\link{lsMosaic}}. The \code{fun} argument can be any function from this
#' package beginning with “var” (\code{\link{varNDVI}}, \code{\link{varEVI}}, 
#' etc.). Custom functions can also be implemented.
#'
#' @param src the path to the folder with the Landsat-7 multispectral imagery.
#' @param fun a \code{function} that computes the remote sensing index.
#' @param AppRoot the directory of the outcoming time series.
#' @param getStack logical argument. If \code{TRUE}, returns the time series 
#' as a \code{RasterStack}, otherwise the images are saved in the Hard Drive
#' Device (HDD).
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing
#' images with the same name.
#' @param verbose logical argument. If \code{TRUE}, the function prints the
#' running steps and warnings.
#' @param ... arguments for nested functions.
#'  \itemize{
#'   \item \code{dates} a vector with the capturing dates being considered
#'   for mosaicking. If not supplied, all dates are mosaicked.
#' }
#' 
#' @return this function does not return anything, unless \code{getStack = TRUE}
#' which then returns a \code{RasterStack} with the time series of with the
#' index. 
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # main output directory
#' wdir <- file.path(tempdir(),"Path_for_downloading_folder")
#' print(wdir)
#' # download Landsat-7 images
#' auxDownload(satellite = "ls7",
#'            username = "username",
#'            password = "password",
#'            startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'            endDate = as.Date("20-01-2018", "%d-%m-%Y"),
#'            extent = ex.navarre,
#'            untar = TRUE,
#'            AppRoot = wdir)
#' # folder with the Landsat-7 untared images 
#' wdir.ls7 <-file.path(wdir,"Landsat7")
#' wdir.ls7.untar <- file.path(wdir.ls7, "untar")
#' # mosaic the Landsat-7 images
#' lsMosaic(wdir.ls7.untar,
#'          AppRoot = wdir.ls7,
#'          out.name = "Navarre",
#'          extent = ex.navarre,
#'          gutils = TRUE)
#' # folder with the mosaicked images
#' wdir.ls7.navarre <- file.path(wdir.ls7, "Navarre")
#' # generate NDVI images of Navarre
#' wdir.ls7.var <- file.path(wdir.ls7, "Navarre_Variables")
#' dir.create(wdir.ls7.var)
#' ls7FolderToVar(wdir.ls7.navarre,
#'                fun = varNDVI,
#'                AppRoot = wdir.ls7.var,
#'                overwrite = TRUE)
#'                
#' files.ls7.ndvi <- list.files(file.path(wdir.ls7.var,"NDVI"),
#'                              pattern = "\\.tif$",
#'                              full.names = TRUE,
#'                              recursive = TRUE)
#' img.ls7.ndvi <- raster(files.ls7.ndvi[1])
#' spplot(img.ls7.ndvi)
#' }
ls7FolderToVar<-function(src,fun,AppRoot,getStack=FALSE,overwrite=FALSE,verbose=FALSE,...){
  src<-pathWinLx(src)
  AppRoot<-pathWinLx(AppRoot)
  function.arg<-list(...)
  vartype<-gsub("var","",as.character(match.call()[c("fun")]))
  if(!getStack){
    AppRoot<-file.path(AppRoot,vartype)
    dir.create(AppRoot,showWarnings = FALSE,recursive=TRUE)
    message(vartype)
  }

  ls.list<-list.files(src,full.names = TRUE)
  
  rstack<-NULL
  result<-NULL
  
  dates<-genGetDates(ls.list)
  if("dates"%in%names(function.arg)){
    ls.list<-ls.list[dates%in%function.arg$dates]
  }
  if(length(ls.list)==0)stop(paste0("No images found in ",src," path."))
  for(imgfd in ls.list){
    message(paste0("Calculating ",vartype," at date ",genGetDates(imgfd),"."))
    ls7bands<-getRGISToolsOpt("LS7BANDS")
    ls.img<-list.files(imgfd,full.names = TRUE,pattern = "\\.tif$")
    out.file.name<-paste0(AppRoot,"/",vartype,"_",format(genGetDates(imgfd),"%Y%j"),".tif")
    if(overwrite|(!file.exists(out.file.name))){
      funString<-"result<-fun("
      #band load and asignation
      funargs<-formalArgs(fun)
      for(arg in funargs){
        band<-ls7bands[names(ls7bands)%in%arg]
        if(length(band)==0)
          next
        eval(parse( text=paste0(arg,"<-raster('",ls.img[grepl(band,ls.img)],"')") ))
        funString<-paste0(funString,arg,"=",arg,",")
      }
      # arguments asignation
      arguments<-as.list(match.call())
      arguments<-arguments[names(arguments)%in%funargs&
                             (!names(arguments)%in%names(ls7bands))]
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
    message(paste0(vartype," images saved in HDD"))
    message(paste0("File dir: ",AppRoot))
  }

}
