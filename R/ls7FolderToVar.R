#' Compute a remote sensing index from a time series of Landsat-7 images 
#'
#' \link{ls7FolderToVar} computes a remote sensing index from the 
#' spectral bands of a time series of Landsat-7 images. The images are specified
#' by the path to the folder that stores the imagery (resulting from the 
#' \code{\link{lsMosaic}} function). The function returns a \code{RasterStack}
#' with the time series of images with the index.
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
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # main output directory
#' src <- paste0(tempdir(),"/Path_for_downloading_folder")
#' print(src)
#' # download Landsat-7 images
#' lsDownload(satellite = "ls7",
#'            username = "username",
#'            password = "password",
#'            startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'            endDate = as.Date("20-01-2018", "%d-%m-%Y"),
#'            extent = ex.navarre,
#'            untar = TRUE,
#'            AppRoot = src)
#' # folder with the Landsat-7 untared images 
#' src.ls7 <-file.path(src,"Landsat7")
#' tif.src <- file.path(src.ls7, "untar")
#' # mosaic the Landsat-7 images
#' lsMosaic(tif.src,
#'          AppRoot = src.ls7,
#'          out.name = "Navarre",
#'          extent = ex.navarre,
#'          gutils = TRUE)
#' # folder with the mosaicked images
#' src2 <- file.path(src.ls7, "Navarre")
#' # generate NDVI images of Navarre
#' src3 <- file.path(src.ls7, "Navarre_Variables")
#' dir.create(src3)
#' ls7FolderToVar(src2,
#'                fun = varNDVI,
#'                AppRoot = src3,
#'                overwrite = TRUE)
#'                
#' flist <- list.files(file.path(src3,"NDVI"),
#'                     pattern = "\\.tif$",
#'                     full.names = TRUE,
#'                     recursive = TRUE)
#' ras <- raster(flist[1])
#' spplot(ras)
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
