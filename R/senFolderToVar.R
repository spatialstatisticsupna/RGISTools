#' Compute a remote sensing index from a time series of Sentinel-2 images
#'
#' \code{senFolderToVar} computes a remote sensing index from the spectral bands
#' of a time series of Sentinel-2 images. The images are specified by the path to
#' the folder that stores the imagery (resulting from the \code{\link{senMosaic}} 
#' function). The function returns a \code{RasterStack} with a time series of 
#' images with the index.
#' 
#' The function requires the definition of the \code{src} and \code{fun} 
#' arguments. The \code{src} is usually the path resulting from 
#' \code{\link{senMosaic}}. The \code{fun} argument can be any function from
#' this package beginning with “var” (\code{\link{varNDVI}}, 
#' \code{\link{varEVI}}, etc.). Custom functions can also be implemented.
#' If \code{fun = varRGB}, then the argument \code{getStack} must be equal to
#' \code{FALSE} and the red-green-blue (RGB) images must be imported afterwards.
#' 
#' @param src the path to the folder with the Sentinel-2 multispectral images.
#' @param AppRoot directory where the outcoming time series is saved.
#' @param fun a \code{function} that computes the remote sensing index.
#' @param getStack logical argument. If \code{TRUE}, returns the time series of
#' images as a \code{RasterStack}, otherwise the images are saved in the Hard
#' Drive Device (HDD).
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing
#' images with the same name.
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param resbands the resolution of the image being used to compute index,
#' when the imagery comes from the Senintel-2 "\code{S2MSI2A}" product. By 
#' default, all resolutions (10m, 20m, and 60m) are used.
#' @param ... arguments for nested functions.
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # main output directory
#' src <- paste0(tempdir(),"/Path_for_downloading_folder")
#' print(src)
#' # download Sentinel-2 images
#' senDownload(startDate = as.Date("2018210","%Y%j"),
#'             endDate = as.Date("2018218","%Y%j"),
#'             platform = "Sentinel-2",
#'             extent = ex.navarre,
#'             product = "S2MSI1C",
#'             pathrow = c("R094"),
#'             username = "username",
#'             password = "password",
#'             AppRoot = src)
#' # folder with the unzipped images from Sentinel-2
#' src.sen <- file.path(src,"Sentinel-2")
#' src.unzip <- file.path(src.sen, "unzip")
#' # mosaic the Sentinel-2 images
#' senMosaic(src.unzip,
#'           AppRoot = src.sen,
#'           gutils = TRUE,
#'           out.name = "Navarre")
#' # path to the folder with the mosaicked images
#' src2 <- file.path(src.sen, "Navarre")
#' src3 <- file.path(src.sen, "Navarre_Variables")
#' dir.create(src3)
#' # generate EVI images of Navarre
#' senFolderToVar(src2,
#'                fun = varEVI,
#'                resbands = c("60m"),
#'                AppRoot = src3)
#'                
#' flist <- list.files(file.path(src3,"EVI"),
#'                     pattern = "\\.tif$",
#'                     full.names = TRUE,
#'                     recursive = TRUE)
#' 
#' files.raster <- lapply(flist, raster)
#' spplot(files.raster[[1]])
#' }
senFolderToVar<-function(src,AppRoot,fun,getStack=FALSE,overwrite=FALSE,verbose=FALSE,resbands=c("10m","20m","60m"),...){
  function.arg<-list(...)
  vartype<-gsub("var","",as.character(match.call()[c("fun")]))
  src<-pathWinLx(src)
  AppRoot<-pathWinLx(AppRoot)
  AppRoot<-file.path(AppRoot,vartype)
  dir.create(AppRoot,showWarnings = FALSE,recursive=TRUE)
  if(verbose){message(paste0("var type: ",vartype))}
  resbands=paste0("_",resbands)
  sen.list<-list.files(src,full.names = TRUE)
  rstack<-NULL
  result<-NULL
  for(imgfd in sen.list){
    message(paste0("Calculating ",vartype," at date ",genGetDates(imgfd),"."))
    senbands<-getRGISToolsOpt("SEN2BANDS")
    sen.img<-list.files(imgfd,full.names = TRUE,pattern = "\\.tif$")
    
    #check if there are S2MSI2A images
    if(sum(unlist(lapply(resbands,grepl,sen.img)))>0){
      if(verbose){message("Multiple resolution layers, getStack not supported.")}
      getStack=FALSE
    }else{
      resbands=c("")
    }
    
    for(resb in resbands){
      out.file.name<-paste0(AppRoot,"/",vartype,"_",format(genGetDates(imgfd),"%Y%j"),resb,".tif")
      if(overwrite|(!file.exists(out.file.name))){
        funString<-"result<-fun("
        #band load and asignation
        funargs<-formalArgs(fun)
        for(arg in funargs){
          band<-senbands[names(senbands)%in%arg]
          if(length(band)==0)
            next
          l.img<-sen.img[grepl(paste0(band,resb,".tif"),sen.img)]
          if(length(l.img)==0&arg=='nir'){
            band<-senbands[names(senbands)%in%'narrownir']
            l.img<-sen.img[grepl(paste0(band,resb,".tif"),sen.img)]
          }
          if(verbose){message(paste0("Loading ",l.img,"..."))}
          eval(parse( text=paste0(arg,"<-raster('",l.img,"')") ))
          funString<-paste0(funString,arg,"=",arg,",")
        }
        # arguments asignation
        arguments<-as.list(match.call())
        arguments<-arguments[names(arguments)%in%funargs&
                               (!names(arguments)%in%names(senbands))]
        for(arg in names(arguments)){
          funString<-paste0(funString,arg,"=function.arg$",arg,",")
        }
        # complete the function
        funString<-paste0(substr(funString,1,nchar(funString)-1),")")
        if(verbose){message(paste0("Running function ",funString,"..."))}
        eval(parse(text=funString))
        
        if(getStack){
          if(is.null(rstack)){
            names(result)<-paste0(vartype,"_",format(genGetDates(imgfd),"%Y%j"))
            rstack<-result
          }else{
            result<-extend(result,rstack)
            rstack<-extend(rstack,result)
            names(result)<-paste0(vartype,"_",format(genGetDates(imgfd),"%Y%j"))
            rstack<-addLayer(rstack,result)
          }
        }else{
          writeRaster(result,out.file.name,overwrite=overwrite)
        }
      }else{
        message(paste0("File exists!\nFile: ",out.file.name))
      }
      
    }
  }
  if(getStack){
    return(rstack)
  }else{
    message(paste0(vartype," images saved in HDD."))
    message(paste0("File dir: ",AppRoot))
  }
}
