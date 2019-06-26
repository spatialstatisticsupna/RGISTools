#' Computes derived variables from Sentinel-2 multispectral bands
#'
#' \code{senFolderToVar} calculates an index using the bands from Sentinel-2 multispectral
#' images. The images are specified by a path to the storing folder
#' (resulting from the \code{\link{senMosaic}} function). The function returns a
#' \code{rasterStack} with the time-series of the index.
#'
#' The function requires the definition of src and fun arguments. The argument \code{src}
#'  contains the path to the folder with the multispectral
#'  images. It can be easily defined as the path resulting from \code{\link{senMosaic}}.
#'  The fun argument is a function with the calculation of the index based
#'  on spectral bands. There are some pre-programmed indexes in \code{RGISTools}.
#'  Functions with the pre-programmed indices start with var (\code{\link{varNDVI}}, \code{\link{varEVI}}).
#'  The user can define its own functions.
#'
#' @param src path to the folder with the Sentinel multispectral images.
#' @param fun is a function defined by the package for computing indexes.
#' All functions in the package starting with three characters.
#' 'var' are acceptable functions. Custom functions can be also implemented.
#' @param getStack logical argument. If \code{TRUE}, returns the time-series as a raster or otherwise as Hard Drive Devide (HDD).
#' @param overwrite logical argument. If \code{TRUE} overwrites the existing images with the same name.
#' @param verbose logical argument. If \code{TRUE} the function prints running stages and warnings.
#' @param resbands using Sentinel \code{S2MSI2A} products specifies the resolution of the output images. By default all 
#' the resolution (10m, 20m and 60m) are calculated.
#' @param ... argument to allow function nestering:
#' \itemize{
#'   \item \code{AppRoot} the directory of the resulting time series.
#' }
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of navarre for the example
#' data(ex.navarre)
#' # assign the folder where the example will be run
#' src <- "Path_for_downloading_folder"
#' # download Sentinel images
#' senDownload(startDate = as.Date("2018210","%Y%j"),
#'             endDate = as.Date("2018218","%Y%j"),
#'             platform = "Sentinel-2",
#'             extent = ex.navarre,
#'             product = "S2MSI1C",
#'             pathrow = c("R094"),
#'             username = "username",
#'             password = "password",
#'             AppRoot = src)
#' # assign the folder with the Sentinel images unzipped
#' src.unzip <- file.path(src, "unzip")
#' # mosaic the Sentinel images
#' senMosaic(src.unzip,
#'           AppRoot = src,
#'           gutils = TRUE,
#'           out.name = "Navarre")
#' # assign src as the path to mosaicked folder
#' src2 <- file.path(src, "Navarre")
#' src3 <- file.path(src, "Navarre_Variables")
#' dir.create(src3)
#' # generate EVI images of Navarre
#' senFolderToVar(src2,
#'                fun = varEVI,
#'                AppRoot = src3)
#'                
#' flist <- list.files(file.path(src3,"EVI"),
#'                     pattern = "\\.tif$",
#'                     full.names = TRUE,
#'                     recursive = TRUE)
#' 
#' files.raster <- stack(flist)
#' spplot(files)
#' }
senFolderToVar<-function(src,fun,getStack=FALSE,overwrite=FALSE,verbose=FALSE,resbands=c("10m","20m","60m"),...){
  AppRoot=defineAppRoot(...)
  vartype<-gsub("var","",as.character(match.call()[c("fun")]))

  AppRoot<-file.path(AppRoot,vartype)
  dir.create(AppRoot,showWarnings = FALSE,recursive=TRUE)
  if(verbose){message(paste0("var type: ",vartype))}
  resbands=paste0("_",resbands)
  sen.list<-list.files(src,full.names = T)
  rstack<-NULL
  result<-NULL
  for(imgfd in sen.list){
    message(paste0("Calculating ",vartype," at date ",genGetDates(imgfd),"."))
    senbands<-getRGISToolsOpt("SEN2BANDS")
    sen.img<-list.files(imgfd,full.names = T,pattern = "\\.tif$")
    
    #check if there are S2MSI2A images
    if(sum(unlist(lapply(resbands,grepl,sen.img)))>0){
      if(verbose){message("Multiple resolution layers, getStack not supported.")}
      getStack=FALSE
    }else{
      resbands=c("")
    }
    
    for(resb in resbands){
      funString<-"result<-fun("
      for(arg in formalArgs(fun)){
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
        writeRaster(result,paste0(AppRoot,"/",vartype,"_",format(genGetDates(imgfd),"%Y%j"),resb,".tif"),overwrite=overwrite)
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
