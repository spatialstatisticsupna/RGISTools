#' Calculates an index from Sentinel-2 multispectral bands
#'
#' \code{senFolderToVar} calculates an index using the bands from Sentinel-2 multispectral
#' images. The images are specified by a path to the storing folder
#' (resulting from the senMosaic function). The function returns a
#' \code{rasterStack} with the time-series of the index.
#'
#' The function requires the definition of src and fun arguments. The argument \code{src}
#'  contains the path to the folder with the multispectral
#'  images. It can be easily defined as the path resulting from \code{senMosaic}.
#'  The fun argument is a function with the calculation of the index based
#'  on spectral bands. There are some pre-programmed indexes in \code{RGISTools}.
#'  Functions with the pre-programmed indices start with var (\code{varNDVI}, \code{varEVI}).
#'  The user can define its own functions.
#'
#' @param src path to the folder with the Sentinel multispectral images
#' @param fun function with the calculation of the index.
#' All functions in the package starting with three characters
#' 'var' are acceptable functions. Custom functions can be also implemented.
#' @param getStack if \code{TRUE}, returns the time-series as a raster or otherwise as Hard Drive Devide (HDD)
#' @param overwrite flag to overwrite the existing images with the same name
#' @param ... argument to allow function nestering
#' \itemize{
#'   \item \code{AppRoot} the directory of the resulting time series
#' }
#'
#' @examples
#' \dontrun{
#' #load a spatial polygon object of navarre for the example
#' data(ex.navarre)
#' #asign the folder where the example will be run
#' src<-"Path_for_downloading_folder"
#' #download sentinel images
#' senDownload(startDate=as.Date("2018210","%Y%j"),
#'             endDate=as.Date("2018218","%Y%j"),
#'             platform="Sentinel-2",
#'             extent=ex.navarre,
#'             product="S2MSI1C",
#'             pathrow=c("R094"),
#'             username="username",
#'             password="password",
#'             AppRoot=src)
#' #asign the folder with the sentinel images unzipped
#' src.unzip<-file.path(src,"unzip")
#' #mosaic the sentinel images
#' senMosaic(src.unzip,
#'           AppRoot=src,
#'           gutils=T,
#'           out.name="Navarre")
#' #asign src as the path to mosaiced folder
#' src<-file.path(src,"Navarre")
#' #generate EVI images of Navarre
#' senFolderToVar(src,
#'                fun=varEVI,
#'                AppRoot=file.path(dirname(src)))
#' }
senFolderToVar<-function(src,fun,getStack=F,overwrite=F,...){
  AppRoot=defineAppRoot(...)
  if(!getStack){
    vartype<-gsub("var","",as.character(match.call()[c("fun")]))
    AppRoot<-file.path(AppRoot,vartype)
    dir.create(AppRoot,showWarnings = F,recursive=T)
    print(vartype)
  }

  sen.list<-list.files(src,full.names = T)
  rstack<-stack()
  result<-raster()
  for(imgfd in sen.list){
    message(paste0("Calculating ",vartype," at date ",genGetDates(imgfd),"."))
    senbands<-getRGISToolsOpt("SEN2BANDS")
    sen.img<-list.files(imgfd,full.names = T,pattern = "\\.tif$")
    funString<-"result<-fun("
    for(arg in formalArgs(fun)){
      band<-senbands[names(senbands)%in%arg]
      if(length(band)==0)
        next
      eval(parse( text=paste0(arg,"<-raster('",sen.img[grepl(band,sen.img)],"')") ))
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
    message(paste0(vartype," images saved in HHD"))
    message(paste0("File dir: ",AppRoot))
  }
}
