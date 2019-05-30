#' Imports time series of images and saves as RData
#'
#' \code{genSaveTSRData} imports time series of (tif) satellite images into R from a folder and creates an RData.
#'
#' The function reads all the images inside the folder specified in \code{src}. The images must be .tif files.
#'  The \code{src} can take the path created by other functions of this package, such as \code{senMosaic},
#'  \code{modMosaic}, \code{senFolderToVar}, etc. The images are imported into R to build a \code{RasterStack} that
#'  is loaded into the global environment. The name of the raster stack will be the one specified
#'  in ts.name. The RasterStack is saved into an RData file in the Approot directory.
#'
#' @param src path to the folder where the time series of images is located
#' @param ts.name the name of the variable containing the time series in R
#' @param startDate the starting date of the time series
#' @param endDate the ending date of the time series
#' @param recursive a flag to read folders recursively
#' @param ... argument to allow function nestering
#' \itemize{
#'   \item \code{AppRoot} the path where the RData will be saved
#' }
#'
#' @examples
#' \dontrun{
#' # load georeferenced polygon of Navarre as example
#' data(ex.navarre)
#' # set the download folder
#' s.start<-Sys.time()
#' src<-"Path_for_downloading_folder"
#' #download the images
#' modDownload(product="MOD09GA",
#'             startDate=as.Date("30-07-2018","%d-%m-%Y"),
#'             endDate=as.Date("06-08-2018","%d-%m-%Y"),
#'             username="username",
#'             password="password",
#'             AppRoot=src,
#'             hdfdir="hdf",
#'             tiffdir="tif",
#'             collection=6,
#'             extent=ex.navarre)
#' #set tif folder where hdf will be imported
#' src<-file.path(src,"MOD09GA")
#' #set the tif folder path
#' tif.src<-file.path(src,"tif")
#' #mosaic and cut navarre region
#' modMosaic(tif.src,
#'           AppRoot=src,
#'           out.name="Navarre",
#'           extent=ex.navarre)
#' #change src to navarre folder
#' src<-file.path(src,"Navarre")
#' #calculate NDVI from navarre folder
#' modFolderToVar(src,
#'                fun=varNDVI,
#'                AppRoot=dirname(src))
#' #change src TS_sample
#' src<-file.path(dirname(src),"NDVI")
#' #create the Rdata
#' genSaveTSRData(src,ts.name="ModisNDVI",AppRoot=dirname(src))
#' s.end<-Sys.time()
#' }
genSaveTSRData<-function(src,ts.name="TS.Name",startDate=NULL,endDate=NULL,recursive=F,...){
  AppRoot=defineAppRoot(...)
  flist<-list.files(src,full.names = T,pattern="\\.tif$",recursive=recursive)
  allDates<-genGetDates(flist)
  if(!is.null(startDate)){
    flist<-flist[allDates>startDate]
    allDates<-allDates[allDates>startDate]
  }
  if(!is.null(startDate)){
    flist<-flist[allDates<endDate]
    allDates<-allDates[allDates<endDate]
  }
  assign(ts.name,readAll(stack(flist)))
  save(list=c(ts.name), file = paste0(AppRoot,"/",ts.name,".RData"))
  eval(parse( text=paste0(ts.name,"<<-",ts.name) ))
  #assign(ts.name,readAll(stack(flist)),envir = globalenv())
  message(paste0("The time series of images in ",src," have been saved as RData.\nYou can find the RDAta in: ",paste0(AppRoot,"/",ts.name,".RData")))
}
