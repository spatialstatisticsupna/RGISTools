#' Imports time series of images and saves as RData
#'
#' \code{genSaveTSRData} imports the time series of (tif) satellite images into R from a folder, creating an RData.
#'
#' The function reads all the images inside the folder specified in \code{src}. The images must be .tif files.
#'  The \code{src} can take the path created by other functions of this package, such as \code{\link{senMosaic}},
#'  \code{\link{modMosaic}}, \code{\link{senFolderToVar}}, etc. The images are imported into R to build a \code{RasterStack} that
#'  is loaded into the global environment. The name of the \code{RasterStack} is one specified
#'  in \code{ts.name}. The \code{RasterStack} is saved into an RData file in the \code{AppRoot} directory.
#'
#' @param src path to the folder where the time series of images is located.
#' @param ts.name the name of the variable containing the time series in R.
#' @param startDate starting date of image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#' @param endDate ending date of image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#' @param dextent creates the \code{RasterStack} from different extent tif images.
#' @param recursive logical argument. If \code{TRUE} reads folders recursively.
#' @param ... argument to allow function nestering.
#' \itemize{
#'   \item \code{AppRoot} the path where the RData will be saved.
#' }
#'
#' @examples
#' \dontrun{
#' # load georeferenced polygon of Navarre as example
#' data(ex.navarre)
#' # set the download folder
#' s.start<-Sys.time()
#' src<-"Path_for_downloading_folder"
#' # download the images
#' modDownload(product = "MOD09GA",
#'             startDate = as.Date("30-07-2018","%d-%m-%Y"),
#'             endDate = as.Date("06-08-2018","%d-%m-%Y"),
#'             username = "username",
#'             password = "password",
#'             AppRoot = src,
#'             hdfdir = "hdf",
#'             tiffdir = "tif",
#'             collection = 6,
#'             extent = ex.navarre)
#' # set tif folder where hdf will be imported
#' src1<-file.path(src,"MOD09GA")
#' # set the tif folder path
#' tif.src<-file.path(src1,"tif")
#' #mosaic and cut navarre region
#' modMosaic(tif.src,
#'           AppRoot = src1,
#'           out.name = "Navarre",
#'           extent = ex.navarre)
#' # change src to navarre folder
#' src2<-file.path(src1,"Navarre")
#' # calculate NDVI from navarre folder
#' modFolderToVar(src2,
#'                fun = varNDVI,
#'                AppRoot = dirname(src2),
#'                overwrite = TRUE)
#' # change src TS_sample
#' src3<-file.path(dirname(src2),"NDVI")
#' # create the Rdata
#' genSaveTSRData(src3, ts.name = "ModisNDVI", AppRoot = src)
#' s.end<-Sys.time()
#' }
genSaveTSRData<-function(src,ts.name="TS.Name",startDate=NULL,endDate=NULL,dextent=FALSE,recursive=FALSE,...){
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
  
  if(dextent){
    imgs<-lapply(flist,raster)
    rstack<-NULL
    for(result in imgs){
      if(is.null(rstack)){
        rstack<-result
      }else{
        result<-extend(result,rstack)
        rstack<-extend(rstack,result)
        rstack<-addLayer(rstack,result)
      }
    } 
    assign(ts.name,readAll(rstack))
  }else{
    assign(ts.name,readAll(stack(flist)))
  }
 
  
  
  save(list=c(ts.name), file = paste0(AppRoot,"/",ts.name,".RData"))
  eval(parse(text=paste0(ts.name,"<<-",ts.name) ))
  message(paste0("The time series of images in ",src," have been saved as RData.\nYou can find the RDAta in: ",paste0(AppRoot,"/",ts.name,".RData")))
}
