#' Saves a time series of images as an RData
#'
#' \code{genSaveTSRData} imports a time series of images from a folder (GTiff
#'  format), builds a \code{RasterStack} and saves it in an RData.
#'
#' The function reads all the images inside the folder specified in \code{src}.
#' Images files must be GTiffs. The \code{src} can take the path created by
#' other functions of this package, such as \code{\link{senMosaic}},
#' \code{\link{modMosaic}}, \code{\link{senFolderToVar}}, etc. The images are
#' imported into `R' to build a \code{RasterStack}. The
#' name of the \code{RasterStack} is specified in \code{ts.name}. The
#' \code{RasterStack} is saved in an RData file in the \code{AppRoot} directory.
#'
#' @param src path to the folder where the time series of images is located.
#' @param ts.name the name of the \code{RasterStack} in the RData.
#' @param startDate a \code{Date} class object with the starting date of the
#' study period.
#' @param endDate a \code{Date} class object with the ending date of the study
#' period.
#' @param dextent a logical argument. If \code{TRUE}, the function expands the
#' extent of the \code{RasterStack} to embrace the extents of all GTiff images.
#' @param recursive logical argument. If \code{TRUE}, reads folders recursively,
#' searching for GTiff images.
#' @param AppRoot the path where the RData is saved.
#'
#' @return if \code{AppRoot} is not asigned a \code{RasterStack} with the time series in src folder.
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # set the download folder
#' s.start <- Sys.time()
#' wdir <- paste0(tempdir(),"/Path_for_downloading_folder")
#' print(wdir)
#' # download the images
#' modDownSearch(product = "MOD09GA",
#'             startDate = as.Date("30-07-2018", "%d-%m-%Y"),
#'             endDate = as.Date("06-08-2018", "%d-%m-%Y"),
#'             username = "username",
#'             password = "password",
#'             AppRoot = wdir,
#'             extract.tif = TRUE,
#'             collection = 6,
#'             extent = ex.navarre)
#' # set folder path where MOD09GA images will be saved
#' wdir.mod <- file.path(wdir,"Modis","MOD09GA")
#' # set the tif folder path
#' wdir.mod.tif <- file.path(wdir.mod,"tif")
#' # mosaic and cut navarre region
#' modMosaic(wdir.mod.tif,
#'           AppRoot = wdir.mod,
#'           out.name = "Navarre",
#'           extent = ex.navarre)
#' # change src to navarre folder
#' wdir.mod.navarre <- file.path(wdir.mod,"Navarre")
#' # calculate NDVI from navarre folder
#' modFolderToVar(wdir.mod.navarre,
#'                fun = varNDVI,
#'                AppRoot = dirname(wdir.mod.navarre),
#'                overwrite = TRUE)
#' # change src TS_sample
#' wdir.mod.ndvi <- file.path(dirname(wdir.mod.navarre),"NDVI")
#' # create the Rdata
#' tiles.mod.ndvi<-genSaveTSRData(wdir.mod.ndvi, ts.name = "ModisNDVI", AppRoot = wdir.mod)
#' # remove values out of 0-1 range
#' tiles.mod.ndvi.lim <- clamp(tiles.mod.ndvi,lower=0,upper=1)
#' # plot the ndvi images
#' spplot(tiles.mod.ndvi.lim)
#' s.end <- Sys.time()
#' s.end - s.start
#' }
genSaveTSRData<-function(src,AppRoot=NULL,ts.name="TS.Name",startDate=NULL,endDate=NULL,dextent=FALSE,recursive=FALSE){
  src<-pathWinLx(src)
  if(!is.null(AppRoot)){AppRoot<-pathWinLx(AppRoot)}
  
  flist<-list.files(src,full.names = TRUE,pattern="\\.tif$",recursive=recursive)
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
 
  
  if(!is.null(AppRoot)){
    save(list=c(ts.name), file = paste0(AppRoot,"/",ts.name,".RData"))
    message(paste0("The time series of images in ",src," have been saved as RData.\nYou can find the RDAta in: ",paste0(AppRoot,"/",ts.name,".RData")))
  }else{
    return(rstack)
  }
}
