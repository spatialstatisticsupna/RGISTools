#' Creates clouds layers for Landsat images
#' 
#' \code{lsCloudMask} creates clouds layers derived from \code{BQA} band from Landsat-7 or Landsat-8 captures. 
#' The function run over untar images downloaded by \code{\link{lsDownSearch}} or \code{\link{lsDownload}}, and 
#' creates a new band designated with the label \code{CLD}.
#'
#' @param src the path to the folder where the Landsat multispectral captures are stored. 
#' @param sensitivity numeric argument. Defines how sensitive is the method detecting the clouds. 0-8000 are
#' valid values.
#' @param overwrite logical argument. If \code{TRUE} overwrites the existing images with the same name.
#' @param verbose logical argument. If \code{TRUE} the function prints running stages and warnings.
#' @param ... argument to allow function nestering:
#' \itemize{
#'   \item \code{AppRoot} the directory where the extracted images should be located
#' }
#'
#' @examples
#' \dontrun{
#' data(ex.navarre)
#' src <- "Path_for_downloading_folder"
#' 
#' lsDownload(satellite = "ls8",
#'            username = "rgistools",
#'            password = "EspacialUPNA88",
#'            startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'            endDate = as.Date("20-01-2018", "%d-%m-%Y"),
#'            extent = ex.navarre,
#'            untar = TRUE,
#'            AppRoot = src)
#' src.untar <- file.path(src,"untar")
#' lsCloudMask(src=src.untar,
#'             overwrite=TRUE,
#'             sensitivity=98)
#'             
#' tiles.path <- list.files(src.untar,
#'                          full.names = TRUE,
#'                          recursive = TRUE,
#'                          pattern = "\\.tif$")
#' cloud.tiles <- tiles.path[grepl("CLD",tiles.path)]
#' cloud.tiles.ras <- stack(cloud.tiles)
#' spplot(cloud.tiles.ras)
#' }
lsCloudMask<-function(src,sensitivity=2800,overwrite=FALSE,verbose=F,...){
  arg<-list(...)
  AppRoot<-defineAppRoot(...)
  imgdir.list<-list.dirs(src)[-1]

  for(id in imgdir.list){
    #id<-imgdir.list[2]
    tif.list<-list.files(id,pattern = "\\.tif$",full.names = T,ignore.case = T)
    qc<-getRGISToolsOpt("LS8BANDS")["quality"]
    qcmask<-tif.list[grepl(qc,tif.list)]
    if(length(qcmask)==0){
      message(paste0("No cloud mask found for date ",genGetDates(basename(id))))
      next
    }
    out.img<-gsub(paste0(qc,".tif"),"CLD.tif",qcmask,ignore.case = T)
    
    if(!file.exists(out.img)|overwrite){
      message("Creating cloud mask for tile ",dirname(qcmask))
      
      ras.cloud<-readAll(raster(qcmask))
      mn<-minValue(ras.cloud)
      if(verbose){
        message(paste0("Minimun: ",mn))
      }
      ras.cloud[ras.cloud<=mn]<-NA
      ras.cloud[ras.cloud<sensitivity]<-NA
      ras.cloud[ras.cloud>=sensitivity]<-1
      
      print(spplot(ras.cloud))
      writeRaster(ras.cloud,out.img,overwrite=overwrite)
      rm(ras.cloud)
    }else{
      message(paste0("Cloud mask of date ",genGetDates(basename(id))," already exists."))
    }
  }
  message(paste0("Cloud mask in ",src," for all the tiles."))
}
