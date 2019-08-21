#' Create cloud masks for Landsat images
#' 
#' \code{lsCloudMask} creates a cloud mask derived from the band for quality
#' assurance (BQA) from Landsat-7 or Landsat-8 time series. The function is
#' applied to untarred images, such as those resulting from 
#' \code{\link{lsDownSearch}} or \code{\link{lsDownload}}. The result is a new
#' image band, called CLD, that is saved as separate GTiffs.
#' 
#' The valid range for the \code{sensitivity} threshold is 0-8000. By defualt,
#' the argument is set to 2800. We recommend 600 and 2800 for Landsat-7 and
#' Landsat-8 respectively.
#'
#' @param src the path to the folder with the untarred images from Landsat-7 or
#' Landsat-8.
#' @param AppRoot the directory where cloud masks are saved.
#' @param sensitivity \code{numeric} argument. Defines the sensitivity of the
#' cloud detection method.
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
#' src <- paste0(tempdir(),"/Path_for_downloading_folder")
#' print(src)
#' 
#' # search and download images from Landsat-8 between
#' # 01-01-2018 and 20-01-2018 for the region of Navarre
#' lsDownload(satellite = "ls8",
#'            username = "username",
#'            password = "password",
#'            startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'            endDate = as.Date("20-01-2018", "%d-%m-%Y"),
#'            pathrow = list(c(200, 31), c(200, 30)),
#'            untar = TRUE,
#'            AppRoot = src)
#'            
#' # define the path where the GTiff images are located
#' src.ls8 <- file.path(src,"Landsat8")
#' src.untar <- file.path(src.ls8,"untar")
#' # calculate the cloud mask from QC layer
#' lsCloudMask(src=src.untar,
#'             overwrite=TRUE)
#'             
#' # mosaic and crop the imagery
#' lsMosaic(src = src.untar,
#'          AppRoot = src.ls8,
#'          out.name = "Navarre",
#'          extent = ex.navarre,
#'          bandFilter = c("CLD"),
#'          gutils = TRUE, # using gdalUtils
#'          overwrite = TRUE) # overwrite
#'          
#' # generate the path where mosaicked images are located
#' src.navarre <- file.path(src,"Landsat8","Navarre")
#' # load the B1 layer and calculate the CLD layer
#' tiles.path <- list.files(src.navarre,
#'                          full.names = TRUE,
#'                          recursive = TRUE,
#'                          pattern = "\\.tif$")
#' cloud.tiles <- tiles.path[grepl("CLD",tiles.path)]
#' b1.tiles <- tiles.path[grepl("B1.tif",tiles.path)]
#' cloud.tiles.ras <- lapply(cloud.tiles,raster)
#' b1.tiles.ras <- lapply(b1.tiles,raster)
#' 
#' # calculate cloud free b1 layers
#' b1.cloud.free <- b1.tiles.ras[[1]] * cloud.tiles.ras[[1]]
#' spplot(b1.cloud.free)
#' }
lsCloudMask<-function(src,AppRoot,sensitivity=2800,overwrite=FALSE,verbose=FALSE,...){
  arg<-list(...)
  src<-pathWinLx(src)
  AppRoot<-pathWinLx(AppRoot)
  imgdir.list<-list.dirs(src,recursive=FALSE)
  
  if(verbose){message(paste0("Identifies folders:  \n",imgdir.list))}
  for(id in imgdir.list){
    #id<-imgdir.list[2]
    tif.list<-list.files(id,pattern = "\\.tif$",full.names = TRUE,ignore.case = TRUE)
    qc<-getRGISToolsOpt("LS8BANDS")["quality"]
    qcmask<-tif.list[grepl(qc,tif.list,ignore.case = TRUE)]
    if(verbose){message(paste0("QC mask name:  \n",qcmask))}
    if(length(qcmask)==0){
      message(paste0("No cloud mask found for date ",genGetDates(basename(id))))
      next
    }

    out.img<-gsub(paste0(qc,".TIF"),"CLD.TIF",qcmask,ignore.case =TRUE)

    if(!file.exists(out.img)||overwrite){
      message("Creating cloud mask for tile ",dirname(qcmask))
      
      ras.cloud<-readAll(raster(qcmask))
      mn<-minValue(ras.cloud)
      if(verbose){
        message(paste0("Minimun: ",mn))
      }
      ras.cloud[ras.cloud<=mn]<-0
      ras.cloud[ras.cloud<1]<-0
      ras.cloud[ras.cloud>=sensitivity]<-0
      ras.cloud[ras.cloud!=0]<-1
      
      NAvalue(ras.cloud)<-0
      writeRaster(ras.cloud,out.img,overwrite=overwrite)
      if(grepl(".TIF",out.img)){
        file.rename(gsub(".tif",".TIF",out.img),out.img)
      }
      rm(ras.cloud)
    }else{
      message(paste0("Cloud mask of date ",genGetDates(basename(id))," already exists."))
    }
  }
  message(paste0("Cloud mask in ",src," for all the tiles."))
}
