#' Create clouds layers for Landsat images
#' 
#' \code{lsCloudMask} creates layers of clouds derived from \code{BQA} band from Landsat-7 or Landsat-8 captures. 
#' The function run over untar images downloaded by \code{\link{lsDownSearch}} or \code{\link{lsDownload}}, and 
#' creates a new band named \code{CLD}.
#'
#' @param src the path to the folder where the Landsat multispectral captures are stored. 
#' @param sensitivity numeric argument. Defines how sensitive is the method detecting the clouds. 0-8000 are
#' valid values. By default, the best value for Landsat 8 images assigned 2800. For Landsat-7 images use 600.
#' @param overwrite logical argument. If \code{TRUE} overwrites the existing images with the same name.
#' @param verbose logical argument. If \code{TRUE} the function prints running stages and warnings.
#' @param ... argument to allow function nestering:
#' \itemize{
#'   \item \code{AppRoot} the directory where the extracted images should be located
#' }
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' src <- "Path_for_downloading_folder"
#' 
#' # Search and download the images from Landsat-8 comprised between
#' # 01-01-2018 and 20-01-2018 for the region of Navarre
#' lsDownload(satellite = "ls8",
#'            username = "username",
#'            password = "password",
#'            startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'            endDate = as.Date("20-01-2018", "%d-%m-%Y"),
#'            extent = ex.navarre,
#'            untar = TRUE,
#'            AppRoot = src)
#'            
#' # generate the path where the GTiff images are located
#' src.untar <- file.path(src,"Landsat8","untar")
#' # calculate the cloud mask from QC layer
#' lsCloudMask(src=src.untar,
#'             overwrite=TRUE)
#'             
#' # generate the path where the mosacked images will be saved
#' src.ls8 <- dirname(src.untar)
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
#' # load the B1 layer and calculated CLD layer
#' tiles.path <- list.files(src.navarre,
#'                          full.names = TRUE,
#'                          recursive = TRUE,
#'                          pattern = "\\.tif$")
#' cloud.tiles <- tiles.path[grepl("CLD",tiles.path)]
#' b1.tiles <- tiles.path[grepl("B1.tif",tiles.path)]
#' cloud.tiles.ras <- stack(cloud.tiles)
#' b1.tiles.ras <- stack(b1.tiles)
#' 
#' # calculate cloud free b1 layers
#' b1.cloud.free <- b1.tiles.ras * cloud.tiles.ras
#' spplot(b1.cloud.free)
#' }
lsCloudMask<-function(src,sensitivity=2800,overwrite=FALSE,verbose=F,...){
  arg<-list(...)
  AppRoot<-defineAppRoot(...)
  imgdir.list<-list.dirs(src,recursive=FALSE)[-1]
  if(verbose){message(paste0("Identifies folders:  \n",imgdir.list))}
  for(id in imgdir.list){
    #id<-imgdir.list[2]
    tif.list<-list.files(id,pattern = "\\.tif$",full.names = T,ignore.case = T)
    qc<-getRGISToolsOpt("LS8BANDS")["quality"]
    qcmask<-tif.list[grepl(qc,tif.list,ignore.case = T)]
    if(verbose){message(paste0("QC mask name:  \n",qcmask))}
    if(length(qcmask)==0){
      message(paste0("No cloud mask found for date ",genGetDates(basename(id))))
      next
    }
    
    out.img<-gsub(paste0(qc,".TIF"),"CLD.TIF",qcmask,ignore.case = T)
    
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
      rm(ras.cloud)
    }else{
      message(paste0("Cloud mask of date ",genGetDates(basename(id))," already exists."))
    }
  }
  message(paste0("Cloud mask in ",src," for all the tiles."))
}
