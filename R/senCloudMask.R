#' Creates clouds layers for sentinel-2 images
#' 
#' \code{senCloudMask} creates clouds layers using \code{CLDPROB} band from \code{S2MSI2A} product.
#'
#' @param src the path to the folder where the \code{S2MSI2A} images are stored. 
#' @param img.res character vector argument. Defines the resolution used to create the cloud mask. Ex "20m" or "30m".
#' @param sensitivity numeric argument. defines how sensitive is the method detecting the clouds. The
#' valid range is 0-100. By default 50.
#' @param overwrite logical argument. If \code{TRUE} overwrites the existing images with the same name.
#' @param ... argument to allow function nestering:
#' \itemize{
#'   \item \code{AppRoot} the directory where the extracted images should be located.
#' }
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # Download S2MSI1C products sensed by Sentinel - 2 
#' # satellite between the julian dates 210 and 218, 2018
#' src <- "Path_for_downloading_folder"
#' senDownload(startDate = as.Date("2018210", "%Y%j"),
#'             endDate = as.Date("2018218", "%Y%j"),
#'             platform = "Sentinel-2",
#'             extent = ex.navarre,
#'             product = "S2MSI2A",
#'             pathrow = c("R094"),
#'             username = "username",
#'             password = "password",
#'             AppRoot = src)
#' # define Sentinel-2 path to the unzip folder
#' src.sen2 <- file.path(src, "Sentinel-2")
#' src.unzip <- file.path(src.sen2, "unzip")
#' # mosaic the Sentinel-2 images
#' senMosaic(src.unzip,
#'           AppRoot = src,
#'           gutils = TRUE,
#'           out.name = "Navarre")
#'           
#' # calculate the cloud mask
#' src.navarre <- file.path(src.sen2, "Navarre")
#' senCloudMask(src = src.navarre,
#'              img.res = "60m",
#'              overwrite = TRUE,
#'              sensitivity=98)
#'              
#' # define Sentinel-2 cloud mask path
#' src.cloud <- file.path(src.sen2, "CloudMask")
#' 
#' # select B02 images of 60 meters
#' tiles.navarre <- list.files(src.navarre,
#'                             full.names = TRUE,
#'                             recursive = TRUE,
#'                            pattern = "\\.tif$")
#' b2.tiles <- tiles.navarre[grepl("B02",tiles.navarre)]
#' b2.tiles <- b2.tiles[grepl("60m",b2.tiles)]
#' 
#' # select cloud mask of 60 meters
#' cloud.tiles <- list.files(src.cloud,
#'                           full.names = TRUE,
#'                           pattern = "\\.tif$")
#' cloud.tiles <- cloud.tiles[grepl("60m",cloud.tiles)]
#' 
#' # remove the cloud mask from b02 tiles
#' b2.tiles.stack <- stack(b2.tiles)
#' cloud.tiles.stack <- stack(cloud.tiles)
#' b2.cloud.free <- b2.tiles.stack*cloud.tiles.stack
#' # plot b2 cloud free layers
#' spplot(b2.cloud.free)
#' }
senCloudMask<-function(src,img.res,sensitivity=50,overwrite=FALSE,...){
  # src<-"D:/Downscaling/Sentinel2_L2/Navarre"/2017209
  # AppRoot<-"D:/Downscaling/Sentinel2_L2"
  # library(RGISTools)
  # getRGISToolsOpt("SEN2BANDS")
  # sensitivity 0-100
  # img.res 10m, 20m o 30m img.res<-"20m"
  # 
  arg<-list(...)
  AppRoot<-defineAppRoot(...)
  imgdir.list<-list.dirs(src)[-1]
  AppRoot<-file.path(AppRoot,"CloudMask")
  dir.create(AppRoot,showWarnings = F,recursive = T)
  for(id in imgdir.list){
    out.img<-file.path(AppRoot,paste0(basename(id),"_",img.res,"_CloudMask.tif"))
    if(!file.exists(out.img)|overwrite){
      #id<-imgdir.list[1]
      tif.list<-list.files(id,pattern = "\\.tif$",full.names = T)
      cloudmask<-tif.list[grepl(getRGISToolsOpt("SEN2BANDS")["cloud"],tif.list)]
      cloudmask<-cloudmask[grepl(img.res,cloudmask)]
      if(length(cloudmask)==0){
        message(paste0("No cloud mask of ",img.res," found for date ",genGetDates(basename(id))))
        next
      }
      message("Creating cloud mask from image ",basename(cloudmask))
      ras.cloud<-raster(cloudmask)
      ras.cloud[is.na(ras.cloud)]<--1
      ras.cloud[ras.cloud>=sensitivity]<-NA
      ras.cloud[!is.na(ras.cloud)]<-1

      writeRaster(ras.cloud,out.img,overwrite=overwrite)
    }else{
      message(paste0("Cloud mask of date ",genGetDates(basename(id))," already exists."))
    }
  }
}
