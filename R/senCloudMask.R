#' Create cloud masks for Sentinel-2 images
#' 
#' \code{senCloudMask} creates cloud masks derived from the cloud probability
#' band \code{CLDPROB} band from the \code{S2MSI2A} product.
#'
#' The valid threshold range for \code{sensitivity} is 0-100. By default,
#' the argument is set to 50.
#' 
#' @param src the path to the folder with the \code{S2MSI2A} images. 
#' @param img.res a \code{character} vector argument. Defines the band resolution
#' used to create the cloud mask. Ex "20m" or "30m".
#' @param sensitivity a \code{numeric} argument. Defines the sensitivity of the
#' cloud detection method.
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing
#' images with the same name.
#' @param ... argument for nested functions:
#' \itemize{
#'   \item \code{AppRoot} the directory where the cloud masks are saved.
#' }
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # Download S2MSI1C products sensed by Sentinel-2 
#' # between the julian days 210 and 218, 2018
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
#' # define the paths to the Sentinle-2 images and the
#' # folder with the unzipped images
#' src.sen2 <- file.path(src, "Sentinel-2")
#' src.unzip <- file.path(src.sen2, "unzip")
#' # mosaic the Sentinel-2 images
#' senMosaic(src.unzip,
#'           AppRoot = src.sen2,
#'           gutils = TRUE,
#'           out.name = "Navarre")
#'           
#' # calculate the cloud mask
#' src.navarre <- file.path(src.sen2, "Navarre")
#' senCloudMask(src = src.navarre,
#'              img.res = "60m",
#'              overwrite = TRUE,
#'              sensitivity = 98,
#'              AppRoot = src.sen2)
#'              
#' # define the path for the Sentinel-2 cloud mask
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
#' # generate a 60-meter resolution cloud mask 
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
  imgdir.list<-list.dirs(src,recursive=FALSE)
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
