#' Creates clouds layers for sentinel-2 images
#' 
#' \code{senCloudMask} creates clouds layers using \code{CLDPROB} band from \code{S2MSI2A} product.
#'
#' @param src the path to the folder where the \code{S2MSI2A} product are stored. 
#' @param img.res character vector argument. Defines the resolution used to create the cloud mask. ex c("10m", "20m", "30m").
#' @param sensitivity numeric argument. defines how sensitive is the method detecting the clouds. 0
#' @param overwrite logical argument. If \code{TRUE} overwrites the existing images with the same name.
#' @param ... argument to allow function nestering:
#' \itemize{
#'   \item \code{AppRoot} the directory where the extracted images should be located
#' }
#'
#' @examples
#' \dontrun{
#' senCloudMask(src="D:/Downscaling/Sentinel2_L2/Navarre",
#'              img.res="20m",
#'              overwrite=TRUE,
#'              AppRoot="D:/Downscaling/Sentinel2_L2",
#'              sensitivity=98)
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
    out.img<-file.path(AppRoot,paste0(basename(id),"_CloudMask.tif"))
    if(!file.exists(out.img)|overwrite){
      #id<-imgdir.list[1]
      tif.list<-list.files(id,pattern = "\\.tif$",full.names = T)
      cloudmask<-tif.list[grepl(getRGISToolsOpt("SEN2BANDS")["cloud"],tif.list)]
      cloudmask<-cloudmask[grepl(img.res,cloudmask)]
      if(length(cloudmask)==0){
        message(paste0("No cloud mask found for date ",genGetDates(basename(id))))
        next
      }
      message("Creating cloud mask from image ",basename(cloudmask))
      ras.cloud<-raster(cloudmask)
      ras.cloud[is.na(ras.cloud)]<--1
      ras.cloud[ras.cloud>=sensitivity]<-NA
      ras.cloud[!is.na(ras.cloud)]<-1

      writeRaster(ras.cloud,out.img,overwrite=overwrite)
    }else{
      messate(paste0("Cloud mask of date ",genGetDates(basename(id))," already exists."))
    }
  }
}
