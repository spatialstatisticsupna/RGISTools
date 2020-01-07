#' Create cloud masks for Sentinel-2 images
#' 
#' \code{senCloudMask} creates cloud masks derived from the cloud probability
#' band (\code{CLDPRB}) band from the "\code{S2MSI2A}" product.
#'
#' The valid threshold range for \code{sensitivity} is 0-100. By default,
#' the argument is set to 50.
#' 
#' @param src the path to the folder with the "\code{S2MSI2A}" images. 
#' @param AppRoot the directory where the cloud masks are saved.
#' @param out.name the name of the folder that stores the outputs. 
#' If the arguemnt is not defined the folder will be named as "CloudMask".
#' @param resbands a \code{character} vector argument. Defines the band resolution
#' used to create the cloud mask. Ex "20m" or "60m".
#' @param sensitivity a \code{numeric} argument. Defines the sensitivity of the
#' cloud detection method.
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing
#' images with the same name.
#' @param ... arguments for nested functions.
#'  \itemize{
#'   \item \code{dates} a vector with the capturing dates being considered
#'   for mosaicking. If not supplied, all dates are mosaicked.
#' }
#' @return this function does not return anything. It saves the cloud masks (CLD)
#' as GTiff files in the \code{AppRoot} directory.
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # Download S2MSI1C products sensed by Sentinel-2 
#' # between the julian days 210 and 218, 2018
#' wdir <- file.path(tempdir(),"Path_for_downloading_folder")
#' print(wdir)
#' senDownSearch(startDate = as.Date("2018210", "%Y%j"),
#'               endDate = as.Date("2018218", "%Y%j"),
#'               platform = "Sentinel-2",
#'               extent = ex.navarre,
#'               product = "S2MSI2A",
#'               pathrow = c("R094"),
#'               username = "username",
#'               password = "password",
#'               AppRoot = wdir)
#' # define the paths to the Sentinle-2 images and the
#' # folder with the unzipped images
#' wdir.sen <- file.path(wdir, "Sentinel-2")
#' wdir.sen.unzip <- file.path(wdir.sen, "unzip")
#' # mosaic the Sentinel-2 images
#' senMosaic(wdir.sen.unzip,
#'           AppRoot = wdir.sen,
#'           gutils = TRUE,
#'           out.name = "Navarre")
#'           
#' # calculate the cloud mask
#' wdir.sen.navarre <- file.path(wdir.sen, "Navarre")
#' senCloudMask(src = wdir.sen.navarre,
#'              resbands = "60m",
#'              overwrite = TRUE,
#'              sensitivity = 98,
#'              AppRoot = wdir.sen)
#'              
#' # define the path for the Sentinel-2 cloud mask
#' wdir.sen.cloud <- file.path(wdir.sen, "CloudMask")
#' 
#' # select B02 images of 60 meters
#' tiles.sen.navarre <- list.files(wdir.sen.navarre,
#'                             full.names = TRUE,
#'                             recursive = TRUE,
#'                            pattern = "\\.tif$")
#' tiles.sen.navarre.b2 <- tiles.sen.navarre[grepl("B02",tiles.sen.navarre)]
#' tiles.sen.navarre.b2 <- tiles.sen.navarre.b2[grepl("60m",tiles.sen.navarre.b2)]
#' 
#' # generate a 60-meter resolution cloud mask 
#' tiles.sen.cloud <- list.files(wdir.sen.cloud,
#'                               full.names = TRUE,
#'                               pattern = "\\.tif$")
#' tiles.sen.cloud.60 <- tiles.sen.cloud[grepl("60m",tiles.sen.cloud)]
#' 
#' # remove the cloud mask from b02 tiles
#' img.sen.navarre.b2 <- stack(tiles.sen.navarre.b2)
#' img.sen.cloud.60 <- stack(tiles.sen.cloud.60)
#' img.sen.navarre.b2.cloud.free <- img.sen.navarre.b2*img.sen.cloud.60
#' # plot b2 cloud free layers
#' spplot(img.sen.navarre.b2.cloud.free)
#' }
senCloudMask<-function(src,AppRoot,out.name,resbands,sensitivity=50,overwrite=FALSE,...){
  # src<-"D:/Downscaling/Sentinel2_L2/Navarre"/2017209
  # AppRoot<-"D:/Downscaling/Sentinel2_L2"
  # library(RGISTools)
  # getRGISToolsOpt("SEN2BANDS")
  # sensitivity 0-100
  # img.res 20m o 30m img.res<-"20m"
  
  arg<-list(...)
  #filter dates
  if("dates"%in%names(arg)){
    dates<-dates[dates%in%arg$dates]
  }
  
  src<-pathWinLx(src)
  if(missing(AppRoot))stop("AppRoot needed for defining output file.")
  AppRoot<-pathWinLx(AppRoot)
  imgdir.list<-list.dirs(src,recursive=FALSE)
  if("dates"%in%names(arg)){imgdir.list<-imgdir.list[genGetDates(imgdir.list)%in%arg$dates]}
  if(missing(out.name))
    AppRoot<-file.path(AppRoot,"CloudMask")
  else
    AppRoot<-file.path(AppRoot,out.name)
  
  dir.create(AppRoot,showWarnings = FALSE,recursive = TRUE)
  if(missing(resbands)){
    resbands<-c("20m","60m")
  }
  for(resband in resbands){
    for(id in imgdir.list){
      out.img<-file.path(AppRoot,paste0(basename(id),"_",resband,"_CloudMask.tif"))
      if(!file.exists(out.img)|overwrite){
        #id<-imgdir.list[1]
        tif.list<-list.files(id,pattern = "\\.tif$",full.names = TRUE)
        cloudmask<-tif.list[grepl(getRGISToolsOpt("SEN2BANDS")["cloud"],tif.list)]
        cloudmask<-cloudmask[grepl(resband,cloudmask)]
        if(length(cloudmask)==0){
          message(paste0("No cloud mask of ",resband," found for date ",genGetDates(basename(id))))
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
  
}
