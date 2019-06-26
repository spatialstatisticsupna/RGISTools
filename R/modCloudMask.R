#' Creates clouds layers for Modis images
#' 
#' \code{modCloudMask} creates clouds layers derived from \code{MOD35_L2} products. 
#' 
#' This function, downloads and process the \code{MOD35_L2} products to create clouds 
#' mask composed by \code{NA}s and \code{1}. The resulting cloud mask layers need to be
#' reprojected.
#'
#' @param startDate starting date of the image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#' @param endDate ending date of the image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#' @param out.name the name of the region, if is not defined "outfile" will be assigned.
#' @param extent \code{Extent}, \code{Raster*}, \code{SpatialPolygons*}, \code{SpatialLines*} or 
#'   \code{SpatialPoints*} object are acceptable formats as long as coordinates 
#'   are in longitude/latitude format. This argument is mandatory if \code{polygon} 
#'   or \code{lonlat} is not defined.
#' @param out.name 
#' @param raw.rm logical argument. If \code{TRUE} region images are removed.
#' @param verbose logical argument. If \code{TRUE} the function prints running stages and warnings.
#' @param ... argument to allow function nestering:
#' \itemize{
#'   \item \code{AppRoot} the directory where the extracted images should be located
#' }
#'
#' @examples
#' \dontrun{
#' modCloudMask(startDate=as.Date("2017208","%Y%j"),
#'              endDate=as.Date("2017213","%Y%j"),
#'              extent=ex.navarre,
#'              AppRoot="D:/Downscaling/CloudMask2",
#'              out.name="Navarre")
#' cmask<-list.files("D:/Downscaling/CloudMask2",full.names = T,pattern = "\\.tif$")
#' a<-lapply(cmask,raster)
#' navarra.path<-list.files("D:/Downscaling/MOD09GA/Navarre",full.names = T,recursive=T,pattern = "\\.tif$")
#' navarra.path<-navarra.path[grepl("b01_1",navarra.path)]
#' b<-lapply(navarra.path,raster)
#' }
modCloudMask<-function(startDate,endDate,extent,out.name,raw.rm,...){
  # library(RGISTools)
  # getRGISToolsOpt("SEN2BANDS")
  # sensitivity 0-1
  # lsCloudMask(src,overwrite=T)
  #startDate=as.Date("2017208","%Y%j")
  #endDate=as.Date("2017213","%Y%j")
  #extent=ex.navarre
  #AppRoot="D:/Downscaling/CloudMask2"
  #ahour aproximate capture hour
  #gdal_setInstallation(search_path = "C:/Program Files/GDAL", rescan = T, ignore.full_scan = TRUE, verbose = T)
  #out.name="Navarre"
  #est.hour=1100 4 digit integer where first 2 digits represent the hour and the second 2 the minutes
  #' 
  arg <- list(...)
  AppRoot <- defineAppRoot(...)
  modDownloadAtmosphere(startDate=startDate,
                        endDate=endDate,
                        extent=extent,
                        product = "MOD35_L2",
                        bFilter=c("Cloud_Mask"),
                        s_srs=CRS("+init=epsg:4326"),
                        AppRoot=AppRoot
  )
  
  tif.dir<-file.path(AppRoot,"tif")
  file.remove(list.files(tif.dir,recursive = T,pattern = "SPI",full.names = T))
  modMosaic(src=tif.dir,
            extent = extent,
            gutils = T,
            out.name = out.name,
            AppRoot=AppRoot)
  
  tif.images<-list.files(file.path(AppRoot,out.name),recursive = T,full.names = T,pattern = "\\.tif$")
  for(i in tif.images){
    cimg<-stack(i)
    cimg[[5]][!is.na(cimg[[5]])]<-1
    cimg[[6]][!is.na(cimg[[6]])]<-1
    cldmask<-cimg[[5]]*cimg[[6]]
    writeRaster(cldmask,file.path(AppRoot,gsub("__","_",basename(i))))
  }
  unlink(file.path(AppRoot,out.name),recursive=T)
  
  # aa<-stack(tif.images[2])[[3]]
  # spplot(aa)
  # aa[aa<20]<-NA
  # 
  # moddates<-unique(modGetDates(tif.images))
  # for(d in 1:length(moddates)){
  #   dimages<-tif.images[which(modGetDates(tif.images)%in%moddates[d])]
  #   hours.diff<-abs(est.hour-as.numeric(modGetHour(dimages)))
  #   mn<-min(hours.diff)
  #   tif.hour<-tif.images[which(hours.diff==mn)]
  # 
  # }
  #mn<-min(hours.diff)
  #tif.hour<-tif.images[which(hours.diff==mn)]
  #stack.imgs<-lapply(tif.hour,stack)
  #stack.imgs<-lapply(stack.imgs,crop,extent)

  #aa<-stack("D:/Downscaling/CloudMask2/Navarre/2017208/Navarre_2017208__mod35_Cloud_Mask.tif")
  #spplot(aa)
  #procesamiento de imagenes

}