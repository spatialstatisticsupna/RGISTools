#' Creates clouds layers for Modis images
#' 
#' \code{modCloudMask} creates layers of clouds derived from \code{MOD35_L2} products. 
#' 
#' This function, downloads and processes the \code{MOD35_L2} products to create clouds 
#' mask composed by \code{NA}s and \code{1}. The resulting cloud mask layers need to be
#' reprojected because resolution and projection differences with other modis products.
#' This function requires \code{gdalUtils} properly installed.
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
#' modDownload(product = "MOD09GA",
#'             startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'             endDate = as.Date("03-01-2018", "%d-%m-%Y"),
#'             username = "username",
#'             password = "password",
#'             AppRoot = src,
#'             extract.tif = TRUE,
#'             collection = 6,
#'             extent = ex.navarre)
#'             
#' src.tiles <- file.path(src, "MOD09GA")
#' tif.src.tiles <- file.path(src.tiles, "tif")
#' # mosaic the Modis images
#' modMosaic(tif.src.tiles, # the input folder 
#'           AppRoot = src.tiles, # the output folder 
#'           out.name = "Navarre", # creates Navarre folder in AppRoot
#'           gutils = TRUE,
#'           extent = ex.navarre)
#'             
#' src.cloud <- file.path(src,"CloudMask")
#' modCloudMask(startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'              endDate = as.Date("04-01-2018", "%d-%m-%Y"),
#'              extent = ex.navarre,
#'              AppRoot = src.cloud,
#'              out.name = "Navarre")
#'
#' # The cloud mask may have different extent, resolution...            
#' cmask <- list.files(src.cloud, full.names = TRUE, pattern = "\\.tif$")
#' cmask.ras <- lapply(cmask, raster) 
#' 
#' navarre.path <- file.path(src.tiles, "Navarre")
#' navarre.img <- list.files(navarre.path,
#'                           full.names = TRUE,
#'                           recursive = TRUE,
#'                           pattern = "\\.tif$")
#' # select b01
#' navarre.img<-navarre.img[grepl("b01_1",navarre.img)]
#' navarre.b01.stack<-stack(navarre.img)
#' 
#' # reproject cloud mask to navarre.b01.stack projection
#' cmask.stack <- stack(lapply(cmask.ras, projectRaster, navarre.b01.stack))
#' 
#' spplot(navarre.b01.stack*cmask.stack)
#' }
modCloudMask<-function(startDate,endDate,extent,out.name,raw.rm,overwrite=FALSE,...){
  arg <- list(...)
  AppRoot <- defineAppRoot(...)
  modDownloadAtmosphere(startDate=startDate,
                        endDate=endDate,
                        extent=extent,
                        product = "MOD35_L2",
                        bFilter=c("Cloud_Mask"),
                        rm.band=c("SPI"),
                        s_srs=CRS("+init=epsg:4326"),
                        AppRoot=AppRoot
  )
  
  tif.dir<-file.path(AppRoot,"tif")
  modMosaic(src=tif.dir,
            extent = extent,
            gutils = T,
            out.name = out.name,
            AppRoot=AppRoot)
  
  tif.images<-list.files(file.path(AppRoot,out.name),recursive = T,full.names = T,pattern = "\\.tif$")
  for(i in tif.images){
    out.file<-file.path(AppRoot,gsub("__","_",basename(i)))
    if((!file.exists(out.file))||overwrite){
      cimg<-stack(i)
      cimg[[5]][!is.na(cimg[[5]])]<-1
      cimg[[6]][!is.na(cimg[[6]])]<-1
      cldmask<-cimg[[5]]*cimg[[6]]
      writeRaster(cldmask,out.file,overwrite=overwrite)
    }
  }
  unlink(file.path(AppRoot,out.name),recursive=TRUE)
  message(paste0("Clouds masks saved in:",AppRoot))
}