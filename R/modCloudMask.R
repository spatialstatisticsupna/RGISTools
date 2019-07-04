#' Create layers of clouds for Modis images
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
#' @param out.name the name of the region.
#' @param extent \code{Extent}, \code{Raster*}, \code{SpatialPolygons*}, \code{SpatialLines*} or 
#'   \code{SpatialPoints*} object are acceptable formats as long as coordinates 
#'   are in longitude/latitude format. This argument is mandatory if \code{polygon} 
#'   or \code{lonlat} is not defined.
#' @param raw.rm logical argument. If \code{TRUE}, region images are removed.
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing images with the same name.
#' @param verbose logical argument. If \code{TRUE}, the function prints running stages and warnings.
#' @param ... argument for function nestering:
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
#' # Search and download the images from Modis between
#' # 01-01-2018 and 03-01-2018 for the region of Navarre
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
#' # assign src1 as the outut folder for ModMosaic
#' src.tiles <- file.path(src, "MOD09GA")
#' tif.src.tiles <- file.path(src.tiles, "tif")
#' # mosaic the Modis images
#' modMosaic(tif.src.tiles, # the input folder 
#'           AppRoot = src.tiles, # the output folder 
#'           out.name = "Navarre", # creates Navarre folder in AppRoot
#'           gutils = TRUE,
#'           extent = ex.navarre)
#'           
#' # assign src as the path to cloud folder      
#' src.cloud <- file.path(src,"CloudMasks")
#' modCloudMask(startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'              endDate = as.Date("04-01-2018", "%d-%m-%Y"),
#'              extent = ex.navarre,
#'              AppRoot = src.cloud,
#'              out.name = "Navarre")
#'
#' # The cloud mask may have different extent, resolution...  
#' src.cloud.navarre <- file.path(src.cloud,"Navarre")
#' cmask <- list.files(src.cloud, full.names = TRUE, pattern = "\\.tif$")
#' cmask.ras <- lapply(cmask, raster) 
#' 
#' navarre.path <- file.path(src.tiles, "Navarre")
#' navarre.img <- list.files(navarre.path,
#'                           full.names = TRUE,
#'                           recursive = TRUE,
#'                           pattern = "\\.tif$")
#' # select b01
#' navarre.img <- navarre.img[grepl("b01_1",navarre.img)]
#' navarre.b01.stack <- stack(navarre.img)
#' 
#' # reproject cloud mask to navarre.b01.stack projection
#' cmask.stack <- stack(lapply(cmask.ras, projectRaster, navarre.b01.stack))
#' 
#' # plot the cloud free b01 layer
#' spplot(navarre.b01.stack*cmask.stack)
#' }
modCloudMask<-function(startDate,endDate,extent,out.name="outname",raw.rm=FALSE,overwrite=FALSE,verbose=FALSE,...){
  arg <- list(...)
  AppRoot <- defineAppRoot(...)
  modDownloadAtmosphere(startDate=startDate,
                        endDate=endDate,
                        extent=extent,
                        product = "MOD35_L2",
                        bFilter=c("Cloud_Mask"),
                        rm.band=c("SPI"),
                        s_srs=CRS("+init=epsg:4326"),
                        AppRoot=AppRoot,
                        verbose=verbose
  )
  
  tif.dir<-file.path(AppRoot,"tif")
  modMosaic(src=tif.dir,
            extent = extent,
            gutils = T,
            AppRoot=AppRoot,
            verbose=verbose)
  
  tif.images<-list.files(file.path(AppRoot,"outfile"),recursive = T,full.names = T,pattern = "\\.tif$")
  dir.create(file.path(AppRoot,out.name),recursive=T,showWarnings = verbose)
  for(i in tif.images){
    out.file<-file.path(AppRoot,out.name,gsub("__","_",basename(i)))
    if((!file.exists(out.file))||overwrite){
      cimg<-stack(i)
      cimg[[5]][!is.na(cimg[[5]])]<-1
      cimg[[6]][!is.na(cimg[[6]])]<-1
      cldmask<-cimg[[5]]*cimg[[6]]
      writeRaster(cldmask,out.file,overwrite=overwrite)
    }
  }
  if(raw.rm){unlink(file.path(AppRoot,"outfile"),recursive=TRUE)}
  message(paste0("Clouds masks saved in:",AppRoot))
}