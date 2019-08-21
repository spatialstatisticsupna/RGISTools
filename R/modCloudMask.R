#' Create cloud masks for MODIS images
#' 
#' \code{modCloudMask} creates cloud masks derived from the "\code{MOD35_L2}" 
#' product.
#' 
#' This function, downloads and processes the "\code{MOD35_L2}" products to create
#' cloud masks composed of \code{NA}'s (cloud) and \code{1}'s (clear). The
#' resulting cloud mask layers need to be reprojected because resolution and
#' projection differences with other MODIS products. This function requires
#' `GDAL' and the `\code{gdalUtils}' library properly installed.
#'
#' @param startDate a \code{Date} class object with the starting date of the 
#' study period.
#' @param endDate a \code{Date} class object with the ending date of the 
#' study period.
#' @param AppRoot the directory where cloud masks are saved.
#' @param out.name he name of the folder that stores the outputs. By default,
#' “outfile” is assigned.
#' @param extent An \code{extent}, \code{Raster*}, or \code{Spatial*} object
#' representing the region of interest with longitude/latitude coordinates.
#' @param raw.rm logical argument. If \code{TRUE}, raw images are removed.
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
#' # search and download images from MODIS between
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
#' # assign src1 as the output folder for modMosaic
#' src.mod <- file.path(src, "Modis")
#' src.tiles <- file.path(src.mod, "MOD09GA")
#' tif.src.tiles <- file.path(src.tiles, "tif")
#' # mosaic the MODIS images
#' modMosaic(tif.src.tiles, # the input folder 
#'           AppRoot = src.tiles, # the output folder 
#'           out.name = "Navarre", # creates Navarre folder in AppRoot
#'           gutils = TRUE,
#'           extent = ex.navarre)
#'           
#' # generate the cloud masks      
#' modCloudMask(startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'              endDate = as.Date("04-01-2018", "%d-%m-%Y"),
#'              extent = ex.navarre,
#'              AppRoot = src,
#'              out.name = "Navarre")
#'              
#' src.cloud <- file.path(src.mod,"CloudMask")
#' # the cloud mask may have different extent, resolution...  
#' src.cloud.navarre <- file.path(src.cloud,"Navarre")
#' cmask <- list.files(src.cloud.navarre, full.names = TRUE, pattern = "\\.tif$")
#' cmask.ras <- lapply(cmask, raster)
#' 
#' navarre.path <- file.path(src.tiles, "Navarre")
#' navarre.img <- list.files(navarre.path,
#'                           full.names = TRUE,
#'                           recursive = TRUE,
#'                           pattern = "\\.tif$")
#' # select b01
#' navarre.img <- navarre.img[grepl("b01_1",navarre.img)]
#' navarre.b01.ras <- lapply(navarre.img,raster)
#' navarre.b01.stack <- stack(lapply(navarre.b01.ras, projectRaster, navarre.b01.ras[[1]]))
#' 
#' # reproject the cloud mask to the projection of navarre.b01.stack
#' cmask.stack <- stack(lapply(cmask.ras, projectRaster, navarre.b01.stack))
#' 
#' # plot the cloud free b01 layer
#' spplot(navarre.b01.stack*cmask.stack)
#' }
modCloudMask<-function(startDate,endDate,AppRoot,extent,out.name="outname",raw.rm=FALSE,overwrite=FALSE,verbose=FALSE,...){
  arg <- list(...)
  AppRoot<-pathWinLx(AppRoot)
  AppRoot <- file.path(AppRoot,"Modis","CloudMask")
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
            gutils = TRUE,
            AppRoot=AppRoot,
            verbose=verbose)
  
  tif.images<-list.files(file.path(AppRoot,"outfile"),recursive = TRUE,full.names = TRUE,pattern = "\\.tif$")
  dir.create(file.path(AppRoot,out.name),recursive=TRUE,showWarnings = verbose)
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