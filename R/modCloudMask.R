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
#' modDownSearch(product = "MOD09GA",
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
modCloudMask<-function(src,AppRoot,overwrite=FALSE,...){
  arg<-list(...)
  src<-pathWinLx(src)
  AppRoot<-pathWinLx(AppRoot)
  imgdir.list<-list.dirs(src,recursive=FALSE)
  AppRoot<-file.path(AppRoot,"CloudMask")
  dir.create(AppRoot,showWarnings = FALSE,recursive = TRUE)
  for(id in imgdir.list){
    out.img<-file.path(AppRoot,paste0(basename(id),paste0("_",getRGISToolsOpt("MOD09BANDS")["cloud"],".tif")))
    if(!file.exists(out.img)|overwrite){
      #id<-imgdir.list[1]
      message(paste0("Creating cloud mask of date ",genGetDates(basename(id)),"."))
      tif.list<-list.files(id,pattern = "\\.tif$",full.names = TRUE)
      cloudmask<-tif.list[grepl(getRGISToolsOpt("MOD09BANDS")["quality"],tif.list)]
      
      r <- raster(cloudmask)
      stime<-Sys.time()
      v <- matrix(as.numeric(matrix(intToBits(getValues(r)), ncol = 32, byrow = T)[,1:3]),ncol = 3)
      
      # clouds
      # interpret the bytes: 0 = clear, 1+1 = not known, assumed clear
      r[] <- rowSums(v[,1:2])
      r[r==1] <- NA
      r[r!=1] <- 1
      #r[(r == 0 | r == 2)] <- 1
      # shadows
      # interpret the bytes: 0 = clear, 1 = shadow
      r_shadow <- r
      r_shadow <- 1 - v[,3]
      r_shadow[r_shadow == 0] <- NA
      # save the result
      ras.cloud <- r * r_shadow
      writeRaster(ras.cloud,out.img,overwrite=overwrite)
    }else{
      message(paste0("Cloud mask of date ",genGetDates(basename(id))," already exists."))
    }
  }
}
