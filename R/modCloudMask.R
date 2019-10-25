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
#' @param src the path to the folder with the MODIS with \code{state_1km} images. 
#' @param AppRoot the directory where cloud masks are saved.
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing
#' images with the same name.
#' @param ... arguments for nested functions.
#' \itemize{
#'   \item \code{dates} a vector with the dates being considered
#'   for creating cloud mask. This argument is optional.
#' }
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
#'             startDate = as.Date("01-01-2017", "%d-%m-%Y"),
#'             endDate = as.Date("03-01-2017", "%d-%m-%Y"),
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
#' 
#' src.tiles.navarre <- file.path(src.tiles, "Navarre")
#' # generate the cloud masks      
#' modCloudMask(src = src.tiles.navarre,
#'              AppRoot = src.tiles,
#'              overwrite = TRUE)
#'              
#' src.cloud <- file.path(src.tiles,"CloudMask")
#' src.cloud.stack <- stack(list.files(src.cloud, full.names=TRUE, pattern="CLD"))
#' 
#' # select b01
#' navarre.img <- stack(list.files(tif.src.tiles, 
#'                                 full.names=TRUE, 
#'                                 recursive = TRUE, 
#'                                 pattern="b01_1"))
#' 
#' # project to 500m
#' src.cloud.stack <- projectRaster(src.cloud.stack,navarre.img)
#' 
#' # plot the cloud free b01 layer
#' spplot(navarre.img*src.cloud.stack)
#' }
modCloudMask<-function(src,AppRoot,out.name,overwrite=FALSE,...){
  arg<-list(...)
  src<-pathWinLx(src)
  if(!missing(AppRoot)){
    AppRoot<-pathWinLx(AppRoot)
    AppRoot<-file.path(AppRoot,"CloudMask")
    if(missing(out.name))
      AppRoot<-file.path(AppRoot,"CloudMask")
    else
      AppRoot<-file.path(AppRoot,paste0(out.name,"_CloudMask"))
    dir.create(AppRoot,showWarnings = FALSE,recursive = TRUE)
  }
  
  imgdir.list<-list.dirs(src,recursive=FALSE)
  if("dates"%in%names(arg)){imgdir.list<-imgdir.list[genGetDates(imgdir.list)%in%arg$dates]}

  for(id in imgdir.list){
    tif.list<-list.files(id,pattern = "\\.tif$",full.names = TRUE)
    cloudmask<-tif.list[grepl(getRGISToolsOpt("MOD09BANDS")["quality"],tif.list)]
    if(missing(AppRoot)){
      out.img<-gsub(paste0(getRGISToolsOpt("MOD09BANDS")["quality"],".tif"),"_CLD.tif",cloudmask,ignore.case =TRUE)
    }else{
      out.img<-file.path(AppRoot,paste0(basename(id),paste0("_",getRGISToolsOpt("MOD09BANDS")["cloud"],".tif")))
    }
    
    if(!file.exists(out.img)|overwrite){
      #id<-imgdir.list[1]
      message(paste0("Creating cloud mask of date ",modGetDates(basename(id)),"."))

      r <- raster(cloudmask)
      stime<-Sys.time()
      v <- matrix(as.numeric(matrix(intToBits(getValues(r)), ncol = 32, byrow = T)[,1:3]),ncol = 3)
      
      # clouds
      # interpret the bytes: 0 = clear, 1+1 = not known, assumed clear
      r[] <- rowSums(v[,1:2])
      r[r==1] <- NA
      r[r!=1] <- 1
      # r[(r == 0 | r == 2)] <- 1
      # shadows
      # interpret the bytes: 0 = clear, 1 = shadow
      r_shadow <- r
      r_shadow <- 1 - v[,3]
      r_shadow[r_shadow == 0] <- NA
      # save the result
      ras.cloud <- r * r_shadow
      writeRaster(ras.cloud,out.img,overwrite=overwrite)
    }else{
      message(paste0("Cloud mask of date ",modGetDates(basename(id))," already exists."))
    }
  }
}
