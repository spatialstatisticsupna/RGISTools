#' Create cloud masks for MODIS images
#' 
#' \code{modCloudMask} creates cloud masks derived from the State Quality
#' Assurance (State QA) band.
#' 
#' This function, interprets the State Quality Assurance (State QA) band to
#' create cloud masks. The \code{NA} and \code{1} values of the mask represent
#' cloudy and clear-sky pixels pixels respectively.
#' @param src the path to the folder with the MODIS with \code{state_1km} images. 
#' @param AppRoot the directory where cloud masks are saved.
#' @param out.name the name of the folder that stores the outputs. 
#' If the arguemnt is not defined the folder will be named as "CloudMask".
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing
#' images with the same name.
#' @param ... arguments for nested functions.
#' \itemize{
#'   \item \code{dates} a vector of dates being considered
#'   for creating cloud masks. This argument is optional.
#' }
#' @return this function does not return anything. It saves the cloud masks (CLD)
#' as GTiff files in the \code{AppRoot} directory.
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' wdir <- file.path(tempdir(),"Path_for_downloading_folder")
#' print(wdir)
#' 
#' # search and download images from MODIS between
#' # 01-01-2018 and 03-01-2018 for the region of Navarre
#' modDownSearch(product = "MOD09GA",
#'             startDate = as.Date("01-01-2017", "%d-%m-%Y"),
#'             endDate = as.Date("03-01-2017", "%d-%m-%Y"),
#'             username = "username",
#'             password = "password",
#'             AppRoot = wdir,
#'             extract.tif = TRUE,
#'             collection = 6,
#'             extent = ex.navarre)
#'             
#' # assign src1 as the output folder for modMosaic
#' wdir.mod <- file.path(wdir, "Modis")
#' wdir.mod.tiles <- file.path(src.mod, "MOD09GA")
#' wdir.mod.tif <- file.path(wdir.mod.tiles, "tif")
#' # mosaic the MODIS images
#' modMosaic(wdir.mod.tif, # the input folder 
#'           AppRoot = wdir.mod.tiles, # the output folder 
#'           out.name = "Navarre", # creates Navarre folder in AppRoot
#'           gutils = TRUE,
#'           extent = ex.navarre)
#' 
#' 
#' wdir.mod.navarre <- file.path(wdir.mod.tiles, "Navarre")
#' # generate the cloud masks      
#' modCloudMask(src = wdir.mod.navarre,
#'              AppRoot = wdir.mod.tiles,
#'              overwrite = TRUE)
#'              
#' files.mod.cld <- file.path(wdir.mod.tiles,"CloudMask")
#' img.mod.cld <- stack(list.files(files.mod.cld, full.names=TRUE, pattern="CLD"))
#' 
#' # select b01
#' img.mod.navarre <- stack(list.files(img.mod.cld, 
#'                                     full.names=TRUE, 
#'                                     recursive = TRUE, 
#'                                     pattern="b01_1"))
#' 
#' # project to 500m
#' img.mod.cld.500 <- projectRaster(img.mod.cld,img.mod.navarre)
#' 
#' # plot the cloud free b01 layer
#' spplot(img.mod.navarre*img.mod.cld.500)
#' }
modCloudMask<-function(src,AppRoot,out.name,overwrite=FALSE,...){
  arg<-list(...)
  src<-pathWinLx(src)
  if(!missing(AppRoot)){
    AppRoot<-pathWinLx(AppRoot)
    if(missing(out.name))
      AppRoot<-file.path(AppRoot,"CloudMask")
    else
      AppRoot<-file.path(AppRoot,out.name)
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
