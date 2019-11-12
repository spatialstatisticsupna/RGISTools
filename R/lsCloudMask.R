#' Create cloud masks for Landsat images
#' 
#' \code{lsCloudMask} creates a cloud mask derived from the band for quality
#' assurance (BQA) from Landsat-7 or Landsat-8 time series. The function is
#' applied to untarred images, such as those resulting from 
#' \code{\link{lsDownload}} or \code{\link{lsDownSearch}}. The result is a new
#' image band, called cloud (CLD), that is saved as separate GTiffs.
#' 
#' The valid range for the \code{sensitivity} threshold is 0-8000. By defualt,
#' the argument is set to 2800. We recommend 600 and 2800 for Landsat-7 and
#' Landsat-8 respectively.
#'
#' @param src the path to the folder with the untarred images from Landsat-7 or
#' Landsat-8.
#' @param AppRoot the directory where the cloud masks are saved.
#' @param out.name the name of the folder that stores the outputs. 
#' If the arguemnt is not defined the folder will be named as "CloudMask".
#' @param sensitivity \code{numeric} argument. Defines the sensitivity of the
#' cloud detection method.
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing
#' images with the same name.
#' @param verbose logical argument. If \code{TRUE}, the function prints the
#' running steps and warnings.
#' @param ... arguments for nested functions.
#' \itemize{
#'   \item \code{dates} a vector with the dates being considered
#'   for creating cloud mask. This argument is optional.
#' }
#' 
#' @return this function does not return anything. It creates new GTiff files
#' for a new cloud band (CLD) inside the AppRoot folder.
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' wdir <- file.path(tempdir(),"Path_for_downloading_folder")
#' print(src)
#' 
#' # search and download images from Landsat-8 between
#' # 01-01-2018 and 20-01-2018 for the region of Navarre
#' lsDownSearch(satellite = "ls8",
#'              username = "username",
#'              password = "password",
#'              startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'              endDate = as.Date("20-01-2018", "%d-%m-%Y"),
#'              pathrow = list(c(200, 31), c(200, 30)),
#'              untar = TRUE,
#'              AppRoot = wdir)
#'            
#' # define the path where the GTiff images are located
#' src.ls8 <- file.path(wdir,"Landsat8")
#' src.ls8.untar <- file.path(src.ls8,"untar")
#'             
#' # mosaic and crop the imagery
#' lsMosaic(src = src.ls8.untar,
#'          AppRoot = src.ls8,
#'          out.name = "Navarre",
#'          extent = ex.navarre,
#'          bFilter = c("CLD"),
#'          gutils = TRUE, # using gdalUtils
#'          overwrite = TRUE) # overwrite
#'          
#' # generate the path where mosaicked images are located
#' src.ls8.navarre <- file.path(src.ls8, "Navarre")
#' 
#' # calculate the cloud mask from QC layer
#' lsCloudMask(src=src.ls8.navarre,
#'             overwrite=TRUE,
#'             AppRoot = src.ls8) 
#'             
#' # load the B1 layer and calculate the CLD layer
#' files.ls8.navarre.path <- list.files(src.ls8.navarre,
#'                                      full.names = TRUE,
#'                                      recursive = TRUE,
#'                                      pattern = "\\.tif$")
#' tiles.ls8.cld <- files.ls8.navarre.path[grepl("CLD",tiles.path)]
#' tiles.ls8.b1 <- files.ls8.navarre.path[grepl("B1.tif",tiles.path)]
#' img.ls8.cld <- lapply(tiles.ls8.cld,raster)
#' img.ls8.b1 <- lapply(tiles.ls8.b1,raster)
#' 
#' # calculate cloud free b1 layers
#' img.ls8.b1.cloud.free <- img.ls8.b1[[1]] * img.ls8.cld[[1]]
#' spplot(img.ls8.b1.cloud.free)
#' }
lsCloudMask<-function(src,AppRoot,out.name,sensitivity=28000,overwrite=FALSE,verbose=FALSE,...){
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
  if(verbose){message(paste0("\nIdentifies folders:  \n",imgdir.list))}
  
  #manage level 2 bands
  lvl<-list.files(imgdir.list[1],pattern = "\\.tif$")
  if(any(grepl(getRGISToolsOpt("LS8BANDS")["quality"],lvl))){
    qc<-getRGISToolsOpt("LS8BANDS")["quality"]
  }else if(any(grepl("pixel_qa",lvl))){
    qc<-"pixel_qa"
  }else{
    warning("Quality band not found!")
  }
  
  for(id in imgdir.list){
    #id<-imgdir.list[2]
    if(verbose){message(paste0("Mask Folder:  \n",id))}
    tif.list<-list.files(id,pattern = "\\.tif$",full.names = TRUE,ignore.case = TRUE)
    if(verbose){message(paste0("qc:  ",qc))}
    qcmask<-tif.list[grepl(qc,tif.list,ignore.case = TRUE)]
    if(missing(AppRoot)){
      out.img<-gsub(paste0(qc,".TIF"),"CLD.TIF",qcmask,ignore.case =TRUE)
    }else{
      out.img<-file.path(AppRoot,paste0(basename(id),paste0("_",getRGISToolsOpt("LS8BANDS")["cloud"],".tif")))
    }
    
    if(verbose){message(paste0("QC mask name:  \n",qcmask))}
    if(length(qcmask)==0){
      message(paste0("No cloud mask found for date ",genGetDates(basename(id))))
      next
    }

    if(!file.exists(out.img)||overwrite){
      message("Creating cloud mask for tile ",dirname(qcmask))
      
      ras.cloud<-readAll(raster(qcmask))
      mn<-minValue(ras.cloud)
      if(verbose){
        message(paste0("Minimun: ",mn))
      }
      ras.cloud[ras.cloud<=max(mn,1)]<-0
      ras.cloud[ras.cloud>=sensitivity]<-0
      ras.cloud[ras.cloud!=0]<-1
      
      NAvalue(ras.cloud)<-0
      writeRaster(ras.cloud,out.img,overwrite=overwrite)
      if(grepl(".TIF",out.img)){
        file.rename(gsub(".tif",".TIF",out.img),out.img)
      }
      rm(ras.cloud)
    }else{
      message(paste0("Cloud mask of date ",genGetDates(basename(id))," already exists."))
    }
  }
  message(paste0("Cloud mask in ",src," for all the tiles."))
}
