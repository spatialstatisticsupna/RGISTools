#' Downloads a time series of satellite images from Landsat 7-8
#'
#' \code{\link{lsDownSearch}} downloads the list of images provided by \code{\link{ls7Search}} and \code{\link{ls8Search}} functions.
#' The images are saved as ‘.tiff’ files in the \code{AppRoot} directory.
#'
#' This function is used for download Landsat images. Uses the data frame result from  any landsat
#' search function (\code{\link{ls7Search}} or \code{\link{ls8Search}}) and download all images in data frame.
#' Image download requires USGS login account. \href{https://ers.cr.usgs.gov/register/}{Get your credentials}.
#'
#' The image files from the USGS EROS web service are compressed as ‘.tar.gz’ files. lsDownSearch decompresses the
#' images and obtains the corresponding ‘.tif’ files. The ‘.tif’ files are saved in the
#' \code{AppRoot} directory. When \code{untarDir} is defined the function untars the images in this folder.
#' This replicates the images in compresses version as 'tar.gz' and uncompresses version. To save space in the disk
#' \code{raw.rm = T} can be defined, and \code{\link{lsDownSearch}} will remove the ‘tar.gz’ files.
#' If \code{raw.rm = F}, the original files remain, which might be useful to have access to the original files
#' in the future and avoid further downloads. By default, \code{\link{lsDownSearch}} saves the images in (...), in the \code{AppRoot}
#' directory. To change this setting, provide \code{AppRoot = "the full path as an argument"}.
#'
#' @param searchres the results from \code{ls7Search} or \code{ls8Search}.
#' @param username login credentials to access the USGS EROS web service.
#' @param password login credentials to access the USGS EROS web service.
#' @param cookies.file  File path for saving the cookies that are used in the download process.
#' @param verbose logical argument. If \code{TRUE} the function prints running stages and warnings.
#' @param raw.rm logical argument. If \code{TRUE} removes the raw images.
#' @param untar logical argument. If \code{TRUE} untars downloaded images.
#' @param ... argument for function nestering accepts:
#' \itemize{
#'   \item \code{AppRoot} the directory to save the downloaded images.
#' }
#'
#'
#' @examples
#' \dontrun{
#' # Search and download the images from Landsat-8 comprised between
#' # 2011 and 2013 for the region of Navarre
#' data(ex.navarre)
#' search.res <- ls8Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                         endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                         extent = ex.navarre,
#'                         browseAvaliable = "Y")
#'
#' #download 1 image
#' lsDownSearch(search.res[1,], 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE, 
#'              raw.rm = TRUE)
#' # download 10 images
#' lsDownSearch(search.res[1:10,], 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE, 
#'              raw.rm = TRUE)
#' # download all the images
#' lsDownSearch(search.res, 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE, 
#'              raw.rm = TRUE)
#'
#' # Search and download the images from Landsat-7 comprised between
#' # 2011 and 2013 for the region of Navarre
#' data(ex.navarre)
#' search.res <- ls7Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                         endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                         extent = ex.navarre,
#'                         browseAvaliable = "Y")
#' #download 1 image
#' lsDownSearch(search.res[1,], 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE, 
#'              raw.rm = TRUE)
#' # download 10 images
#' lsDownSearch(search.res[1:10,], 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE, 
#'              raw.rm = TRUE)
#' # download all the images
#' lsDownSearch(search.res, 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE, 
#'              raw.rm = TRUE)
#' 
#' # removes metadata data frame to free memory
#' lsRemoveMetadata()
#' 
#' files <- list.files("./", 
#'                     pattern = "\\.TIF$", 
#'                     full.names = TRUE, 
#'                     recursive = TRUE)[6,5,4]
#' files.stack <- stack(files)
#' qrange <- c(0.001, 0.999)
#' imagen <- varRGB(files.stack.raster[[1]], 
#'                  files.stack.raster[[2]],
#'                  files.stack.raster[[3]],
#'                  qrange)
#' plotRGB(imagen)
#' }
lsDownSearch<-function(searchres,
                       username=NULL,
                       password=NULL,
                       cookies.file="lscookies.txt",
                       verbose=FALSE,
                       raw.rm=FALSE,
                       untar=FALSE,
                       overwrite=FALSE,
                       ...){
  stopifnot(class(searchres)=="data.frame")
  if(is.null(username)|is.null(password)){
    stop("Username and/or password not defined!")
  }
  arg<-list(...)
  AppRoot<-defineAppRoot(...)
  #identify mission
  if(grepl("LC8",searchres[1,]$sceneID)){
    downDir<-getRGISToolsOpt("LS8DownloadDir")
  }else if(grepl("LE7",searchres[1,]$sceneID)){
    downDir<-getRGISToolsOpt("LS7DownloadDir")
  }else{
    stop("Unknown mission.")
  }
  downPath<-file.path(AppRoot,downDir,"raw")
  #create download folder
  if(!file.exists(downPath)){
    dir.create(downPath,recursive=T)
  }

  #start usgs session
  handler<-startUSGSsession(username,password,cookies.file,verbose)
  if(verbose)
    print("USGS session started, downloading images...")
  for(scene in searchres$sceneID){
    if(!file.exists(paste0(downPath,"/",scene,".tar.gz"))){
      if(grepl("LC8",searchres[1,]$sceneID)){
        .ls8DownloadUSGS(scene,downPath,handler,verbose=verbose,overwrite=overwrite)
      }else if(grepl("LE7",searchres[1,]$sceneID)){
        .ls7DownloadUSGS(scene,downPath,handler,verbose=verbose,overwrite=overwrite)
      }
    }
    #Unzip in downDir when available
    if(untar){
      print(paste0("Untar ",scene," file."))
      untarDir<-file.path(AppRoot,downDir,"untar",scene)
      if(overwrite){
        file.remove(untarDir,showWarnings=FALSE,recursive=TRUE)
      }
      dir.create(untarDir,recursive=T,showWarnings=FALSE)
      untar(paste0(downPath,"/",scene,".tar.gz"),exdir=untarDir)
      #Flag is true, so remove compressed files
      if(raw.rm){
        file.remove(paste0(downPath,"/",scene,".tar.gz"))
      }
    }
  }

  if(untar){
    message(paste0("The images have been downloaded and saved on HDD. \nFile path: ",untarDir))
  }else{
    message(paste0("The images have been downloaded and saved on HDD. \nFile path: ",downPath))
  }

}
