#' Download a time series of satellite images from Landsat 7-8
#'
#' \code{\link{lsDownSearch}} downloads the list of images provided by \code{\link{ls7Search}} and \code{\link{ls8Search}} functions.
#' The images are saved as ‘GTiff’ files in the \code{AppRoot} directory.
#'
#' This function is used for downloading Landsat images. It downloads all the list of images 
#' provided in the output data frame of the search function (\code{\link{ls7Search}} or \code{\link{ls8Search}}).
#' Image download requires USGS login account. \href{https://ers.cr.usgs.gov/register/}{Get your credentials}.
#'
#' The image files from the USGS EROS web service are compressed as ‘.tar.gz’ files. \code{\link{lsDownSearch}} decompresses the
#' images and obtains the corresponding ‘.tif’ files. The ‘.tif’ files are saved in the
#' \code{AppRoot} directory. When \code{untar} is defined the function untars the images in "untar" folder.
#' This replicates the images in compresses version as 'tar.gz' and uncompresses version in "untar" folder. To save space in the disk
#' \code{raw.rm = TRUE} can be defined, and \code{\link{lsDownSearch}} will remove the ‘tar.gz’ files.
#' If \code{raw.rm = FALSE}, the original files remain, which might be useful to have access to the original files
#' in the future and avoid further downloads. By default, \code{\link{lsDownSearch}} saves the images in (...), in the \code{AppRoot}
#' directory. To change this setting, provide \code{AppRoot = "the full path as an argument"}.
#'
#' @param searchres the results from \code{ls7Search} or \code{ls8Search}.
#' @param username login credentials to access the USGS EROS web service.
#' @param password login credentials to access the USGS EROS web service.
#' @param lvl flag to specify Landsat product level wanted. Default value, 1.
#' @param products character vector with the avaliable products for Landsat level 2. By default \code{c("sr","source_metadata")}.
#' @param cookies.file  File path for saving the cookies that are used in the download process.
#' @param verbose logical argument. If \code{TRUE}, the function prints running stages and warnings.
#' @param raw.rm logical argument. If \code{TRUE}, removes the raw images.
#' @param untar logical argument. If \code{TRUE}, untars downloaded images.
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing images with the same name.
#' @param ... argument for function nestering accepts:
#' \itemize{
#'   \item \code{AppRoot} the directory to save the downloaded images.
#' }
#'
#'
#' @examples
#' \dontrun{
#' #' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # Search and download the images from Landsat-8 between
#' # 2011 and 2013 in the region of Navarre
#' search.res <- ls8Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                         endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                         extent = ex.navarre,
#'                         browseAvaliable = "Y")
#'
#' # download 1 image
#' lsDownSearch(searchres = search.res[1,], 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE, 
#'              raw.rm = TRUE)
#' # download 10 images
#' lsDownSearch(searchres = search.res[1:10,], 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE, 
#'              raw.rm = TRUE)
#' # download all the images
#' lsDownSearch(searchres = search.res, 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE, 
#'              raw.rm = TRUE)
#'
#' # Search and download the images from Landsat-7 between
#' # 2011 and 2013 in the region of Navarre
#' search.res <- ls7Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                         endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                         extent = ex.navarre,
#'                         browseAvaliable = "Y")
#' # download 1 image
#' lsDownSearch(searchres = search.res[1,], 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE, 
#'              raw.rm = TRUE)
#' # download 10 images
#' lsDownSearch(searchres = search.res[1:10,], 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE, 
#'              raw.rm = TRUE)
#' # download all the images
#' lsDownSearch(searchres = search.res, 
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
                       lvl=1,
                       products=c("sr","source_metadata"),
                       verbose=FALSE,
                       raw.rm=FALSE,
                       untar=FALSE,
                       overwrite=FALSE,
                       n.attempts=5,
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

  if(lvl==1){
    message("Starting Landsat level 1 download process...")
    lsEarthExplorerdownload(searchres=searchres,
                            username=username,
                            password=password,
                            cookies.file=NULL,
                            downDir=downDir,
                            AppRoot=AppRoot,
                            downPath=downPath,
                            verbose=verbose,
                            untar=untar,
                            overwrite=overwrite)
  }else if(lvl==2){
    message("Starting Landsat level 2 download process...")
    lsEspaOrderImages(search.res=searchres,
                      username=username,
                      password=password,
                      product=products,
                      verbose=verbose)
    message("Ordering images on ESPA platform...")
    c.handle<-lsEspaCreateConnection(username=username,password=password)
    images.order<-lsEspaGetOrderImages(c.handle=c.handle)
    message("Cheking the order status and starting the download process.")
    lsEspaDownloadOrders(images.order=images.order,
                         c.handle=c.handle,
                         verbose=verbose,
                         n.attempts=n.attempts,
                         untar=untar,
                         overwrite=overwrite,
                         AppRoot=downPath)
    close(c.handle)
  }else{
    stop("Landsat level not identified, check 'lvl'. Valid values 1 or 2.")
  }
  if(untar){
    message(paste0("The images have been downloaded and saved on HDD. \nFile path: ",untarDir))
  }else{
    message(paste0("The images have been downloaded and saved on HDD. \nFile path: ",downPath))
  }
}






