#' Download Landsat-7 or Landsat-8 images from a search list
#'
#' \code{lsDownSearch} downloads the results from \code{\link{ls7Search}} and
#' \code{\link{ls8Search}} functions. The images are saved as GTiff files in the
#' \code{AppRoot} directory.
#'
#' \code{lsDonwSearch} downloads the list of URLs provided by \link{ls7Search}
#' or \link{ls8Search} as a \code{data.frame}. The function requires an USGS's
#' `EarthExplorer' account, which can be obtained 
#' \href{https://ers.cr.usgs.gov/register/}{here}.
#' 
#' The files from `EarthExplorer' are compressed as ‘tar.gz’. \code{lsDownSearch}
#' decompresses the images and obtains the corresponding GTiffs. The GTiff files
#' are saved in the \code{AppRoot} directory. To change this option, provide
#' \code{AppRoot = “full path”}. When \code{untar = TRUE}, the function
#' decompresses the imagery. Image decompression duplicates the information
#' due to the presence of both, compressed and decompressed images. Set 
#' \code{raw.rm = TRUE} to remove former ones.
#' 
#' @param searchres the results from \code{\link{ls7Search}} or 
#' \code{\link{ls8Search}}.
#' @param username USGS's `EarthExplorer' username.
#' @param password USGS's `EarthExplorer' password.
#' @param AppRoot the download directory.
#' @param lvl a number specifying the processing level. Default value, 1.
#' @param product \code{character} vector with the requested Level-2 products.
#' By default \code{c("sr","source_metadata")}.
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param raw.rm logical argument. If \code{TRUE}, removes the raw images.
#' @param untar logical argument. If \code{TRUE}, untars downloaded images.
#' @param nattempts the number of attempts to download an image in case it
#' becomes corrupted.
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing
#' images with the same name.
#' @param ... arguments for nested functions:
#'
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' 
#' src <- paste0(tempdir(),"/Path_for_downloading_folder")
#' print(src)
#' 
#' # search and download the images from Landsat-8 between
#' # 2011 and 2013 in the region of Navarre
#' search.res <- ls8Search(startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'                         endDate = as.Date("20-01-2018", "%d-%m-%Y"),
#'                         extent = ex.navarre,
#'                         browseAvaliable = "Y",
#'                         AppRoot = src)
#'
#' # download 1 image
#' lsDownSearch(searchres = search.res[1,], 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE)
#' # download 4 images
#' lsDownSearch(searchres = search.res[1:4,], 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE)
#' # download all the images
#' lsDownSearch(searchres = search.res, 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE)
#'
#' # search and download the images from Landsat-7 between
#' # 2011 and 2013 in the region of Navarre
#' src <- paste0(tempdir(),"/Path_for_downloading_folder")
#' print(src)
#' search.res <- ls7Search(startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'                         endDate = as.Date("20-01-2018", "%d-%m-%Y"),
#'                         extent = ex.navarre,
#'                         browseAvaliable = "Y",
#'                         AppRoot=src)
#' # download 1 image
#' lsDownSearch(searchres = search.res[1,], 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE,
#'              AppRoot=src)
#' # download 4 images
#' lsDownSearch(searchres = search.res[1:4,], 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE, 
#'              AppRoot=src)
#' # download all the images
#' lsDownSearch(searchres = search.res, 
#'              username = "user", 
#'              password = "pass", 
#'              untar = TRUE, 
#'              AppRoot=src)
#' 
#' # removes the metadata to free memory space
#' lsRemoveMetadata()
#' 
#' # select Landsat-7 RGB bands
#' src.ls7 <- file.path(src,"Landsat7")
#' files <- list.files(src.ls7, 
#'                     pattern = "\\.TIF$", 
#'                     full.names = TRUE, 
#'                     recursive = TRUE)[c(6,5,4)]
#' files.stack <- stack(files)
#' qrange <- c(0.001, 0.999)
#' imagen <- varRGB(files.stack[[1]], 
#'                  files.stack[[2]],
#'                  files.stack[[3]],
#'                  qrange)
#' plotRGB(imagen)
#' }
lsDownSearch<-function(searchres,
                       username=NULL,
                       password=NULL,
                       AppRoot,
                       lvl=1,
                       product=c("sr","source_metadata"),
                       verbose=FALSE,
                       raw.rm=FALSE,
                       untar=FALSE,
                       overwrite=FALSE,
                       nattempts=5,
                       ...){
  stopifnot(class(searchres)=="data.frame")
  if(is.null(username)|is.null(password)){
    stop("Username and/or password not defined!")
  }
  arg<-list(...)
  AppRoot<-pathWinLx(AppRoot)
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
    dir.create(downPath,recursive=TRUE)
  }

  if(lvl==1){
    message("Starting Landsat level 1 download process...")
    lsEarthExplorerdownload(searchres=searchres,
                            username=username,
                            password=password,
                            cookies.file=NULL,
                            downDir=downDir,
                            downPath=downPath,
                            AppRoot=AppRoot,
                            verbose=verbose,
                            untar=untar,
                            raw.rm=raw.rm,
                            overwrite=overwrite)
  }else if(lvl==2){
    message("Starting Landsat level 2 download process...")
    lsEspaOrderImages(search.res=searchres,
                      username=username,
                      password=password,
                      product=product,
                      verbose=verbose)
    message("Ordering images on ESPA platform...")
    c.handle<-lsEspaCreateConnection(username=username,password=password)
    images.order<-lsEspaGetOrderImages(c.handle=c.handle)
    message("Cheking the order status and starting the download process.")
    lsEspaDownloadOrders(orders=images.order,
                         c.handle=c.handle,
                         verbose=verbose,
                         nattempts=nattempts,
                         untar=untar,
                         overwrite=overwrite,
                         AppRoot=downPath)
  }else{
    stop("Landsat level not identified, check 'lvl'. Valid values 1 or 2.")
  }
  if(untar){
    untarDir<-file.path(AppRoot,downDir,"untar")
    message(paste0("The images have been downloaded and saved on HDD. \nFile path: ",untarDir))
  }else{
    message(paste0("The images have been downloaded and saved on HDD. \nFile path: ",downPath))
  }
}




