#' Download Landsat-7 or Landsat-8 images from a search list
#'
#' \code{lsDownload} downloads the results from \code{\link{ls7Search}} and
#' \code{\link{ls8Search}} functions. The images are saved as GTiff files in the
#' \code{AppRoot} directory.
#'
#' \code{lsDonwSearch} downloads the list of URLs provided by \link{ls7Search}
#' or \link{ls8Search} as a \code{data.frame}. The function requires an USGS's
#' `EarthExplorer' account, which can be obtained 
#' \href{https://ers.cr.usgs.gov/register/}{here}.
#' 
#' The files from `EarthExplorer' are compressed as ‘tar.gz’. \code{lsDownload}
#' decompresses the images and obtains the corresponding GTiffs. The GTiff files
#' are saved in the \code{AppRoot} directory. To change this option, provide
#' \code{AppRoot = “full path”}. When \code{untar = TRUE}, the function
#' decompresses the imagery. When only a subset of bands is required, band names
#' can be provided through the \code{bFilter} argument. The band names are 
#' specified by string “band” and the band number (e.g., “band1”). Image
#' decompression duplicates the information due to the presence of both,
#' compressed and decompressed images. Set \code{raw.rm = TRUE} to remove former
#' ones.
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
#' @param l2rqname character argument defining the name of the order for level
#' 2 products.
#' @param ... arguments for nested functions.
#'  \itemize{
#'        \item \code{dates} a vector with the capturing dates being considered
#'   for downloading. 
#'        \item \code{bFilter} a vector with the bands to be extracted when \code{untar=TRUE}. If not
#'   supplied, all bands are extracted.
#' }
#' 
#' @return this function does not return anything. It saves the imagery as
#' `tar.gz’ (and GTiff files) in a folder called `raw’ (`untar’) in the
#'  \code{AppRoot} directory.
#' 
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' 
#' wdir <- file.path(tempdir(),"Path_for_downloading_folder")
#' print(wdir)
#' 
#' # search and download the images from Landsat-8 between
#' # 2011 and 2013 in the region of Navarre
#' sres <- ls8Search(startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'                   endDate = as.Date("20-01-2018", "%d-%m-%Y"),
#'                   extent = ex.navarre,
#'                   browseAvaliable = "Y",
#'                   AppRoot = wdir)
#'
#' # download 1 image
#' lsDownload(searchres = sres[1,], 
#'            username = "user", 
#'            password = "pass", 
#'            AppRoot = wdir,
#'            untar = TRUE)
#' # download 4 images
#' lsDownload(searchres = sres[1:4,], 
#'            username = "user", 
#'            password = "pass",
#'            AppRoot = wdir, 
#'            untar = TRUE)
#' # download all the images
#' lsDownload(searchres = sres, 
#'            username = "user", 
#'            password = "pass",
#'            AppRoot = wdir,
#'            untar = TRUE)
#'
#' # search and download the images from Landsat-7 between
#' # 2011 and 2013 in the region of Navarre
#' wdir <- file.path(tempdir(),"Path_for_downloading_folder")
#' print(wdir)
#' sres <- ls7Search(startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'                   endDate = as.Date("20-01-2018", "%d-%m-%Y"),
#'                   extent = ex.navarre,
#'                   browseAvaliable = "Y",
#'                   AppRoot = wdir)
#' # download 1 image
#' lsDownload(searchres = sres[1,], 
#'            username = "user", 
#'            password = "pass", 
#'            untar = TRUE,
#'            AppRoot = wdir)
#' # download 4 images
#' lsDownload(searchres = sres[1:4,], 
#'            username = "user", 
#'            password = "pass", 
#'            untar = TRUE, 
#'            AppRoot = wdir)
#' # download all the images
#' lsDownload(searchres = sres, 
#'            username = "user", 
#'            password = "pass", 
#'            untar = TRUE, 
#'            AppRoot = wdir)
#' 
#' # removes the metadata to free memory space
#' lsRemoveMetadata()
#' 
#' # select Landsat-7 RGB bands
#' wdir.ls7 <- file.path(wdir,"Landsat7")
#' files.ls7 <- list.files(wdir.ls7, 
#'                         pattern = "\\.TIF$", 
#'                         full.names = TRUE, 
#'                         recursive = TRUE)[c(6,5,4)]
#' files.ls7.rgb <- stack(files.ls7)
#' qrange <- c(0.001, 0.999)
#' img.ls7.rgb <- varRGB(files.ls7.rgb[[1]], 
#'                       files.ls7.rgb[[2]],
#'                       files.ls7.rgb[[3]],
#'                       qrange)
#' plotRGB(img.ls7.rgb)
#' }
lsDownload<-function(searchres,
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
                     l2rqname,
                     ...){
  stopifnot(class(searchres)=="data.frame")
  if(is.null(username)|is.null(password)){
    stop("Username and/or password not defined!")
  }
  
  if(!missing(l2rqname)){setRGISToolsOpt("LS.ESPA.Request",l2rqname)}
  
  arg<-list(...)
  if("dates"%in%names(arg)){searchres<-searchres[as.Date(searchres$acquisitionDate)%in%arg$dates,]}
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
  message(paste0("Downloading the images in: ",downPath))
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
                            overwrite=overwrite,
                            ...)
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
                         AppRoot=downPath,
                         ...)
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




