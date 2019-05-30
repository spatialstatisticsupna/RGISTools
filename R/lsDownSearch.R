#' Downloads a time series of satellite images from Landsat 7-8
#'
#' \code{lsDownSearch} downloads the list of images provided by \code{ls7Search} and \code{ls8Search} functions.
#' The images are saved as ‘.tiff’ files in the AppRoot directory..
#'
#' This function is used for download landsat images. Uses the data frame result from  any landsat
#' search function (\code{\link{ls7Search}} or \code{\link{ls8Search}}) and download all images in data frame.
#' Image download requires USGS login account from \url{https://ers.cr.usgs.gov/register/}.
#'
#' The image files from the USGS EROS web service are compressed as ‘.tar.gz’ files. lsDownSearch decompresses the
#' images and obtains the corresponding ‘.tiff’ files. The ‘.tiff’ files are saved in the
#' \code{AppRoot} directory. When \code{untarDir} is defined the function untars the images in this folder.
#' This replicates the images in compresses version as tar.gz and uncompresses version. To save space in the disk
#' \code{raw.rm = T} can be defined, and \code{lsDownSearch} will remove the ‘tar.gz’ files.
#' If \code{raw.rm = F}, the original files remain, which might be useful to have access to the original files
#' in the future and avoid further downloads. By default, lsDownSearch saves the images in (...), in the AppRoot
#' directory. To change this setting, provide AppRoot = the full path as an argument.
#'
#' @param searchres the results from \code{ls7Search} or \code{ls8Search}
#' @param username login credentials to access the USGS EROS web service
#' @param password login credentials to access the USGS EROS web service
#' @param cookies.file  File path for saving the cookies that are used in the download process
#' @param verbose Flag for debugging mode
#' @param raw.rm Flag for removing the raw images
#' @param untar Flag for untaring downloaded images
#' @param ... argument for function nestering and/or
#' \itemize{
#'   \item \code{AppRoot}  Root directory where the images will be saved
#' }
#'
#'
#' @examples
#' \dontrun{
#' # Search and download the images from Landsat 8 comprised between
#' # 2011 and 2013 for the region of Navarre
#' data(navarre)
#' search<-ls8Search(startDate=as.Date("01-01-2011","%d-%m-%Y"),
#'                   endDate=as.Date("31-12-2013","%d-%m-%Y"),
#'                   extent=navarre,
#'                   browseAvaliable="Y")
#'
#' #download 1 image
#' lsDownSearch(search[1,],username="user",password="pass",untarDir=T,raw.rm=T)
#' # download 10 images
#' lsDownSearch(search[1:10,],username="user",password="pass",untarDir=T,raw.rm=T)
#' # download all the images
#' lsDownSearch(search,username="user",password="pass",untarDir=T,raw.rm=T)
#'
#' # Search and download the images from Landsat 7 comprised between
#' # 2011 and 2013 for the region of Navarre
#' data(navarre)
#' search<-ls7Search(startDate=as.Date("01-01-2011","%d-%m-%Y"),
#'                   endDate=as.Date("31-12-2013","%d-%m-%Y"),
#'                   extent=navarre,
#'                   browseAvaliable="Y")
#' #download 1 image
#' lsDownSearch(search[1,],username="user",password="pass",untarDir=T,raw.rm=T)
#' #download 10 images
#' lsDownSearch(search[1:10,],username="user",password="pass",untarDir=T,raw.rm=T)
#' #download all the images
#' lsDownSearch(search,username="user",password="pass",untarDir=T,raw.rm=T)
#' }
lsDownSearch<-function(searchres,
                       username=NULL,
                       password=NULL,
                       cookies.file="lscookies.txt",
                       verbose=FALSE,
                       raw.rm=FALSE,
                       untar=FALSE,
                       ...){
  stopifnot(class(searchres)=="data.frame")
  if(is.null(username)|is.null(password)){
    stop("User")
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
        .ls8DownloadUSGS(scene,downPath,handler,verbose=verbose)
      }else if(grepl("LE7",searchres[1,]$sceneID)){
        .ls7DownloadUSGS(scene,downPath,handler,verbose=verbose)
      }
    }
    #Unzip in downDir when available
    if(untar){
      print(paste0("Untar ",scene," file."))
      untarDir<-file.path(AppRoot,downDir,untar,scene)
      dir.create(untarDir,recursive=T)
      untar(paste0(downPath,"/",scene,".tar.gz"),exdir=untarDir)
      #Flag is true, so remove compressed files
      if(raw.rm){
        file.remove(paste0(downPath,"/",scene,".tar.gz"))
      }
    }
  }

  if(untar){
    message(paste0("The images have been downloaded and saved on HHD. \nFile path: ",untarDir))
  }else{
    message(paste0("The images have been downloaded and saved on HHD. \nFile path: ",downPath))
  }

}
