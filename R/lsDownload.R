#' Search and downloads a time series of satellite images from Landsat 7-8
#'
#' \code{lsDownSearch} downloads the list of images provided by \code{ls7Search} and \code{ls8Search} functions.
#' The images are saved as ‘.tiff’ files in the \code{AppRoot} directory.
#'
#' This function is used for searching and downloading landsat images. The function uses landsat
#' search function (\code{\link{ls7Search}} or \code{\link{ls8Search}}) depending on the \code{satellite} argument,
#' and then, calls \code{\link{lsDownload}} function with the response of the search.
#' Image download requires USGS login account. \href{https://ers.cr.usgs.gov/register/}{Get your credentials}.
#'
#' The image files from the USGS EROS web service are compressed as ‘.tar.gz’ files. \code{\link{lsDownload}} decompresses the
#' images and obtains the corresponding ‘.tif’ files. The ‘.tif’ files are saved in the
#' \code{AppRoot} directory. When \code{untarDir} is defined the function untars the images in this folder.
#' This replicates the images in compresses version as tar.gz and uncompresses version. To save space in the disk
#' \code{raw.rm = T} can be defined, and \code{\link{lsDownload}} will remove the ‘tar.gz’ files.
#' If \code{raw.rm = F}, the original files remain, which might be useful to have access to the original files
#' in the future and avoid further downloads. By default, \code{\link{lsDownload}} saves the images in (...), in the \code{AppRoot}
#' directory. To change this setting, provide \code{AppRoot = "the full path as an argument"}.
#'
#' @param satellite string containing the type of satellite for downloading (\code{"ls7"} or \code{"ls8"}).
#' @param startDate starting date of the image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#' @param endDate ending date of the image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#' @param username login credentials to access the USGS EROS web service.
#' @param password login credentials to access the USGS EROS web service.
#' @param verbose logical argument. If \code{TRUE} the function prints running stages and warnings.
#' @param raw.rm logical argument. If \code{TRUE} removes the raw images.
#' @param untar logical argument. If \code{TRUE} untars downloaded images.
#' @param ... argument for function nestering accepts:
#'  \itemize{
#'   \item any argument for function nestering including all \code{\link{ls8Search}/\link{ls7Search}} or \code{\link{lsDownSearch}}
#' function arguments.
#'   \item \code{AppRoot} the directory to save the downloaded images.
#' }
#'
#' @examples
#' \dontrun{
#' data(ex.navarre)
#' lsDownload(satellite = "ls8",
#'            username = "username",
#'            password = "password",
#'            startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'            endDate = as.Date("20-01-2018", "%d-%m-%Y"),
#'            extent = ex.navarre)
#'            
#' # remove metadata data frame to free memory
#' lsRemoveMetadata()
#' }
lsDownload<-function(satellite,
                     startDate,
                     endDate,
                     username,
                     password,
                     verbose=FALSE,
                     untar=TRUE,
                     raw.rm=FALSE,
                     ...){
  AppRoot<-defineAppRoot(...)
  if(tolower(satellite)=="ls7"){
    print("Searching Landsat-7 image time series.")
    searchres=ls7Search(startDate=startDate,
                        endDate=endDate,
                        ...)
  }else if (tolower(satellite)=="ls8"){
    print("Searching Landsat-8 image time series.")
    searchres=ls8Search(startDate=startDate,
                        endDate=endDate,
                        ...)
  }else{
    stop("Satellite not supported. Perform the search with the argument satellite as ls7 or ls8.")
  }
  message("Search result:")
  message(searchres)
  lsDownSearch(searchres=searchres,username=username,password=password,untar=untar,raw.rm=raw.rm,AppRoot=AppRoot,...)
}
