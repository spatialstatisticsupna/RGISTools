#' Search and downloads a time series of satellite images from Landsat 7-8
#'
#' \code{lsDownSearch} downloads the list of images provided by \code{ls7Search} and \code{ls8Search} functions.
#' The images are saved as ‘.tiff’ files in the AppRoot directory..
#'
#' This function is used for searching and downloading landsat images. The function uses landsat
#' search function (\code{\link{ls7Search}} or \code{\link{ls8Search}}) depending on the \code{satellite} argument,
#' and then, calls \code{lsDownload} function with the response of the search.
#' Image download requires USGS login account from \url{https://ers.cr.usgs.gov/register/}.
#'
#' The image files from the USGS EROS web service are compressed as ‘.tar.gz’ files. lsDownload decompresses the
#' images and obtains the corresponding ‘.tif’ files. The ‘.tif’ files are saved in the
#' \code{AppRoot} directory. When \code{untarDir} is defined the function untars the images in this folder.
#' This replicates the images in compresses version as tar.gz and uncompresses version. To save space in the disk
#' \code{raw.rm = T} can be defined, and \code{lsDownload} will remove the ‘tar.gz’ files.
#' If \code{raw.rm = F}, the original files remain, which might be useful to have access to the original files
#' in the future and avoid further downloads. By default, lsDownload saves the images in (...), in the AppRoot
#' directory. To change this setting, provide AppRoot = the full path as an argument.
#'
#' @param satellite string containing the type of satellite for downloading (\code{"ls7"} or \code{"ls8"})
#' @param startDate starting date of the time series for search images
#' @param endDate ending date of the time series for search images
#' @param username login credentials to access the USGS EROS web service
#' @param password login credentials to access the USGS EROS web service
#' @param verbose Flag for debugging mode
#' @param raw.rm Flag for removing the raw images
#' @param untar Flag for untaring downloaded images
#' @param ... any argument for function nestering including all \code{ls8Search/ls7Search} or \code{lsDownSearch}
#' function arguments
#'
#' @examples
#' \dontrun{
#' data(navarre)
#' search<-lsDownload(satellite="ls8",
#'                    username="rgistools",
#'                    password="EspacialUPNA88",
#'                    startDate=as.Date("01-01-2018","%d-%m-%Y"),
#'                    endDate=as.Date("20-01-2018","%d-%m-%Y"),
#'                    extent=navarre)
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
    print("Searching Landsat 7 image time series.")
    searchres=ls7Search(startDate=startDate,
                        endDate=endDate,
                        ...)
  }else if (tolower(satellite)=="ls8"){
    print("Searching Landsat 8 image time series.")
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
