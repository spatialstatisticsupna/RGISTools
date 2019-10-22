#' Search and download Landsat-7 or Landsat-8 images
#'
#' \code{lsDownSearch} searches and downloads Landsat-7 or Landsat-8 images
#' concerning a particular location and time interval from the 
#' \href{https://earthexplorer.usgs.gov/}{`EarthExplorer' repository}.
#' Images are saved as GTiff files in the \code{AppRoot} directory.
#'
#' \code{lsDownSearch} is a wrapper function of \code{\link{ls7Search}}, 
#' \code{\link{ls8Search}}, and \code{\link{lsDownload}} to search and
#' download images in a single step. The function requires USGS's `EarthExplorer'
#' credentials, which can be obtained
#' \href{https://ers.cr.usgs.gov/register/}{here}.
#' 
#' The files from `EarthExplorer' are compressed as ‘tar.gz’. \code{lsDownSearch}
#' decompresses the images and obtains the corresponding GTiffs. The GTiffs are
#' saved in the \code{AppRoot} directory. To change this option, provide 
#' \code{AppRoot = “full path”}. When the \code{untarDir} argument is defined,
#' the function untars the imagery in this location. Image decompression
#' duplicates the information due to the presence of both, compressed and 
#' decompressed images. Set \code{raw.rm = TRUE} to remove the former ones.
#' 
#' @param satellite string containing the type of satellite 
#' (\code{"ls7"} or \code{"ls8"}).
#' @param AppRoot the download directory.
#' @param username USGS’s `EarthExplorer' username.
#' @param password USGS’s `EarthExplorer' password.
#' @param lvl a number specifying the processing level. Default value, 1.
#' @param product \code{character} vector with the requested Level-2 products.
#' By default \code{c("sr", "source_metadata")}.
#' @param verbose logical argument. If \code{TRUE}, the function prints the
#' running steps and warnings.
#' @param raw.rm logical argument. If \code{TRUE}, removes the raw images.
#' @param untar logical argument. If \code{TRUE}, untars downloaded images.
#' @param ... argumetns for nested functions:
#'  \itemize{
#'   \item \code{dates} a vector with the capturing dates being considered
#'   for searching. This argument is mandatory if 
#'   \code{startDate} and \code{endDate} are not defined.
#'   \item  \code{startDate} a \code{Date} class object with the starting date of the 
#' study period. This argument is mandatory if 
#'   \code{dates} is not defined.
#'   \item  \code{endDate} a \code{Date} class object with the ending date of the 
#' study period. This argument is mandatory if 
#'   \code{dates} is not defined.
#'   \item \code{region} a \code{Spatial*}, projected \code{raster*}, or \code{sf*} class object 
#' defining the area of interest.
#'   \item any argument for \code{\link{ls8Search}}/\code{\link{ls7Search}} or 
#'   \code{\link{lsDownload}}.
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
#' src <- paste0(tempdir(),"/Path_for_downloading_folder")
#' print(src)
#' # search and download the images from Landsat-8 between
#' # 01-01-2018 and 20-01-2018 for the region of Navarre
#' lsDownSearch(satellite = "ls8",
#'              username = "username",
#'              password = "password",
#'              startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'              endDate = as.Date("20-01-2018", "%d-%m-%Y"),
#'              extent = ex.navarre,
#'              AppRoot = src)
#'            
#' # remove metadata to free memory space
#' lsRemoveMetadata()
#' }
lsDownSearch<-function(satellite,
                     username,
                     password,
                     AppRoot,
                     lvl=1,
                     product=c("sr","source_metadata"),
                     verbose=FALSE,
                     untar=TRUE,
                     raw.rm=FALSE,
                     ...){
  if(missing(username)|missing(password))stop("username or password not defined!")
  if(tolower(satellite)=="ls7"){
    message("Searching Landsat-7 image time series.")
    searchres=ls7Search(verbose=verbose,
                        AppRoot=AppRoot,
                        ...)
  }else if (tolower(satellite)=="ls8"){
    message("Searching Landsat-8 image time series.")
    searchres=ls8Search(verbose=verbose,
                        AppRoot=AppRoot,
                        ...)
  }else{
    stop("Satellite not supported. Perform the search with the argument satellite as 'ls7' or 'ls8'.")
  }
  if(verbose){
    message("Search result:")
    message(searchres)
  }
  lsDownload(searchres=searchres,
             username=username,
             password=password,
             untar=untar,
             raw.rm=raw.rm,
             AppRoot=AppRoot,
             lvl=lvl,
             product=product,
             ...)
}
