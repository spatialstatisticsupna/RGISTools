#' Search and downloads Modis images in structured folder
#'
#' \code{modDownload} searches and downloads the MODIS products on the  NASA Common Metadata
#' Repository to find those which are relevant for a particular location and date interval.
#'
#' \code{modDownload} NASA Common Metadata Repository (CMR) powered api \url{https://lpdaacsvc.cr.usgs.gov/services/inventory}
#' The catalogue of  MODIS products with their short names and other
#' information can be found at: \url{https://modis.gsfc.nasa.gov/data/dataprod/}
#' For further information on collections, please visit MODIS website at: \url{https://modis-atmos.gsfc.nasa.gov/collections/overview}
#' By the time the RGISTools package is released, NASA carries out the maintenance of the MODIS website on Wednesdays. Therefore, an error
#' may occur when trying to connect with their server during this day of the week.
#'
#' @param product modis product type
#' @param startDate Start date of time series
#' @param endDate End date of time series
#' @param username EarthData username
#' @param password EarthData password
#' @param collection Modis collection
#' @param hdfdir folder name for hdf download
#' @param tiffdir folder name for tif format images
#' @param verbose Flag for printing debugging information
#' @param ... argument for function nestering
#' \itemize{
#'   \item \code{AppRoot} Aplication environment root path
#'   \item \code{latlon} Vectorial projected file
#' }
#'
#' @examples
#' \dontrun{
#' data(ex.navarre)
#' modDownload(product="MOD09GA",
#'            startDate=as.Date("01-01-2018","%d-%m-%Y"),
#'            endDate=as.Date("03-01-2018","%d-%m-%Y"),
#'            username="username",
#'            password="password",
#'            AppRoot="Path_for_downloading_folder",
#'            hdfdir="hdf",
#'            tiffdir="tif",
#'            collection=6,
#'            extent=ex.navarre)
#'}
modDownload<-function(product,
                     startDate,
                     endDate,
                     username,
                     password,
                     collection=6,
                     verbose=F,
                     hdfdir="hdf",
                     tiffdir="tif",
                     ...){
  arg<-list(...)
  AppRoot<-defineAppRoot(...)
  search<-modSearch(product=product,
                    startDate=startDate,
                    endDate=endDate,
                    collection=collection,
                    ...)
  if(verbose){
    print(search)
  }
  downdir<-file.path(AppRoot,product,hdfdir)
  tiffdir<-file.path(AppRoot,product,tiffdir)
  dir.create(downdir,recursive = T,showWarnings = F)
  for(s in search){
    print(basename(s))
    modDownSearch(s,username,password,AppRoot=downdir)
    dir.create(tiffdir,recursive=T,showWarnings = F)
    modExtractHDF(file.path(downdir,basename(s)),AppRoot=tiffdir)
  }
  message(paste0("The images have been downloaded and saved on HHD. \nFile path: ",tiffdir))
}
