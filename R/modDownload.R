#' Search and downloads Modis images in structured folder
#'
#' \code{modDownload} searches and downloads the Modis products on the  NASA Common Metadata
#' Repository to find those which are relevant for a particular location and date interval.
#'
#' \code{modDownload} \href{https://lpdaacsvc.cr.usgs.gov/services/inventory}{NASA Common Metadata Repository} (CMR) powered api.
#' The catalogue of  Modis products with their short names and other
#' information can be found at: \href{https://modis.gsfc.nasa.gov/data/dataprod/}{Modis data product info}.
#' For further information on collections, please visit \href{https://modis-atmos.gsfc.nasa.gov/collections/overview}{Modis website}.
#' By the time the \code{RGISTools} package is released, NASA carries out the maintenance of the Modis website on Wednesdays. Therefore, an error
#' may occur when trying to connect with their server during this day of the week.
#'
#' @param product Modis product type.
#' @param startDate starting date of time series.
#' @param endDate ending date of time series.
#' @param username EarthData username.
#' @param password EarthData password.
#' @param collection Modis collection.
#' @param hdfdir folder name for downloading the hdf files.
#' @param tiffdir folder name for downloading the tif files.
#' @param verbose logical argument. If \code{TRUE} the function prints running stages and warnings.
#' @param ... argument for function nestering:
#' \itemize{
#'   \item \code{lonlat} A vector or a polygon with the coordinates of
#' the point or region of interest in longitude/latitude format.
#' This argument is mandatory if polygon or extent is not defined.
#'   \item \code{extent} \code{Extent}, \code{Raster*}, \code{SpatialPolygons*}, \code{SpatialLines*} or 
#'   \code{SpatialPoints*} object are acceptable formats as long as coordinates 
#'   are in longitude/latitude format. This argument is mandatory if \code{polygon} 
#'   or \code{lonlat} is not defined.
#'   \item \code{polygon} A list of vectors defining the points of the polygon in longitude/latitude format
#' This argument is mandatory if \code{lonlat} or extent is not defined.
#'   \item \code{AppRoot} the directory to save the resulting time series
#' }
#'
#' @examples
#' \dontrun{
#' data(ex.navarre)
#' modDownload(product = "MOD09GA",
#'             startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'             endDate = as.Date("03-01-2018", "%d-%m-%Y"),
#'             username = "username",
#'             password = "password",
#'             AppRoot = "Path_for_downloading_folder",
#'             hdfdir = "hdf",
#'             tiffdir = "tif",
#'             collection = 6,
#'             extent = ex.navarre)
#'}
modDownload<-function(product,
                     startDate,
                     endDate,
                     username,
                     password,
                     collection=6,
                     verbose=FALSE,
                     hdfdir="hdf",
                     tiffdir="tif",
                     ...){
  arg<-list(...)
  AppRoot<-defineAppRoot(...)
  search.res<-modSearch(product=product,
                    startDate=startDate,
                    endDate=endDate,
                    collection=collection,
                    ...)
  if(verbose){
    print(search.res)
  }
  downdir<-file.path(AppRoot,product,hdfdir)
  tiffdir<-file.path(AppRoot,product,tiffdir)
  dir.create(downdir,recursive = T,showWarnings = F)
  for(s in search.res){
    print(basename(s))
    modDownSearch(s,username,password,AppRoot=downdir)
    dir.create(tiffdir,recursive=T,showWarnings = F)
    modExtractHDF(file.path(downdir,basename(s)),AppRoot=tiffdir)
  }
  message(paste0("The images have been downloaded and saved on HDD. \nFile path: ",tiffdir))
}
