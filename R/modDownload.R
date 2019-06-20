#' Search and downloads Modis images in structured folder
#'
#' \code{modDownload} searches and downloads the Modis products on the 
#' \href{https://lpdaacsvc.cr.usgs.gov/services/inventory}{NASA Common Metadata Repository} to find images 
#' within a particular location and date interval.
#'
#' \code{modDownload} uses \href{https://lpdaacsvc.cr.usgs.gov/services/inventory}{NASA Common Metadata Repository} (CMR) 
#' powered api to search satellite all ground products releses by NASA. The catalogue of Modis ground products with their 
#' short names and other information can be found at: \href{https://modis.gsfc.nasa.gov/data/dataprod/}{Modis data product info}.
#' For further information on collections, please visit \href{https://modis-atmos.gsfc.nasa.gov/collections/overview}{Modis website}.
#' By the time the \code{RGISTools} package is released, NASA carries out the maintenance of the Modis website on Wednesdays. 
#' Therefore, an error may occur when trying to connect with their server during this day of the week.
#' \href{https://urs.earthdata.nasa.gov/users/new}{Get your credentials}.
#'
#' @param product a \code{character} argument with the name of Modis product type.
#' @param startDate starting date of the image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#' @param endDate ending date of the image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#' @param username EarthData username.
#' @param password EarthData password.
#' @param collection Modis collection.
#' @param extract.tif logical argument. If \code{TRUE} extracts as tif image format all the layers in a hdf image.
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
#' files <- list.files("./Path_for_downloading_folder/tif",
#'                     pattern = "\\.tif$",
#'                     full.names = TRUE,
#'                     recursive = TRUE)[1,4,3]
#' files.stack <- stack(files)
#' qrange <- c(0.001, 0.999)
#' imagen <- varRGB(files.stack.raster[[1]], 
#'                  files.stack.raster[[2]],
#'                  files.stack.raster[[3]],
#'                  qrange)
#' plotRGB(imagen)
#'}
modDownload<-function(product,
                     startDate,
                     endDate,
                     username,
                     password,
                     collection=6,
                     verbose=FALSE,
                     extract.tif=FALSE,
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
  downdir<-file.path(AppRoot,product,"hdf")
  tiffdir<-file.path(AppRoot,product,"tif")
  dir.create(downdir,recursive = T,showWarnings = F)
  for(s in search.res){
    print(basename(s))
    modDownSearch(s,username,password,AppRoot=downdir)
    if(extract.tif){
      dir.create(tiffdir,recursive=T,showWarnings = F)
      modExtractHDF(file.path(downdir,basename(s)),AppRoot=tiffdir)
    }
  }
  message(paste0("The images have been downloaded and saved on HDD. \nFile path: ",tiffdir))
}
