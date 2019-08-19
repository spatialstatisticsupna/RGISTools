#' Search and download MODIS images
#'
#' \code{modDownload} seeks and downloads MODIS images concerning a particular
#' location and time interval from the EarthExplorer repository. Images are
#' saved as GTiff files in the \code{AppRoot} directory.
#'
#' \code{modDownload} uses the 
#' \href{https://lpdaacsvc.cr.usgs.gov/services/inventory}{NASA’s Common Metadata Repository}
#' to search and the
#' \href{https://earthdata.nasa.gov/}{EarthData web service}
#' to download the imagery. The catalogue of MODIS products can be found
#' \href{https://modis.gsfc.nasa.gov/data/dataprod/}{here}.
#' The catalogue shows detailed information about the products and their short
#' names. By the time RGISTools is released, NASA carries out the maintenance
#' of its website on Wednesdays, which may cause an error when connecting to
#' their server. You can get your EarthData credentials
#' \href{https://urs.earthdata.nasa.gov/users/new}{here}.
#'
#' @param product a \code{character} argument with the short name of the MODIS
#' product.
#' @param startDate a \code{Date} class object with the starting date of the 
#' study period.
#' @param endDate a \code{Date} class object with the ending date of the 
#' study period.
#' @param username NASA's EarthData username.
#' @param password NASA's EarthData password.
#' @param nattempts the number of attempts to download an image in case it
#' becomes corrupted.
#' @param collection MODIS collection, by default 6.
#' @param extract.tif logical argument. If \code{TRUE}, extracts all the layers
#' from hdf files and saves them as GTiff.
#' @param verbose logical argument. If \code{TRUE}, the function prints running stages and warnings.
#' @param ... arguments for nested functions:
#' \itemize{
#'   \item \code{lonlat} a vector with the longitude/latitude
#'   coordinates of the point of interest. This argument is mandatory if
#'   \code{polygon} or \code{extent} are not defined.
#'   \item \code{extent} an \code{extent}, \code{Raster*}, or \code{Spatial*}
#'   object representing the region of interest with longitude/latitude
#'   coordinates. This argument is mandatory if \code{polygon} or \code{lonlat}
#'   are not defined.
#'   \item \code{polygon} A list of vectors defining the points of a polygon in
#'   longitude/latitude format. This argument is mandatory if \code{lonlat} or
#'   \code{extent} are not defined.
#'   \item \code{AppRoot} the directory to save the outcoming time series.
#'   \item Any argument in \code{\link{modExtractHDF}} function. Ex.
#'   \code{bFilter="b01_1"}.
#' }
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' src <- "Path_for_downloading_folder"
#' modDownload(product = "MOD09GA",
#'             startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'             endDate = as.Date("03-01-2018", "%d-%m-%Y"),
#'             username = "username",
#'             password = "password",
#'             AppRoot = src,
#'             extract.tif = TRUE,
#'             collection = 6,
#'             extent = ex.navarre)
#' tif.src <- file.path(src,"Modis","MOD09GA","tif")
#' files <- list.files(tif.src,
#'                     pattern = "\\.tif$",
#'                     full.names = TRUE,
#'                     recursive = TRUE)[c(16,19,18)]
#' files.stack <- stack(files)
#' qrange <- c(0.001, 0.999)
#' imagen <- varRGB(files.stack[[1]], 
#'                  files.stack[[2]],
#'                  files.stack[[3]],
#'                  qrange)
#' plotRGB(imagen)
#'}
modDownload<-function(product,
                     startDate,
                     endDate,
                     username,
                     password,
                     collection=6,
                     nattempts=5,
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
  AppRoot<-file.path(AppRoot,"Modis")
  downdir<-file.path(AppRoot,product,"hdf")
  tiffdir<-file.path(AppRoot,product,"tif")
  if(extract.tif)
    dir.create(tiffdir,recursive=T,showWarnings = F)
  natps<-0
  dir.create(downdir,recursive = T,showWarnings = F)
  for(s in search.res){
    #print(basename(s))
    recursiveModDownload(s=s,
                         username=username,
                         password=password,
                         downdir=downdir,
                         tiffdir=tiffdir,
                         verbose=verbose,
                         extract.tif=extract.tif,
                         nattempts=nattempts,
                         natps=0,
                         ...)
  }
  message(paste0("The images have been downloaded and saved on HDD. \nFile path: ",tiffdir))
}


recursiveModDownload<-function(s,username,password,downdir,tiffdir,verbose,nattempts,extract.tif,natps,...){
  tryCatch(
    {
      modDownSearch(s,username,password,AppRoot=downdir)
      if(extract.tif){
        if(verbose){message(paste0("Extracting ",file.path(downdir,basename(s))," to dir ",tiffdir))}
        modExtractHDF(file.path(downdir,basename(s)),AppRoot=tiffdir,verbose=verbose,...)
      }
    },
    error=function(cond) {
      message(cond)
      file.remove(file.path(downdir,basename(s)))
      if(natps<nattempts){
        message("Error downloading the image, trying again...")
        recursiveModDownload(s=s,
                             username=username,
                             password=password,
                             downdir=downdir,
                             tiffdir=tiffdir,
                             verbose=verbose,
                             nattempts=nattempts,
                             extract.tif=extract.tif,
                             natps=natps+1,
                             ...)
      }else{
        message(paste0("No way for downloading ",basename(s), " image, skipping..."))
      }
    })
}
