#' Search and download Sentinel images
#'
#' \code{senDownload} combines both \code{\link{senSearch}} and \code{\link{senDownSearch}} functions for searching and downloading. 
#' It is recommended for creating long time series of images in an automated way.
#'
#' This function accepts all the arguments in \code{\link{senSearch}} function and automatically downloads
#' images matching with the search query. The function creates a folder hierarchy, and gives the possibility
#' of unzipping the images.
#' \code{senDownload} requires the credentials to access the ESA’s SciHub data service.
#' \href{https://scihub.copernicus.eu/dhus/#/self-registration}{Get your credentials}.
#'
#' @param username Scihub platform username.
#' @param password Scihub platform password.
#' @param verbose logical argument. If \code{TRUE}, the function prints running stages and warnings.
#' @param ... argument forfunction nestering
#' \itemize{
#'   \item \code{product} Sentinel product type. Ex. "S2MSI1C", "S2MSI2A", "S2MSI2Ap", ... 
#'   \item \code{startDate} starting date of the image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#'   \item \code{endDate} ending date of the image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#'   \item \code{extent} \code{Extent}, \code{Raster*}, \code{SpatialPolygons*}, \code{SpatialLines*} or 
#'   \code{SpatialPoints*} object of the region of interest in longitude/latitude coordinate system.
#'   \item \code{platform} platform name of the Sentinel mission (Sentinel-1, Sentinel-2,...).
#'   \item \code{nattempts} the number of attempts that the function has to carry out
#'    an image in case of corrupted files.
#'   \item \code{unzip} logical argument. If \code{TRUE}, unzips the images.
#'   \item \code{error.log} error log file name.
#'   \item \code{verbose} logical argument. If \code{TRUE}, the function prints running stages and warnings.
#'   \item \code{AppRoot} the directory where the images will be saved.
#' }
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # Download S2MSI1C products sensed by Sentinel - 2 
#' # satellite between the julian dates 210 and 218, 2018
#' src <- "Path_for_downloading_folder"
#' senDownload(startDate = as.Date("2018210", "%Y%j"),
#'             endDate = as.Date("2018218", "%Y%j"),
#'             platform = "Sentinel-2",
#'             extent = ex.navarre,
#'             product = "S2MSI1C",
#'             pathrow = c("R094"),
#'             username = "username",
#'             password = "password",
#'             AppRoot = src)
#'             
#' src.sen <- file.path(src, "Sentinel-2")
#' src.sen.unzip <- file.path(src.sen, "unzip")
#'                   
#' files <- list.files(src.unzip,
#'                     pattern = "\\TCI.jp2$",
#'                     full.names = TRUE,
#'                     recursive = TRUE)
#' rgb <- stack(files[1])
#' plotRGB(rgb)
#' }
senDownload<-function(username,
                      password,
                      verbose=FALSE,
                      ...){
  arg<-list(...)

  AppRoot<-defineAppRoot(...)
  if("platform"%in%names(arg)){
    AppRoot<-file.path(AppRoot,arg$platform)
  }else if("product"%in%names(arg)){
    if(substr(arg$product,1,2)=="S2"){
      AppRoot<-file.path(AppRoot,"Sentinel-2")
    }
  }
  
  senURL<-senSearch(username=username,
                    password=password,
                    ...)
  
  if(verbose){
    message("Urls before filters.")
    message(paste(names(senURL),collapse="\n"))
  }
  if(!is.null(arg$pathrow)){
      #senURL<-senURL[grepl(arg$filter,names(senURL))]
    senURL<-senURL[Reduce("|",lapply(arg$pathrow,grepl,names(senURL)))]
  }
  if(!is.null(arg$senbox)){
    senURL<-senURL[Reduce("|",lapply(arg$senbox,grepl,names(senURL)))]
  }

  message(paste0(length(senURL)," tiles found! Starting the download process..."))
  
  if(length(senURL)==0){stop("There are not images for downloading.")}
  senDownSearch(searchres=senURL,
                username=username,
                password=password,
                AppRoot=AppRoot,
                unzip=TRUE,
                ...)
}



