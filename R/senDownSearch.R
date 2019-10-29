#' Search and download Sentinel images
#'
#' \code{senDownSearch} searches and downloads Sentinel images concerning a
#' particular location and time interval from `SciHub's' repository.  
#' 
#' \code{senDownSearch} is a wrapper function of \code{\link{senSearch}} and 
#' \code{\link{senDownload}} to search and download images in a single step.
#' The function requires ESA’s `SciHub' credentials, which can be obtained
#' \href{https://scihub.copernicus.eu/dhus/#/self-registration}{here}.
#'
#' @param username ESA’s `SciHub' username.
#' @param password ESA’s `SciHub' password.
#' @param AppRoot the directory where the images are saved.
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param ... arguments for nested functions:
#' \itemize{
#'   \item \code{product} the type of Sentinel product. Ex. "S2MSI1C",
#'   "S2MSI2A", "S2MSI2Ap", ... 
#'   \code{startDate} and \code{endDate} are not defined.
#'   \item  \code{startDate} a \code{Date} class object with the starting date of the 
#' study period. This argument is mandatory if 
#'   \code{dates} is not defined.
#'   \item  \code{endDate} a \code{Date} class object with the ending date of the 
#' study period. This argument is mandatory if 
#'   \code{dates} is not defined.
#'   \item \code{dates} a vector with the capturing dates being considered
#'   for searching. This argument is mandatory if 
#'   \code{startDate} and \code{endDate} are not defined.
#'   \item \code{region} a \code{Spatial*}, projected \code{raster*}, or \code{sf*} class object 
#' defining the area of interest.
#'   \item \code{extent} an \code{extent}, \code{Raster*}, or \code{Spatial*}
#'   object representing the region of interest with longitude/latitude
#'   coordinates.
#'   \item \code{platform} the name of the Sentinel mission ("Sentinel-1", 
#'   "Sentinel-2", ...).
#'   \item \code{nattempts} the number of attempts to download an image in case
#'   it becomes corrupted.
#'   \item \code{unzip} logical argument. If \code{TRUE}, unzips the images.
#'   \item \code{verbose} logical argument. If \code{TRUE}, the function prints
#'   the running steps and warnings.
#' }
#' @return this function does not return anything. It saves the imagery as
#' `zip’ (and JP2 files) in a folder called `raw’ (`unzip’) in the
#'  \code{AppRoot} directory.
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # Download S2MSI1C products sensed by Sentinel-2 
#' # between the julian dates 210 and 218, 2018
#' wdir <- paste0(tempdir(),"/Path_for_downloading_folder")
#' print(wdir)
#' senDownload(startDate = as.Date("2018210", "%Y%j"),
#'             endDate = as.Date("2018218", "%Y%j"),
#'             platform = "Sentinel-2",
#'             extent = ex.navarre,
#'             product = "S2MSI1C",
#'             pathrow = c("R094"),
#'             username = "username",
#'             password = "password",
#'             AppRoot = wdir)
#'             
#' wdir.sen <- file.path(wdir, "Sentinel")
#' wdir.sen.unzip <- file.path(wdir.sen, "unzip")
#'                   
#' files.sen.unzip <- list.files(wdir.sen.unzip,
#'                               pattern = "\\TCI.jp2$",
#'                               full.names = TRUE,
#'                               recursive = TRUE)
#' img.sen.rgb <- stack(files.sen.unzip[1])
#' plotRGB(img.sen.rgb)
#' }
senDownSearch<-function(username,
                        password,
                        AppRoot,
                        verbose=FALSE,
                        ...){
  if(missing(username)|missing(password))stop("username or password not defined!")
  arg<-list(...)
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
  senDownload(searchres=senURL,
                username=username,
                password=password,
                AppRoot=AppRoot,
                unzip=TRUE,
                ...)
}



