#' Searchs and downloads Sentinel images
#'
#' \code{senDownload} combines both \code{\link{senSearch}} and \code{\link{senDownSearch}} functions for searching and downloading. 
#' It is recommended for creating long time series in an automated way.
#'
#' This function accepts all the arguments in \code{\link{senSearch}} function and automatically downloads
#' images matching with the search query. The function creates a folder hierarchy, and gives the possibility
#' of unzipping the images.
#' \code{senDownload} requires the credentials to access the ESA’s SciHub data service.
#' \href{https://scihub.copernicus.eu/dhus/#/self-registration}{Get your credentials}.
#'
#' @param username Scihub platform username.
#' @param password Scihub platform password.
#' @param ... argument to allow function nestering
#' \itemize{
#'   \item \code{product} Sentinel product type.
#'   \item \code{startDate} starting date of the image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#'   \item \code{endDate} ending date of the image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#'   \item \code{extent} location as project file with extention.
#'   \item \code{platform} platform name of the Sentinel mission (Sentinel-1, Sentinel-2,...).
#'   \item \code{nattempts} the number of attempts that the function has to carry out
#'    an image in case the file becomes corrupted.
#'   \item \code{unzip} logical argument. If \code{TRUE} unzips the images.
#'   \item \code{error.log} error log file name.
#'   \item \code{verbose} logical argument. If \code{TRUE} the function prints running stages and warnings.
#'   \item \code{AppRoot} the directory where the images will be saved.
#' }
#'
#' @examples
#' \dontrun{
#' # Download S2MSI1C products sensed by Sentinel - 2 
#' # satellite between the julian dates 210 and 218, 2018
#' data(ex.navarre)
#' senDownload(startDate = as.Date("2018210", "%Y%j"),
#'             endDate = as.Date("2018218", "%Y%j"),
#'             platform = "Sentinel-2",
#'             extent = ex.navarre,
#'             product = "S2MSI1C",
#'             pathrow = c("R094"),
#'             username = "username",
#'             password = "password")
#' files<-list.files("./",
#'                   pattern = "\\.jp2$",
#'                   full.names = T,
#'                   recursive = T)[4,3,2]
#' files.stack<-stack(files)
#' qrange<-c(0.001,0.999)
#' imagen<-varRGB(files.stack.raster[[1]], 
#'                files.stack.raster[[2]],
#'                files.stack.raster[[3]],
#'                qrange)
#' plotRGB(imagen)
#' }
senDownload<-function(username,
                      password,
                      verbose=FALSE,
                      ...){
  arg<-list(...)

  AppRoot<-defineAppRoot(...)

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

  if(verbose){
    message("Urls for downloading")
    message(paste(names(senURL),collapse="\n"))
  }
  if(length(senURL)==0){stop("There are not images for downloading.")}
  senDownSearch(searchres=senURL,
                username=username,
                password=password,
                AppRoot=AppRoot,
                unzip=T,
                overwrite=TRUE,
                ...)
}



