#' Search Sentinel images
#'
#' \code{senSearch} seeks Sentinel images through ESA's powered application
#' programming interface (API), called 
#' \href{http://scihub.copernicus.eu}{`SciHub'}, that concern a particular
#' location and date interval. The function returns a \code{data.frame} with
#' the names of the images and their uniform resource locators (URLs).
#'
#' \code{senSearch} uses the
#' \href{http://scihub.copernicus.eu}{ESA's powered API} (`SciHub').The catalogue
#' of Sentinel-2 products can be found
#' \href{https://sentinel.esa.int/web/sentinel/missions/sentinel-2/data-products}{here}.
#' Images are searched within a range of dates and a region of interest. Dates
#' must be provided as a \code{Date} class object. Credentials from ESA’s `SciHub'
#' are needed and they can be obtained 
#' \href{https://scihub.copernicus.eu/dhus/#/self-registration}{here}.
#' 
#' @param username ESA’s `SciHub' username.
#' @param password ESA’s `SciHub' password.
#' @param ... arguments for nested functions:
#' \itemize{
#'   \item \code{product} the type of Sentinel product.  Ex. "S2MSI1C",
#'   "S2MSI2A", "S2MSI2Ap", ...
#'   \item \code{startDate} a \code{Date} class object with the starting date of
#'   the study period.
#'   \item \code{endDate} a \code{Date} class object with the ending date of
#'    the study period.
#'   \item \code{extent} an \code{extent}, \code{Raster*}, or \code{Spatial*}
#'   object representing the region of interest with longitude/latitude
#'   coordinates.
#'   \item \code{lonlat} a vector with the longitude/latitude
#'   coordinates of the point of interest.
#'   \item \code{platform} the name of the Sentinel mission ("Sentinel-1", 
#'   "Sentinel-2", ...).
#'   \item \code{qformat} the format of the response.
#'   \item \code{verbose} logical argument. If \code{TRUE}, the function prints
#'   the running steps and warnings.
#'   \item \code{error.log} the name of the error log file.
#' }
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # perform the search query
#' searchres <- senSearch(startDate = as.Date("2018210", "%Y%j"),
#'                        endDate = as.Date("2018218", "%Y%j"),
#'                        platform = "Sentinel-2",
#'                        extent = ex.navarre,
#'                        product = "S2MSI1C",
#'                        username = "username",
#'                        password = "password")
#' head(searchres)
#' }
senSearch<-function(username,
                    password,
                    ...){
  arg<-list(...)
  if(!"verbose"%in%names(arg)){
    arg$verbose=FALSE
  }
  query.url<-senSearchQuery(...)

  if(arg$verbose)
    message(query.url)
  c.handle = new_handle()

  handle_setopt(c.handle,
                referer=getRGISToolsOpt("SCIHUBHUSURL"),
                useragent = getRGISToolsOpt("USERAGENT"),
                followlocation = TRUE ,
                autoreferer = TRUE ,
                username=username,
                password=password)
  response=curl(query.url,handle =c.handle)
  tryCatch({
    html<-suppressWarnings(readLines(response))
  }, error = function(e) {
    if(grepl("HTTP error 503.",e$message)){
      stop("Service on maintenace. HTTP error 503.")
    }
    stop(e)
  })

  json <- fromJSON(paste0(html))
  if(arg$verbose)
    message(paste0("Search Total result: ",json$feed$`opensearch:totalResults`))
  cont=1

  if(as.integer(json$feed$`opensearch:totalResults`)>0){
    imgNames<-c()
    imgURL<-c()
    for(i in json$feed$entry){
      if(arg$verbose){
        message(paste0("Image result ",cont," Name:",i$title))
        message(paste0("Image result ",cont," Url:",i$link[[1]]$href))#each entry have 3 links: 1-image link, 2-meta data link, 3-quicklook mini image
      }
      imgNames<-c(imgNames,i$title)
      imgURL<-c(imgURL,i$link[[1]]$href)
      cont<-cont+1
    }
    if(arg$verbose)
      message(paste0("Results added to the list: ",cont))
  }else{
    message("There is no images in response.")
    return(NULL)
  }
  names(imgURL)<-imgNames
  close(response)
  #recursively perform search to get all results search results
  if(as.integer(json$feed$`opensearch:totalResults`)>100){
    dt<-senGetDates(imgNames)
    mn.date<-min(dt)
    if(arg$verbose){
      message(paste0("New end date: ",mn.date))
    }
    if(sum(dt==mn.date)==100){
      stop("\nSpatial regions composed by 100 or more tiles are not supported!
           Try the search with a smaller spatial region.")
    }
    if(sum(dt==mn.date)>50){
      mn.date=mn.date-1
    }
    rURLs<-senSearch(username=username,
                     password=password,
                     #startDate=max(senGetDates(imgNames)),
                     endDate=mn.date,
                      ...)
    imgURL<-c(imgURL,rURLs)
    imgURL<-imgURL[!duplicated(imgURL)]
  }
  return(imgURL)
}
