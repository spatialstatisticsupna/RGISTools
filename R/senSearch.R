#' Searches Sentinel images using ESA’s SciHub API
#'
#' \code{senSearch} searches Sentinel-2 products on ESA powered api called \href{http://scihub.copernicus.eu}{Scihub}.
#'
#' Provides the urls for downloading the images from ESA's \href{http://scihub.copernicus.eu}{Scihub} API. The images are
#' searched within a range of dates and a region of interest. Dates have to be
#' provided as a date class object. The region of interest should be provided as a
#' spatial class object with an "\code{EPSG:4326}" coordinate projection. 
#' Credentials from ESA’s SciHub are needed to use this function.
#' \href{https://scihub.copernicus.eu/dhus/#/self-registration}{Get your credentials}.
#'
#' For further information on ESA’s missions and data products, please visit
#' \href{https://sentinel.esa.int/web/sentinel/missions}{Sentinel mission web page}.
#'
#' @param username Scihub username.
#' @param password Scihub password.
#' @param ... argument to allow function nestering:
#' \itemize{
#'   \item \code{product} Sentinel product type.  Ex. "S2MSI1C", "S2MSI2A", "S2MSI2Ap", ... 
#'   \item \code{startDate} starting date of the image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#'   \item \code{endDate} ending date of the image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#'   \item \code{extent} location as projecte file with extention.
#'   \item \code{lonlat} a vector or a polygon with the coordinates of
#' the point or region of interest in longitude/latitude format.
#'   \item \code{platform} platform name of the Sentinel mission (Sentinel-1, Sentinel-2,...).
#'   \item \code{qformat} format of response
#'   \item \code{verbose} logical argument. If \code{TRUE} the function prints running stages and warnings.
#'   \item \code{error.log} the name of the error log file.
#'   \item \code{AppRoot} the directory where the images will be saved.
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
    print(paste0("Search Total result: ",json$feed$`opensearch:totalResults`))
  cont=1

  if(as.integer(json$feed$`opensearch:totalResults`)>0){
    imgNames<-c()
    imgURL<-c()
    for(i in json$feed$entry){
      if(arg$verbose){
        print(paste0("Image result ",cont," Name:",i$title))
        print(paste0("Image result ",cont," Url:",i$link[[1]]$href))#each entry have 3 links: 1-image link, 2-meta data link, 3-quicklook mini image
      }
      imgNames<-c(imgNames,i$title)
      imgURL<-c(imgURL,i$link[[1]]$href)
      cont<-cont+1
    }
    if(arg$verbose)
      print(paste0("Results added to the list: ",cont))
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
      print(paste0("New end date: ",mn.date))
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
