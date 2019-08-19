#' Search MODIS images
#'
#' \code{modSearch} seeks MODIS images in the 
#' \href{https://lpdaacsvc.cr.usgs.gov/services/inventory}{NASA Common Metadata Repository}
#' concerning a particular location and date interval. The function returns an 
#' \code{array} with the names of the images and their URLs.
#'
#' \code{modSearch} uses the
#' \href{https://lpdaacsvc.cr.usgs.gov/services/inventory}{NASA Common Metadata
#' Repository} (CMR) powered API. The catalogue of MODIS products can be found
#' \href{https://modis.gsfc.nasa.gov/data/dataprod/}{here}.
#' The catalogue shows the product short names and provides detailed information
#' about the product. By the time RGISTools is released, NASA carries out the
#' maintenance of its website on Wednesdays, which may cause an error when
#' connecting to their server. You can get your EarthData credentials
#' \href{https://urs.earthdata.nasa.gov/users/new}{here}.
#' 
#' The function can be used to retrieve the web address of the preview 
#' (\code{resType = "browseurl"}) or the actual image (\code{resType = "url"}).
#' By default, the URL points towards the actual image.
#'
#' @param product the short name of the MODIS product.
#' @param startDate  a \code{Date} class object with the starting date of the 
#' study period.
#' @param endDate a \code{Date} class object with the ending date of the 
#' study period.
#' @param collection MODIS collection. By default, 6.
#' @param resType response type of the query (\code{browseurl} or \code{url}).
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
# @param pathrow A list of vectors defining the path and row number for the region of interest according
# to the Sinusoidal Tile Grid (\url{https://modis-land.gsfc.nasa.gov/MODLAND_grid.html})
# This argument is mandatory if extent is not defined.
#' @param ... arguments for nested functions:
#' \itemize{
#'   \item \code{lonlat} a vector with the longitude/latitude
#'   coordinates of the point of interest. This argument is mandatory if 
#'   \code{polygon} or \code{extent} are not defined.
#'   \item \code{extent}  an \code{extent}, \code{Raster*}, or 
#'   \code{Spatial*} object representing the region of interest with 
#'   longitude/latitude coordinates. This argument is mandatory if 
#'   \code{polygon} or \code{lonlat} are not defined.
#'   \item \code{polygon} a list of vectors defining the points of a polygon
#'   with longitude/latitude coordinates. This argument is mandatory if
#'   \code{lonlat} or \code{extent} are not defined.
#'   \item \code{AppRoot} the directory to save the outcoming time series.
#' }
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre with longitude/latitude coordinates
#' data(ex.navarre)
#' # searching MODIS MYD13A2 images between 2011 and 2013 by longitude/latitude
#' # using a polygon class variable
#' img.list <- modSearch(product = "MYD13A2",
#'                       startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                       endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                       collection = 6,
#'                       extent = ex.navarre)
#' # region of interest: defined based on longitude/latitude extent
#' # searching MODIS MYD13A2 images in 2010 by longitude/latitude
#' # using a extent class variable defined by the user
#' aoi = extent(c(-2.49, -0.72, 41.91, 43.31))
#' my.imgs <- modSearch(product = "MYD13A2",
#'                      startDate = as.Date("01-01-2010", "%d-%m-%Y"),
#'                      endDate = as.Date("31-12-2010", "%d-%m-%Y"),
#'                      collection = 6,
#'                      extent = aoi)
#' head(my.imgs)
#' }
modSearch<-function(product,startDate,endDate,collection=6,resType="url",verbose=FALSE,...){
  stopifnot(class(startDate)=="Date")
  stopifnot(class(endDate)=="Date")
  arg=list(...)
  if(any(names(arg)%in%c("pathrow"))){
    stopifnot(class(arg$pathrow)=="list")
    stop("pathrow search not supported for Modis Search")
  }else if("lonlat"%in%names(arg)){
    stopifnot(class(arg$lonlat)=="numeric")
    stopifnot(length(arg$lonlat)==2)
    loc<-paste0(getRGISToolsOpt("MODINVENTORY.url"),
                "?product=",product,
                "&version=",collection,
                "&latitude=",arg$lonlat[2],
                "&longitude=",arg$lonlat[1],
                "&return=",resType,
                "&date=",format(startDate,"%Y-%m-%d"),
                ",",format(endDate,"%Y-%m-%d"))
  }else if("extent"%in%names(arg)){
    stopifnot(class(extent(arg$extent))=="Extent")
    loc<-paste0(getRGISToolsOpt("MODINVENTORY.url"),
                "?product=",product,
                "&version=",collection,
                "&bbox=",paste0(c(bbox(arg$extent)),collapse = ","),
                "&return=",resType,
                "&date=",format(startDate,"%Y-%m-%d"),
                ",",format(endDate,"%Y-%m-%d"))
  }else if("polygon"%in%names(arg)){
    #arg$polygon<-list(c(1,2),c(3,4))
    pts<-paste(arg$polygon[[1]][2])
    pts<-paste(pts,arg$polygon[[1]][1],sep = ",")
    for(x in 2:length(arg$polygon)){
      pts<-paste(pts,arg$polygon[[x]][2],sep = ",")
      pts<-paste(pts,arg$polygon[[x]][1],sep = ",")
    }
    loc<-paste0(getRGISToolsOpt("MODINVENTORY.url"),
                "?product=",product,
                "&version=",collection,
                "&polygon=",pts,
                "&return=",resType,
                "&date=",format(startDate,"%Y-%m-%d"),
                ",",format(endDate,"%Y-%m-%d"))
  }else{
    stop('Search method not defined.')
  }

  if(verbose){
    message(paste0("Search query: ",loc))
  }
  c.handle = new_handle()
  req <- curl(loc, handle = c.handle)
  html<-readLines(req)
  html<-paste(html,collapse = "\n ")

  if(grepl("Internal Server Error", html)){
    stop(paste0("Error: ",getRGISToolsOpt("MODINVENTORY.url")," web out of service"))
  }
  xmlres <- xmlRoot(xmlNativeTreeParse(html))
  modisres <- xmlSApply(xmlres,
                        function(x) xmlSApply(x,xmlValue))
  close(req)
  return(modisres)
}

