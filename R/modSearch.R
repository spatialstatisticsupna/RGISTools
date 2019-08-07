#' Search Modis Images on the NASA Common Metadata Repository
#'
#' \code{modSearch} searches the Modis products on the  
#' \href{https://lpdaacsvc.cr.usgs.gov/services/inventory}{NASA Common Metadata Repository} to 
#'  find the available images for a particular location and date interval.
#' The outut is an array of urls with the relevant images.
#'
#' \code{modSearch} \href{https://lpdaacsvc.cr.usgs.gov/services/inventory}{NASA Common Metadata Repository} (CMR) powered api.
#' The catalogue of Modis products with their short names and other
#' information can be found at: \href{https://modis.gsfc.nasa.gov/data/dataprod/}{Modis data product info}.
#' For further information on collections, please visit \href{https://modis-atmos.gsfc.nasa.gov/collections/overview}{Modis website}.
#' By the time the \code{RGISTools} package is released, NASA carries out the maintenance of the Modis website on Wednesdays. Therefore, an error
#' may occur when trying to connect with their server during this day of the week.
#'
#' @param product the short name of the Modis product.
#' @param startDate starting date of the image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#' @param endDate ending date of the image time series in \code{Date} class. For instance, using any format from \code{as.Date} function.
#' @param collection Modis collection.
#' @param resType response type of the query (\code{browseurl} or \code{url}), by default the url where the images are located.
#' @param verbose logical argument. If \code{TRUE}, the function prints running stages and warnings.
# @param pathrow A list of vectors defining the path and row number for the region of interest according
# to the Sinusoidal Tile Grid (\url{https://modis-land.gsfc.nasa.gov/MODLAND_grid.html})
# This argument is mandatory if extent is not defined.
#' @param ... argument for function nestering:
#' \itemize{
#'   \item \code{lonlat} a vector or a polygon with the coordinates of
#' the point or region of interest in longitude/latitude format.
#' This argument is mandatory if polygon or extent is not defined.
#'   \item \code{extent} \code{Extent}, \code{Raster*}, \code{SpatialPolygons*}, \code{SpatialLines*} or \code{SpatialPoints*} object are acceptable formats
#' as long as coordinates are in longitude/latitude format.
#' This argument is mandatory if \code{polygon} or \code{lonlat} is not defined.
#'   \item \code{polygon} a list of vectors defining the points of the polygon in longitude/latitude coordinates.
#' This argument is mandatory if \code{lonlat} or \code{extent} is not defined.
#'   \item \code{AppRoot} the directory to save the output time series.
#' }
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre in longitude/latitude coordinates
#' data(ex.navarre)
#' # Searching Modis MYD13A2 images between 2011 and 2013 by longitude/latitude
#' # using a polygon class variable
#' img.list <- modSearch(product = "MYD13A2",
#'                       startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                       endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                       collection = 6,
#'                       extent = ex.navarre)
#' # Area of interest: defined based on longitude-latitude extent
#' # Searching Modis MYD13A2 images in 2010 by longitude/latitude
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

