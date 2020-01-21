#' Search Landsat 7-8 images
#'
#' \code{ls7Search} searches Landsat-7 images in the Landsat repository concerning
#' a particular location and date interval. The function returns a 
#' \code{data.frame} with the names of the images and their metadata.
#'
#' \code{ls7Search} searches images in the metadata file. If the metadata was
#' downloaded before to the current directory, \code{ls7Search} will use this
#' metadata by default. In case the metadata was not downloaded before, 
#' \code{ls7Search} will make that call for you. The function creates the
#' following subfolders "Landsat-8/metadata", where the metadata file is
#' located.
#'
#' Landsat images are organized by tiles, which have a unique path and row
#' numbers according to the
#' \href{https://landsat.gsfc.nasa.gov/the-worldwide-reference-system/}{Worldide Reference System}.
#' The fastest way to search an image in the metadata file is by path and row
#' (\code{pathrow}). This method requires to know in advance the path and row
#' numbers of the tile that is relevant for your region of interest. From the
#' user's standpoint, the simplest way to search a time series of Landsat-7
#' images is by \code{region}, \code{extent}, or \code{lonlat}, since they do 
#' not require any prior knowledge about the tiles.
#'
#' The function can screen the results by any other attribute in the metadata.
#' For instance, to filter the imagery with an available preview, the 
#' \code{browseAvaliable=”Y”} must be added as an argument of the function
#' (see the examples).
#'
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param precise logical argument. If \code{TRUE}, conducts a thorough search,
#' tile by tile (slower).
#' @param AppRoot directory of the metadata file. 
#' @param ... arguments for nested functions:
#'  \itemize{
#'   \item \code{dates} a vector with the capturing dates being searched. This
#'   argument is mandatory if \code{startDate} and \code{endDate} are not defined.
#'   \item  \code{startDate} a \code{Date} class object with the starting date of the 
#' study period. This argument is mandatory if 
#'   \code{dates} is not defined.
#'   \item  \code{endDate} a \code{Date} class object with the ending date of the 
#' study period. This argument is mandatory if 
#'   \code{dates} is not defined.
#'   \item \code{region} a \code{Spatial*}, projected \code{raster*}, o
#'   r \code{sf} class object defining the area of interest. This argument is
#'   mandatory if \code{pathrow}, \code{extent}, or \code{lonlat} are not defined.
#'   \item \code{pathrow} a list of vectors with the path and row numbers of
#'   the tiles concerning the region of interest. This argument is mandatory
#'   if \code{region}, \code{extent} or \code{lonlat} are not provided. Ex. 
#'   \code{list(c(200,31),c(200,30))}.
#'   \item \code{lonlat} a vector with the longitude/latitude
#'   coordinates of the point of interest. Ex. \code{c(-1.64323,42.81687)}.
#'   This argument is mandatory if \code{region}, \code{pathrow}, or \code{lonlat}
#'   are not defined.
#'   \item \code{extent} an \code{extent}, \code{Raster*}, or 
#'   \code{Spatial*} object representing the region of interest with 
#'   longitude/latitude coordinates. This argument is mandatory if 
#'   \code{region}, \code{pathrow} or \code{lonlat} are not defined.
#'   
#' }
#'
#' @return a \code{data.frame} with the name of the images and their metadata.
#'
#' @examples
#' \dontrun{
#' # search by path and row numbers of a tile
#' getRGISToolsOpt("EE.DataSets")
#' sres <- lsSearch(datasetName = "LANDSAT_8_C1",
#'                  startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                  endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                  username = "username",
#'                  password = "password",
#'                  region = ex.navarre,
#'                  pathrow = list(c(200,31),c(200,30)))
#'                  
#' sres <- lsSearch(datasetName = "LANDSAT_8_C1",
#'                  startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                  endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                  username = "username",
#'                  password = "password",
#'                  lonlat = c(-1.64323,42.81687))
#'                                    
#'                   
#' # search by extent (long/lat coordinates)
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' sres <- lsSearch(datasetName = "LANDSAT_8_C1",
#'                  startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                  endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                  username = "username",
#'                  password = "password",
#'                  extent = ex.navarre)
#' }
lsSearch<-function(username,password,startDate,endDate,datasetName,region,logout=TRUE,verbose=FALSE,...){
  arg<-list(...)
  ApiKey<-getRGISToolsOpt("LS.EE.KEY")
  if(is.null(ApiKey)){
    loginEEAPI(username,password,verbose)
  }

  ############################################
  # Spatial arguments
  ############################################
  if(missing(region)){
    if("extent"%in%names(arg)){
      stopifnot(class(extent(arg$extent))=="Extent")
      region <- st_as_sf(as(extent(arg$extent), 'SpatialPolygons'))
      region <- st_set_crs(region, 4326)
    }else if("lonlat"%in%names(arg)){
      stopifnot(class(arg$lonlat)=="numeric")
      stopifnot(length(arg$lonlat)==2)
      dat_sim <- data.frame(lat = arg$lonlat[2],long = arg$lonlat[1])
      dat_sf <- st_as_sf(dat_sim, coords = c("long", "lat"), crs = 4326)
      region <- suppressWarnings(st_buffer(dat_sf, dist = .1))
    }else{
      warning("Location not defined!")
    }
  }

  ############################################
  # Perform the search
  ############################################
  squery=lsSearchQuery(datasetName=datasetName,
                       startDate=startDate,
                       endDate=endDate,
                       sf.obj=st_transform(region,crs=4326),
                       ...)
  
  c.handle = new_handle()
  handle_setopt(c.handle,
                referer=getRGISToolsOpt("LS.EE.API"),
                useragent = getRGISToolsOpt("USERAGENT"),
                followlocation = TRUE ,
                autoreferer = TRUE)
  ApiSearch<-curl(paste0(getRGISToolsOpt("LS.EE.API"),'search?jsonRequest=',squery),
                  handle =c.handle)
  jsonres<-fromJSON(readLines(ApiSearch))
  close(ApiSearch)
  res.df<-data.frame(t(sapply(jsonres$data$results,c)))
  names(res.df)[which(names(res.df)%in%"browseUrl")]<-"browseURL"
  names(res.df)[which(names(res.df)%in%"entityId")]<-"sceneID"
  
  #boundaries for previsualization
  bounds<-lapply(unlist((res.df["sceneBounds"])),function(x){return(as.numeric(unlist(strsplit(x,","))))})
  bounds<-t(sapply(bounds,c))
  rownames(bounds)<-NULL
  colnames(bounds)<-c("LongitudeMin","LatitudeMin","LongitudeMax","LatitudeMax")
  res.df<-cbind(res.df,bounds)
  
  #add path row for previsualization
  pathrow=cbind(as.numeric(substr(lsGetPathRow(unlist(res.df$sceneID)),1,3)),
                as.numeric(substr(lsGetPathRow(unlist(res.df$sceneID)),4,6)))
  colnames(pathrow)<-c("path","row")
  res.df<-cbind(res.df,pathrow)
  
  ############################################
  # Filter by path row
  ############################################
  if("pathrow"%in%names(arg)){
    stopifnot(class(arg$pathrow)=="list")
    res.df<-do.call(rbind,lapply(arg$pathrow,function(rp,res.df,verbose)return(genFilterDF(res.df,row=rp[2],path=rp[1],verbose=verbose)),
                                 res.df,
                                 verbose=verbose))
  }
  if(logout){logoutEEAPI(verbose)}
  return(res.df)
}