#' Search Landsat 7-8 images using EarthExplorer API
#'
#' \code{lsSearch} searches Landsat 7-8 images in the EarthExplorer API concerning
#' a particular location and date interval. The function returns a 
#' \code{data.frame} with the names of the images and their metadata.
#' 
#' @param datasetName the name of the dataset. Avaliable names saved in `RGISTools'
#' (\code{getRGISToolsOpt("EE.DataSets")}).
#' @param username NASA’s `EarthData' username.
#' @param password NASA’s `EarthData' password.
#' @param region a \code{Spatial*}, projected \code{raster*}, o
#'   r \code{sf} class object defining the area of interest. This argument is
#'   mandatory if \code{pathrow}, \code{extent}, or \code{lonlat} are not defined.
#'  @param startDate a \code{Date} class object with the starting date of the 
#' study period. This argument is mandatory if 
#'   \code{dates} is not defined.
#'  @param endDate a \code{Date} class object with the ending date of the 
#' study period. This argument is mandatory if 
#'   \code{dates} is not defined.
#'  @param dates a vector with the capturing dates being searched. This
#'   argument is mandatory if \code{startDate} and \code{endDate} are not defined.
#' @param logout logical argument. If \code{TRUE}, logges out from EarthExplorer
#' API
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param ... arguments for nested functions:
#'  \itemize{
#'   \item \code{pathrow} a list of vectors with the path and row numbers of
#'   the tiles concerning the region of interest. This argument is mandatory
#'   if \code{region}, \code{extent} or \code{lonlat} are not provided. Ex. 
#'   \code{list(c(200,31),c(200,30))}.
#'   \item \code{lonlat} a vector with the longitude/latitude (EPSG:4326)
#'   coordinates of the point of interest. Ex. \code{c(-1.64323,42.81687)}.
#'   This argument is mandatory if \code{region}, \code{pathrow}, or \code{lonlat}
#'   are not defined.
#'   \item \code{extent} an \code{extent}, \code{Raster*}, or 
#'   \code{Spatial*} object representing the region of interest with 
#'   longitude/latitude (EPSG:4326) coordinates. This argument is mandatory if 
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
lsSearch<-function(datasetName,startDate,endDate,region,username,password,dates,logout=TRUE,verbose=FALSE,...){
  arg<-list(...)
  ApiKey<-getRGISToolsOpt("LS.EE.KEY")
  if(is.null(ApiKey)){
    loginEEAPI(username,password,verbose)
  }
  
  if(missing(dates)){
    if(missing(startDate)&&missing(endDate)) stop("startDate and endDate, or dates must be defined.")
  }else{
    startDate<-as.Date(min(dates))
    endDate<-as.Date(max(dates))
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
  jsonres<-fromJSON(suppressWarnings(readLines(ApiSearch)))
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
  ############################################
  # Filter by dates
  ############################################
  if(!missing(dates)){
    res.df<-res.df[as.Date(unlist(res.df$acquisitionDate))%in%dates,]
  }
  
  return(res.df)
}