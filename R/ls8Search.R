#' Search Landsat-8 images
#'
#' \code{ls8Search} searches Landsat-8 images in the Landsat repository concerning
#' a particular location and date interval. The function returns a 
#' \code{data.frame} with the names of the images and their metadata.
#' 
#' \code{ls8Search} searches images in the metadata file. If the metadata was
#' downloaded before to the current directory, \code{ls8Search} will use this
#' metadata by default. In case the metadata was not downloaded yet, 
#' \code{ls8Search} will make that call for you.
#'
#' Landsat images are organized by tiles, which have a unique path and row
#' numbers according to the
#' \href{https://landsat.gsfc.nasa.gov/the-worldwide-reference-system/}{Worldide Reference System}.
#' The fastest way to search an image in the metadata file is by path and row
#' (\code{pathrow}). This method requires to know in advance the path and row
#' number of the tile that is relevant for your region of interest. From the
#' user's standpoint, the simplest way to search a time series of Landsat-7
#' images is by \code{extent}, \code{lonlat} or \code{polygon}, since they do
#' not require any prior knowledge about tiles.
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
#'   \item \code{dates} a vector with the capturing dates being considered
#'   for searching. This argument is mandatory if 
#'   \code{startDate} and \code{endDate} are not defined.
#'   \item  startDate a \code{Date} class object with the starting date of the 
#' study period. This argument is mandatory if 
#'   \code{dates} is not defined.
#'   \item  endDate a \code{Date} class object with the ending date of the 
#' study period. This argument is mandatory if 
#'   \code{dates} is not defined.
#'   \item \code{pathrow} a \code{list} of vectors with the path and row numbers
#'   of the tiles concerning the region of interest. This argument is mandatory
#'   if \code{extent} or \code{lonlat} are not provided. Ex. 
#'   \code{list(c(200,31),c(200,30))}.
#'   \item \code{lonlat} a vector with the longitude/latitude coordinates of the
#'   point of interest. Ex. \code{c(-1.64323,42.81687)}.
#'   \item \code{extent} an \code{extent}, \code{Raster*}, or 
#'   \code{Spatial*} object representing the region of interest with 
#'   longitude/latitude coordinates. This argument is mandatory if 
#'   \code{pathrow} or \code{lonlat} are not defined.
#'   \item column names in the .LS8MD \code{data.frame} and their values.
#' }
#'
#' @return a \code{data.frame} with the name of the images and their metadata.
#'
#' @examples
#' \dontrun{
#' # search by path and row numbers of a tile
#' src <- paste0(tempdir(),"/Path_for_downloading_folder")
#' search.res <- ls8Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                         endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                         pathrow = list(c(200, 31), c(200, 30)),
#'                         browseAvaliable = "Y",
#'                         AppRoot = src)
#'
#' # search by extent (long/lat coordinates)
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' search.res <- ls8Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                         endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                         extent = ex.navarre,
#'                         precise = TRUE,
#'                         browseAvaliable = "Y",
#'                         AppRoot = src)
#'
#' # search by extent (fast mode)
#' search.res <- ls8Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                       endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                       extent = ex.navarre,
#'                       precise = FALSE,
#'                       browseAvaliable = "Y",
#'                       AppRoot = src)
#' # remove metadata to free memory space
#' lsRemoveMetadata()
#' }
ls8Search<-function(AppRoot,verbose=FALSE,precise=FALSE,...){
  arg<-list(...)
  if((!"dates"%in%names(arg))&
     ((!"startDate"%in%names(arg)|(!"endDate"%in%names(arg))))
  )stop("startDate and endDate, or dates argument need to be defined!")
  
  if("dates"%in%names(arg)){
    stopifnot(class(arg$dates)=="Date")
    startDate<-min(arg$dates)
    endDate<-max(arg$dates)
  }else{
    startDate<-arg$startDate
    endDate<-arg$endDate
  }
  
  stopifnot(class(startDate)=="Date")
  stopifnot(class(endDate)=="Date")
    
  if(endDate<as.Date("2011-03-13"))
    stop("There is no Landsat-8 Images before 13-03-2013.")
  if(startDate<as.Date("2011-03-13"))
    warning("There is no Landsat-8 Images before 13-03-2013.")

  AppRoot<-pathWinLx(AppRoot)
  if(!ls8IsMetaData()){
    message("MetaData not loaded! loading...")
    ls8LoadMetadata(AppRoot=AppRoot,update=FALSE,...)
  }

  #first filter by date
  LS8MD<-getRGISToolsOpt("LS8METADATA")
  LS8MD<-LS8MD[as.Date(LS8MD$acquisitionDate)>=startDate&
               as.Date(LS8MD$acquisitionDate)<=endDate,]
  

  #filter by position
  #pathrow list(c(path1,row1),c(path2,row2)...)
  #extent in latlog
  if("pathrow"%in%names(arg)){
    stopifnot(class(arg$pathrow)=="list")
    LS8MD<-do.call(rbind,lapply(arg$pathrow,function(rp,LS8MD,verbose)return(genFilterDF(LS8MD,row=rp[2],path=rp[1],verbose=verbose)),
                                LS8MD,
                                verbose=verbose))
  }else if("extent"%in%names(arg)){
    if(precise){
      stopifnot(class(extent(arg$extent))=="Extent")
      ext<-extent(arg$extent)
      tiles<-unlist(apply(LS8MD[grepl("Corner",names(LS8MD))],1,tileIn,ext))
      LS8MD<-LS8MD[tiles,]
    }else{
      #data(ls8pr)
      pathrow<-names(ls8pr)[unlist(lapply(ls8pr,tileInExt,ext2=extent(arg$region)))]
      pathrow<-as.data.frame(cbind(as.integer(substr(pathrow,1,3)),as.integer(substr(pathrow,4,6))))
      pathrow = lapply(as.list(1:dim(pathrow)[1]), function(x) pathrow[x[1],])
      LS8MD<-do.call(rbind,lapply(pathrow,function(rp,LS8MD,verbose){rp=unlist(rp);return(genFilterDF(LS8MD,row=rp[2],path=rp[1],verbose=verbose))},
                                  LS8MD=LS8MD,
                                  verbose=verbose))
    }
  }else if("lonlat"%in%names(arg)){
    stopifnot(class(arg$lonlat)=="numeric")
    stopifnot(length(arg$lonlat)==2)
    circle=list()
    circle[[1]]<-Polygons(list(Polygon(genCreateSpatialCircle(x=arg$lonlat[1],y=arg$lonlat[2]))),ID=1)

    circle<-SpatialPolygons(circle,proj4string=st_crs("+init=epsg:4326")$proj4string)
    if(precise){
      tiles<-unlist(apply(LS8MD[grepl("Corner",names(LS8MD))],1,tileIn,ext=circle))
      LS8MD<-LS8MD[tiles,]
    }else{
      #data(ls8pr)
      pathrow<-names(ls8pr)[unlist(lapply(ls8pr,tileInExt,ext2=circle))]
      pathrow<-as.data.frame(cbind(as.integer(substr(pathrow,1,3)),as.integer(substr(pathrow,4,6))))
      pathrow = lapply(as.list(1:dim(pathrow)[1]), function(x) unname(pathrow[x[1],]))
      LS8MD<-do.call(rbind,lapply(pathrow,
                                  function(pr,LS8MD,verbose){pr=unlist(pr);return(genFilterDF(LS8MD,row=pr[2],path=pr[1],verbose=verbose))},
                                  LS8MD=LS8MD,
                                  verbose=verbose))
    }
  }else if("region"%in%names(arg)){
    arg$region<-transform_multiple_proj(arg$region, proj4=st_crs("+init=epsg:4326"))
    if(precise){
      tiles<-unlist(apply(LS8MD[grepl("Corner",names(LS8MD))],1,tileIn,ext=extent(arg$region)))
      LS8MD<-LS8MD[tiles,]
    }else{
      #data(ls8pr)
      pathrow<-names(ls8pr)[unlist(lapply(ls8pr,tileInExt,ext2=extent(arg$region)))]
      pathrow<-as.data.frame(cbind(as.integer(substr(pathrow,1,3)),as.integer(substr(pathrow,4,6))))
      pathrow = lapply(as.list(1:dim(pathrow)[1]), function(x) unname(pathrow[x[1],]))
      LS8MD<-do.call(rbind,lapply(pathrow,
                                  function(pr,LS8MD,verbose){pr=unlist(pr);return(genFilterDF(LS8MD,row=pr[2],path=pr[1],verbose=verbose))},
                                  LS8MD=LS8MD,
                                  verbose=verbose))
    }
  }else{
    warning("Location not defined!")
  }
  arg<-arg[names(arg)[which(!names(arg)%in%c("pathrow","region"))]]
  if(length(arg)>0)
    LS8MD<-genFilterDF(LS8MD,verbose=verbose,...)
  
  LS8MD<-LS8MD[!duplicated(LS8MD[,c('sceneID')]),]
  
  #filter dates
  if("dates"%in%names(arg)){
    LS8MD<-LS8MD[as.Date(LS8MD$acquisitionDate)%in%arg$dates,]
  }
  
  return(LS8MD)
}

tileIn<-function(dv,ext){
  lat<-unlist(dv[grepl("Latitude",names(dv))])
  lon<-unlist(dv[grepl("Longitude",names(dv))])
  sps<-extent(min(lon),max(lon),min(lat),max(lat))
  return(!is.null(raster::intersect(sps,ext)))
}
tileInExt<-function(ext1,ext2){
  return(!is.null(raster::intersect(ext1,ext2)))
}
