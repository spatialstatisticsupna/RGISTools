#' Search Landsat-7 images
#'
#' \code{ls7Search} seeks Landsat-7 images in the Landsat repository concerning
#' a particular location and date interval. The function returns a 
#' \code{data.frame} with the names of the images and their metadata.
#'
#' \code{ls7Search} seeks images in the metadata file. If the metadata was
#' downloaded before to the current directory, \code{ls7Search} will use this
#' metadata by default. In case the metadata was not downloaded before, 
#' \code{ls7Search} will make that call for you.
#'
#' Landsat images are organized by tiles, which have a unique path and row
#' numbers according to the
#' \href{https://landsat.gsfc.nasa.gov/the-worldwide-reference-system/}{Worldide Reference System}.
#' The fastest way to search an image in the metadata file is by path and row
#' (\code{pathrow}). This method requires to know in advance the path and row
#' number of the tile that is relevant for your region of interest. From the
#' user's standpoint, the simplest way to search a time series of Landsat-7
#' images is by \code{extent}, \code{lonlat} or \code{polygon}, since they do 
#' not require any prior knowledge about the tiles.
#'
#' The function can screen the results by any other attribute in the metadata.
#' For instance, to filter the imagery with an available preview, the 
#' \code{browseAvaliable=”Y”} must be added as an argument of the function
#' (see the examples).
#'
#' @param startDate a \code{Date} class object with the starting date of the 
#' study period.
#' @param endDate a \code{Date} class object with the ending date of the 
#' study period.
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param precise logical argument. If \code{TRUE}, conducts a thorough search,
#' tile by tile (slower).
#' @param AppRoot directory of the metadata file. 
#' @param ... arguments for nested functions:
#'  \itemize{
#'   \item \code{pathrow} a list of vectors with the path and row numbers of
#'   the tiles concerning the region of interest. This argument is mandatory
#'   if \code{extent} or \code{lonlat} are not provided. Ex. 
#'   \code{list(c(200,31),c(200,30))}.
#'   \item \code{lonlat} a vector with the longitude/latitude
#'   coordinates of the point of interest. Ex. \code{c(-1.64323,42.81687)}.
#'   \item \code{extent} an \code{extent}, \code{Raster*}, or 
#'   \code{Spatial}* object representing the region of interest with 
#'   longitude/latitude coordinates. This argument is mandatory if 
#'   \code{pathrow} or \code{lonlat} are not defined.
#'   
#'   \item column names in the .LS7MD \code{data.frame} and their values.
#' }
#'
#' @examples
#' \dontrun{
#' # search by path and row numbers of a tile
#' search.res <- ls7Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                         endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                         pathrow = list(c(200,31),c(200,30)),
#'                         browseAvaliable = "Y")
#'                   
#' # search by point coordinates (long/lat coordinates)
#' search.res <- ls7Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                         endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                         lonlat = c(-1.64323,42.81687),
#'                         browseAvaliable = "Y")
#'                   
#' # search by extent (long/lat coordinates)
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' search.res <- ls7Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                         endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                         extent = ex.navarre,
#'                         precise = TRUE,
#'                         browseAvaliable = "Y")
#'
#' # search by extent (fast mode)
#' search.res <- ls7Search(startDate = as.Date("01-01-2011","%d-%m-%Y"),
#'                         endDate = as.Date("31-12-2013","%d-%m-%Y"),
#'                         extent = ex.navarre,
#'                         precise = FALSE,
#'                         browseAvaliable = "Y")
#' # remove metadata to free memory space
#' lsRemoveMetadata()
#' }
ls7Search<-function(startDate,endDate,AppRoot,verbose=FALSE,precise=FALSE,...){
  stopifnot(class(startDate)=="Date")
  stopifnot(class(endDate)=="Date")
  arg<-list(...)
  
  AppRoot<-pathWinLx(AppRoot)
  if(!ls7IsMetaData()|endDate>as.Date(Sys.time())|getRGISToolsOpt("LS7META.var")%in%ls(all.names=TRUE)){
    message("MetaData not loaded! loading...")
    ls7LoadMetadata(AppRoot=AppRoot,update=FALSE,...)
  }
   # now it can be found
  #first filter by date
  LS7MD<-get(getRGISToolsOpt("LS7META.var"), envir=globalenv())[as.Date(get(getRGISToolsOpt("LS7META.var"), envir=globalenv())$acquisitionDate)>=startDate&
                                                                as.Date(get(getRGISToolsOpt("LS7META.var"), envir=globalenv())$acquisitionDate)<=endDate,]

  #filter by position
  #pathrow list(c(path1,row1),c(path2,row2)...)
  #extent in latlog

  if("pathrow"%in%names(arg)){
      stopifnot(class(arg$pathrow)=="list")
      LS7MD<-do.call(rbind,lapply(arg$pathrow,function(rp,LS7MD,verbose)return(genFilterDF(LS7MD,row=rp[2],path=rp[1],verbose=verbose)),
                                   LS7MD,
                                   verbose=verbose))
  }else if("extent"%in%names(arg)){
      stopifnot(class(extent(arg$extent))=="Extent")
      if(precise){
        tiles<-unlist(apply(LS7MD[grepl("Corner",names(LS7MD))],1,tileIn,ext=extent(arg$extent)))
        LS7MD<-LS7MD[tiles,]
      }else{
        #data(ls7pr)
        pathrow<-names(ls7pr)[unlist(lapply(ls7pr,tileInExt,ext2=extent(arg$extent)))]
        pathrow<-as.data.frame(cbind(as.integer(substr(pathrow,1,3)),as.integer(substr(pathrow,4,6))))
        pathrow = lapply(as.list(1:dim(pathrow)[1]), function(x) unname(pathrow[x[1],]))
        LS7MD<-do.call(rbind,lapply(pathrow,
                                    function(pr,LS7MD,verbose){pr=unlist(pr);return(genFilterDF(LS7MD,row=pr[2],path=pr[1],verbose=verbose))},
                                    LS7MD=LS7MD,
                                    verbose=verbose))
      }
  }else if("lonlat"%in%names(arg)){
    stopifnot(class(arg$lonlat)=="numeric")
    stopifnot(length(arg$lonlat)==2)
    circle=list()
    circle[[1]]<-Polygons(list(Polygon(genCreateSpatialCircle(x=arg$lonlat[1],y=arg$lonlat[2]))),ID=1)

    circle<-SpatialPolygons(circle,proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
    if(precise){
      tiles<-unlist(apply(LS7MD[grepl("Corner",names(LS7MD))],1,tileIn,ext=circle))
      LS7MD<-LS7MD[tiles,]
    }else{
      #data(ls7pr)
      pathrow<-names(ls7pr)[unlist(lapply(ls7pr,tileInExt,ext2=circle))]
      pathrow<-as.data.frame(cbind(as.integer(substr(pathrow,1,3)),as.integer(substr(pathrow,4,6))))
      pathrow = lapply(as.list(1:dim(pathrow)[1]), function(x) unname(pathrow[x[1],]))
      LS7MD<-do.call(rbind,lapply(pathrow,
                                  function(pr,LS7MD,verbose){pr=unlist(pr);return(genFilterDF(LS7MD,row=pr[2],path=pr[1],verbose=verbose))},
                                  LS7MD=LS7MD,
                                  verbose=verbose))
    }
  }else if("polygon"%in%names(arg)){
    stopifnot(class(arg$polygon)=="SpatialPolygons"||class(arg$polygon)=="SpatialPolygonsDataFrame")
    if(precise){
      tiles<-unlist(apply(LS7MD[grepl("Corner",names(LS7MD))],1,tileIn,ext=extent(arg$polygon)))
      LS7MD<-LS7MD[tiles,]
    }else{
      #data(ls7pr)
      pathrow<-names(ls7pr)[unlist(lapply(ls7pr,tileInExt,ext2=extent(arg$polygon)))]
      pathrow<-as.data.frame(cbind(as.integer(substr(pathrow,1,3)),as.integer(substr(pathrow,4,6))))
      pathrow = lapply(as.list(1:dim(pathrow)[1]), function(x) unname(pathrow[x[1],]))
      LS7MD<-do.call(rbind,lapply(pathrow,
                                  function(pr,LS7MD,verbose){pr=unlist(pr);return(genFilterDF(LS7MD,row=pr[2],path=pr[1],verbose=verbose))},
                                  LS7MD=LS7MD,
                                  verbose=verbose))
    }
  }else{
    warning("Location not defined!")
  }

  arg<-arg[names(arg)[which(!names(arg)%in%c("pathrow","extent"))]]
  if(length(arg)>0)
    LS7MD<-genFilterDF(LS7MD,verbose=verbose,...)

  LS7MD<-LS7MD[!duplicated(LS7MD[,c('sceneID')]),]
  return(LS7MD)
}


