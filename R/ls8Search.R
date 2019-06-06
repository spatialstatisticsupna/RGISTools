#' Search landsat 8 time-series images list
#'
#' \code{ls8Search} searches the LANDSAT 8 image repository to find those which are relevant for
#' a particular location and date interval. The function returns the search result as a data frame
#' with the names of the images and their metadata.
#'
#' \code{ls8Search} is a stand-alone function. If the metadata for the time and region of interest has been
#' downloaded before, \code{ls8Search} will use this metadata by default. In case the metadata has not
#' been yet downloaded, \code{ls8Search} will make the call for you.
#'
#' The search is done by defining a temporal interval and a location. The arguments \code{startDate}
#' and \code{endDate} defines the temporal interval.These are mandatory arguments. The function defines the spatial location
#' using at least one of the following arguments: \code{pathrow}, \code{extent}, \code{lonlat} y \code{polygon}. When more than one of these argument is defined,
#' the function will work with the first evaluated method, when none of them is defined, the function shows an error message.
#'
#' \code{ls8Search} uses the metadata file downloaded by \code{ls8LoadMetadata}. However, it also works as a stand-alone function.
#' If the metadata for the time and region of interest was downloaded before, \code{ls8Search} uses this metadata by default.
#' When the metadata was not download, \code{ls8Search} makes the call for you.
#'
#' Landsat images are catalogued spatially using a unique path and row. The fastest way to search an image
#' in the metadata file is filtering by its path and row. This search method requires previous knowledge on
#' the path and row relevant for your region of interest.
#'
#' From the user point of view, the easiest way to search a time series of Landsat-8 is using the extent,
#' \code{lonlat} and polygon arguments. These methods do not requires to know in advance the path and rows of the images.
#' These method uses spatial objects to define the region of interest. The projection of the spatial needs to be
#' "\code{+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs}”. The argument \code{extent} accepts any R objects being defined
#' by a spatial extent. The argument \code{lonlat} only accepts an R vector with one coordinate in the form of
#' latitude-longitude (ex. \code{c(42.81687, -1.64323)}, where the first element is the latitude and the second is the longitude).
#' The argument \code{polygon}, accepts \code{spatialpolygon} or s\code{patialpolygondataframe} objects.
#'
#' The search procedure using spatial objects compares the spatial extension of Landsat images with the
#' the extension of the objects provided by the user. The function checks which ones overlays.
#' The search functions has an estimate of the extension of each
#' path row as preprocessed. The function first compares
#' the object with the predefined extension and gets the path and row of the images that overlays with the spatial
#' object. Then, it uses the path and row to get the search result which reduces its.
#' The function gives the possibility to evaluate and compare each image without preprocess data. These can
#' be specified with the \code{precise=T} argument in the function call, but this procedure will be slower.
#'
#' In addition, the search function enables further filtering. The function can filter
#' the results by any column name in the metadata file, using the column name as an argument. For example, to
#' filter the images that can be previewed, the user has to find the images with a “Y” in the browseAvaliable column.
#' This can be achieved by adding \code{browseAvaliable=”Y”} as a function argument.
#'
#'
#' @param startDate starting date of the time series for search images.
#' @param endDate ending date of the time series for search images.
#' @param verbose logical argument. If \code{TRUE} the function prints running stages and warnings.
#' @param precise logical argument. If \code{TRUE} the search is donw tile by tile (slower).
#' @param ... argument to allow function nestering:
#'  \itemize{
#'   \item \code{pathrow} a list of vectors defining the path and row number for the region of interest according
#' to the Worldwide Reference System (\url{https://landsat.gsfc.nasa.gov/the-worldwide-reference-system/})
#' This argument is mandatory if extent is not defined.
#'   \item \code{lonlat} this argument is optional. A vector or a polygon with the coordinates of
#' the point or region of interest in latitude/longitude format.
#'   \item \code{extent} this argument is optional. Extent, Raster*, SpatialPolygons*, SpatialLines* or SpatialPoints*
#' object are acceptable formats as long as are latitude/longitude format.
#' This argument is mandatory if pathrow is not defined.
#'   \item \code{AppRoot} the root directory where meta data file will be saved.
#'   \item all column names in .LS8MD data frame for filter results.
#' }
#'
#' @examples
#' \dontrun{
#' #search by known row and path
#' search<-ls8Search(startDate=as.Date("01-01-2011","%d-%m-%Y"),
#'                   endDate=as.Date("31-12-2013","%d-%m-%Y"),
#'                   pathrow=list(c(200,31),c(200,30)),
#'                   browseAvaliable="Y")
#'
#' #search by projected file must be in lat long projection
#' data(ex.navarre)
#' search<-ls8Search(startDate=as.Date("01-01-2011","%d-%m-%Y"),
#'                   endDate=as.Date("31-12-2013","%d-%m-%Y"),
#'                   extent=ex.navarre,
#'                   precise=T,
#'                   browseAvaliable="Y")
#'
#' #search by projected file fast
#' search<-ls8Search(startDate=as.Date("01-01-2011","%d-%m-%Y"),
#'                   endDate=as.Date("31-12-2013","%d-%m-%Y"),
#'                   extent=ex.navarre,
#'                   precise=F,
#'                   browseAvaliable="Y")
#' }
ls8Search<-function(startDate,endDate,verbose=FALSE,precise=FALSE,...){
  stopifnot(class(startDate)=="Date")
  stopifnot(class(endDate)=="Date")
  if(endDate<as.Date("2011-03-13"))
    stop("There is no Landsat-8 Images before 13-03-2013.")
  arg<-list(...)
  AppRoot<-defineAppRoot(...)

  if(!ls8IsMetaData()|endDate>as.Date(Sys.time())|getRGISToolsOpt("LS8META.var")%in%ls(all.names=T)){
    message("MetaData not loaded! loading...")
    ls8LoadMetadata(AppRoot=AppRoot,update=F)
  }

  #first filter by date
  LS8MD<-get(getRGISToolsOpt("LS8META.var"), envir=globalenv())[as.Date(get(getRGISToolsOpt("LS8META.var"), envir=globalenv())$acquisitionDate)>=startDate&
                                                                as.Date(get(getRGISToolsOpt("LS8META.var"), envir=globalenv())$acquisitionDate)<=endDate,]

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
      pathrow<-names(ls8pr)[unlist(lapply(ls8pr,tileInExt,ext2=extent(arg$extent)))]
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
    circle[[1]]<-Polygons(list(Polygon(genCreateSpatialCircle(x=arg$lonlat[2],y=arg$lonlat[1]))),ID=1)

    circle<-SpatialPolygons(circle,proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
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
  }else if("polygon"%in%names(arg)){
    stopifnot(class(arg$polygon)=="SpatialPolygons"||class(arg$polygon)=="SpatialPolygonsDataFrame")
    if(precise){
      tiles<-unlist(apply(LS8MD[grepl("Corner",names(LS8MD))],1,tileIn,ext=extent(arg$polygon)))
      LS8MD<-LS8MD[tiles,]
    }else{
      #data(ls8pr)
      pathrow<-names(ls8pr)[unlist(lapply(ls8pr,tileInExt,ext2=extent(arg$polygon)))]
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
  arg<-arg[names(arg)[which(!names(arg)%in%c("pathrow","extent"))]]
  if(length(arg)>0)
    LS8MD<-genFilterDF(LS8MD,verbose=verbose,...)
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
