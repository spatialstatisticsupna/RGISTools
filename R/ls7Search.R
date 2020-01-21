#' Search Landsat-7 images
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
#'   \item column names in the .LS7MD \code{data.frame} and their values.
#' }
#'
#' @return a \code{data.frame} with the name of the images and their metadata.
#'
#' @examples
#' \dontrun{
#' wdir <- file.path(tempdir(),"Path_for_downloading_folder")
#' # search by path and row numbers of a tile
#' sres <- ls7Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                   endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                   pathrow = list(c(200,31),c(200,30)),
#'                   browseAvaliable = "Y",
#'                   AppRoot = wdir)
#'                   
#' # search by point coordinates (long/lat coordinates)
#' sres <- ls7Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                   endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                   lonlat = c(-1.64323,42.81687),
#'                   browseAvaliable = "Y",
#'                   AppRoot = wdir)
#'                   
#' # search by extent (long/lat coordinates)
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' sres <- ls7Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                   endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                   extent = ex.navarre,
#'                   precise = TRUE,
#'                   browseAvaliable = "Y",
#'                   AppRoot = wdir)
#'
#' # search by extent (fast mode)
#' sres <- ls7Search(startDate = as.Date("01-01-2011","%d-%m-%Y"),
#'                   endDate = as.Date("31-12-2013","%d-%m-%Y"),
#'                   extent = ex.navarre,
#'                   precise = FALSE,
#'                   browseAvaliable = "Y",
#'                   AppRoot = wdir)
#' # remove metadata to free memory space
#' lsRemoveMetadata()
#' }
ls7Search<-function(AppRoot,verbose=FALSE,precise=FALSE,...){
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

  
  AppRoot<-pathWinLx(AppRoot)
  if(!ls7IsMetaData()){
    message("MetaData not loaded! loading...")
    ls7LoadMetadata(AppRoot=AppRoot,update=FALSE,...)
  }
  
  
  LS7MD<-getRGISToolsOpt("LS7METADATA")
  LS7MD<-LS7MD[as.Date(LS7MD$acquisitionDate)>=startDate&
               as.Date(LS7MD$acquisitionDate)<=endDate,]
  

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
    
    dat_sim <- data.frame(lat = arg$lonlat[2],long = arg$lonlat[1])
    dat_sf <- st_transform(st_as_sf(dat_sim, coords = c("long", "lat"), crs = 4326), 3035)
    circle <- st_buffer(dat_sf, dist = 1)
    circle <- st_transform(circle, 4326)
    
    if(precise){
      tiles<-unlist(apply(LS7MD[grepl("Corner",names(LS7MD))],1,tileIn,ext=extent(circle)))
      LS7MD<-LS7MD[tiles,]
    }else{
      pathrow<-names(ls7pr)[unlist(lapply(ls7pr,tileInExt,ext2=extent(circle)))]
      pathrow<-as.data.frame(cbind(as.integer(substr(pathrow,1,3)),as.integer(substr(pathrow,4,6))))
      pathrow = lapply(as.list(1:dim(pathrow)[1]), function(x) unname(pathrow[x[1],]))
      LS7MD<-do.call(rbind,lapply(pathrow,
                                  function(pr,LS7MD,verbose){pr=unlist(pr);return(genFilterDF(LS7MD,row=pr[2],path=pr[1],verbose=verbose))},
                                  LS7MD=LS7MD,
                                  verbose=verbose))
    }
  }else if("region"%in%names(arg)){
    arg$region<-transform_multiple_proj(arg$region, proj4=st_crs(4326))
    if(precise){
      tiles<-unlist(apply(LS7MD[grepl("Corner",names(LS7MD))],1,tileIn,ext=extent(arg$region)))
      LS7MD<-LS7MD[tiles,]
    }else{
      pathrow<-names(ls7pr)[unlist(lapply(ls7pr,tileInExt,ext2=extent(arg$region)))]
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

  if("cloudCover"%in%names(arg)){
    LS7MD<-LS7MD[LS7MD$cloudCover>min(arg$cloudCover)&LS7MD$cloudCover<max(arg$cloudCover),]
  }
  
  arg<-arg[names(arg)[which(!names(arg)%in%c("pathrow","region","cloudCover"))]]
  if(length(arg)>0){
    arg$df<-LS7MD
    LS7MD<-do.call(genFilterDF,arg)
  }

  LS7MD<-LS7MD[!duplicated(LS7MD[,c('sceneID')]),]
  #filter dates
  if("dates"%in%names(arg)){
    LS7MD<-LS7MD[as.Date(LS7MD$acquisitionDate)%in%arg$dates,]
  }
  class(LS7MD)<-"ls7res"
  return(LS7MD)
}


