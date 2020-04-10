senSearchQuery<-function(...){
  url<-paste0(getRGISToolsOpt("SCIHUBHUSURL"),"/search?q=")
  arg<-list(...)
  if(!"verbose"%in%names(arg)){
    arg$verbose=FALSE
  }
  #add ingestion date to query
  if("startDate"%in%names(arg)){
    if(arg$verbose)
      message("Adapting dates.")
    startDate<-paste0(format(arg$startDate,"%Y-%m-%d"),"T00:00:00.000Z")
    if(is.null(arg$endDate)){
      endDate<-"NOW"
    }else{
      endDate<-paste0(format(arg$endDate,"%Y-%m-%d"),"T23:59:59.999Z")
    }
    url<-paste0(url,"beginposition:[",startDate," TO ",endDate,"]")
  }
  if("platform"%in%names(arg)){
    if(arg$verbose)
      message("Adding platform name.")
    url<-paste0(url," AND platformname:",arg$platform)
  }
  if("extent"%in%names(arg)){
    stopifnot(class(extent(arg$extent))=="Extent")
    if(arg$verbose)
      message("Adding query extent.")
    ext<-extent(arg$extent)
    url<-paste0(url," AND footprint:",'"',"intersects(POLYGON((",ext@xmin," ",ext@ymin,","
                ,ext@xmin," ",ext@ymax,","
                ,ext@xmax," ",ext@ymax,","
                ,ext@xmax," ",ext@ymin,","
                ,ext@xmin," ",ext@ymin,")))",'"')

  }
  if("lonlat"%in%names(arg)){
    if(arg$verbose){
      message(print("Adding query intersects"))
    }
    if(!length(arg$lonlat)==2){
      stop("The intersects argument is not a longitude/latitude valid location.")
    }
    url<-paste0(url," AND footprint:",'"',"intersects(",arg$lonlat[1],", ",arg$lonlat[2],")",'"')
  }
  if("region"%in%names(arg)){
    if(arg$verbose){
      message(print("Adding query region"))
    }
    arg$region<-transform_multiple_proj(arg$region, proj4=st_crs(4326))
    ext<-st_bbox(arg$region)
    url<-paste0(url," AND footprint:",'"',"intersects(POLYGON((",ext$xmin," ",ext$ymin,","
                ,ext$xmin," ",ext$ymax,","
                ,ext$xmax," ",ext$ymax,","
                ,ext$xmax," ",ext$ymin,","
                ,ext$xmin," ",ext$ymin,")))",'"')
  }
  if("product"%in%names(arg)){
    if(arg$verbose){
      message("Added product type.")
    }
    url<-paste0(url," AND producttype:",arg$product)
  }
  if("relativeorbit"%in%names(arg)){
    if(arg$verbose){
      message("Added relative orbit number type.")
    }
    url<-paste0(url," AND relativeorbitnumber:",arg$relativeorbit)
  }
  if("cloudCover"%in%names(arg)){
    if(arg$verbose){
      message("Added cloud cover percentage.")
    }
    url<-paste0(url," AND cloudcoverpercentage:[",min(arg$cloudCover)," TO ",max(arg$cloudCover),"]")
  }
  if("timeliness"%in%names(arg)){
    if(arg$verbose){
      message("Added timeliness.")
    }
    url<-paste0(url,' AND timeliness:"',arg$timeliness,'"')
  }
  if("qformat"%in%names(arg)){
    url<-paste0(url,"&format=",arg$qformat)
  }else{
    url<-paste0(url,"&format=json")
  }
  url<-paste0(url,"&rows=100")
  return(URLencode(url))
}
