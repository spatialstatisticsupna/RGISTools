senSearchQuery<-function(...){
  url<-paste0(getRGISToolsOpt("SCIHUBHUSURL"),"/search?q=")
  arg<-list(...)
  if(!"verbose"%in%names(arg)){
    arg$verbose=FALSE
  }
  #add ingestion date to query
  if("startDate"%in%names(arg)){
    if(arg$verbose)
      print("Adapting dates.")
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
      print("Adding platform name.")
    url<-paste0(url," AND platformname:",arg$platform)
  }
  if("extent"%in%names(arg)){
    stopifnot(class(extent(arg$extent))=="Extent")
    if(arg$verbose)
      print("Adding query extent.")
    ext<-extent(arg$extent)
    url<-paste0(url," AND footprint:",'"',"intersects(POLYGON((",ext@xmin," ",ext@ymin,","
                ,ext@xmin," ",ext@ymax,","
                ,ext@xmax," ",ext@ymax,","
                ,ext@xmax," ",ext@ymin,","
                ,ext@xmin," ",ext@ymin,")))",'"')

  }
  if("intersects"%in%names(arg)){
    if(arg$verbose){
      print(print("Adding query intersects"))
    }
    if(!length(arg$intersects)==2){
      stop("The intersects argument is not a latitude longitude valid location.")
    }
    url<-paste0(url," AND footprint:",'"',"intersects(",arg$intersects[[1]],", ",arg$intersects[[2]],")",'"')
  }
  if("product"%in%names(arg)){
    if(arg$verbose){
      print("Added product type.")
    }
    url<-paste0(url," AND producttype:",arg$product)
  }
  if("relativeorbit"%in%names(arg)){
    if(arg$verbose){
      print("Added relative orbit number type.")
    }
    url<-paste0(url," AND relativeorbitnumber:",arg$relativeorbit)
  }
  if("qformat"%in%names(arg)){
    url<-paste0(url,"&format=",arg$qformat)
  }else{
    url<-paste0(url,"&format=json")
  }
  url<-paste0(url,"&rows=100")
  return(URLencode(url))
}
