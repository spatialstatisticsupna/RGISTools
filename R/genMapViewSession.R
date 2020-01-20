genMapViewSession<-function(r,lpos,lname,add.Layer,...){
  sessionmap<-getRGISToolsOpt("GMapView")
  if(is.null(sessionmap)|(!add.Layer)){
    sessionmap<-viewRGB(subset(r,lpos),
                        layer.name =lname,...)
    # %>% leafem::addLogo(file.path(system.file("logo", package = "RGISTools"),"RGISTools_logo.gif"), 
    #                                               url = "",
    #                                               width=180,
    #                                               height = 70,
    #                                               offset.x=10,
    #                                               offset.y=10,
    #                                               position = c("topright"),
    #                                               src=c("local"))
    setRGISToolsOpt("GMapView",sessionmap)
    return(sessionmap)
  }else{
    sessionmap<-viewRGB(subset(r,lpos),layer.name =lname,...)+sessionmap
    setRGISToolsOpt("GMapView",sessionmap)
    return(sessionmap)
  }
}