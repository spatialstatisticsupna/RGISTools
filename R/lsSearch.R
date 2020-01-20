
lsSearch<-function(username,password,startDate,endDate,datasetName,region,logout=TRUE,...){
  ApiKey<-getRGISToolsOpt("LS.EE.KEY")
  if(is.null(ApiKey)){
    loginEEAPI(username,password)
  }
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
  paste0(getRGISToolsOpt("LS.EE.API"),'search?jsonRequest=',squery)
  ApiSearch<-suppressWarnings(curl(paste0(getRGISToolsOpt("LS.EE.API"),'search?jsonRequest=',squery),
                  handle =c.handle))
  jsonres<-suppressWarnings(fromJSON(readLines(ApiSearch)))
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

  if(logout){logoutEEAPI()}
  return(res.df)
}