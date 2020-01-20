lsUpdateEEDataSets<-function(username,password,logout=TRUE){
  ApiKey<-getRGISToolsOpt("LS.EE.KEY")
  if(is.null(ApiKey)){
    loginEEAPI(username,password)
  }
  c.handle = new_handle()
  handle_setopt(c.handle,
                referer=getRGISToolsOpt("LS.EE.API"),
                useragent = getRGISToolsOpt("USERAGENT"),
                followlocation = TRUE ,
                autoreferer = TRUE)
  respose<-suppressWarnings(curl(paste0(getRGISToolsOpt("LS.EE.API"),'datasets?jsonRequest={"apiKey":"', getRGISToolsOpt("LS.EE.KEY"), '"}'),
                                 handle =c.handle))
  rm(c.handle)
  datasets<-fromJSON(suppressWarnings(readLines(respose)))
  datasets<-datasets$data
  datasets<-unlist(lapply(datasets, function(x){x$datasetName}))
  if(logout){logoutEEAPI()}
  return(datasets)
}
