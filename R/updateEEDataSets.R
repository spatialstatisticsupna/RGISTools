updateEEDataSets<-function(){
  c.handle = new_handle()
  handle_setopt(c.handle,
                referer=getRGISToolsOpt("LS.EE.API"),
                useragent = getRGISToolsOpt("USERAGENT"),
                followlocation = TRUE ,
                autoreferer = TRUE)
  respose<-curl(paste0(getRGISToolsOpt("LS.EE.API"),'datasets?jsonRequest={"apiKey":"', getRGISToolsOpt("LS.EE.KEY"), '"}'),
                handle =c.handle)
  datasets<-fromJSON(readLines(respose))
  datasets<-datasets$data
  names(datasets)<-unlist(lapply(datasets, function(x){x$datasetName}))
  class(datasets)<-"EarthExplorerDataSets"
  return(datasets)
}