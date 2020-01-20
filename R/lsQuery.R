lsSearchQuery<-function(datasetName,startDate,endDate,sf.obj,...){
  args<-list(...)
  lsquery<-NULL
  #temporal filter
  lsquery$datasetName<- datasetName#'LANDSAT_8_C1'
  lsquery$temporalFilter<-list("startDate"=format(startDate,"%d-%m-%Y"),
                               "endDate"=format(endDate,"%d-%m-%Y"))
  #spatial filter
  lsquery$spatialFilter<-list("filterType"='mbr',
                              "lowerLeft"=list("latitude"=st_bbox(sf.obj)[["ymin"]],
                                               "longitude"=st_bbox(sf.obj)[["xmin"]]),
                              "upperRight"=list("latitude"=st_bbox(sf.obj)[["ymax"]],
                                                "longitude"=st_bbox(sf.obj)[["xmax"]]))
  #additional criteria
  lsquery$maxResults<-50000
  lsquery$startingNumber<-1
  lsquery$sortOrder<-"ASC"
  lsquery$apiKey<-getRGISToolsOpt("LS.EE.KEY")
  return(toJSON(lsquery))
}