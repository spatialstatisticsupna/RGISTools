earthdataOpenSearchQuery<-function(startDate,
                                   endDate,
                                   extent,
                                   product,
                                   collection=NULL,
                                   keyword=NULL,
                                   instrument=NULL,
                                   satellite=NULL,
                                   parentIdentifier=NULL,
                                   versionId=NULL,
                                   dataCenter=NULL,
                                   geometry=NULL,
                                   placeName=NULL,
                                   uid=NULL,
                                   numberOfResults=100,
                                   cursor=1,
                                   commit="Search",
                                   clientId="our_html_ui",
                                   verbose=F){
  stopifnot(class(startDate)=="Date")
  stopifnot(class(endDate)=="Date")
  osquery<-paste0(getRGISToolsOpt("EARTHDATA.opensearch"),
                  "/granules?utf8=%E2%9C%93",
                  "&clientId=",clientId,
                  "&keyword=",keyword,
                  "&instrument=",instrument,
                  "&satellite=",satellite,
                  "&parentIdentifier=",parentIdentifier,
                  "&shortName=",product,
                  "&versionId=",versionId,
                  "&dataCenter=",dataCenter,
                  "&startTime=",format(startDate,"%Y-%m-%d"),"T00%3A00%3A00Z",
                  "&endTime=",format(endDate,"%Y-%m-%d"),"T00%3A00%3A00Z",
                  "&spatial_type=bbox",
                  "&boundingBox=",paste0(c(bbox(extent)),collapse = ","),
                  "&geometry=",geometry,
                  "&placeName=",placeName,
                  "&uid=",uid,
                  "&numberOfResults=",numberOfResults,
                  "&cursor=",cursor,
                  "&commit=",commit)

  if(verbose){
    message(paste0("Search query: ",osquery))
  }
  return(osquery)
}


downloadClouds<-function(startDate,
                         endDate,
                         extent,
                         collection=NULL,
                         product,
                         verbose=F,
                         overwrite=F,
                         ...){
  arg<-list(...)
  AppRoot<-defineAppRoot(...)
  #create the earthdata search query
  loc<-earthdataOpenSearchQuery(startDate,endDate,extent,product="MOD06_L2")
  #perform the search
  c.handle = new_handle()
  req <- curl(loc, handle = c.handle)
  html<-readLines(req)
  cloudres<-gsub("\"/>","",gsub('.*href=\\"',"",html[grepl("https://ladsweb.modaps.eosdis.nasa.gov/archive/",html)]))
  cloudres<-cloudres[grepl(".hdf",cloudres)]
  hdf.dir<-file.path(AppRoot,"hdf")
  dir.create(hdf.dir,recursive = T,showWarnings = F)
  for(l in cloudres){
    curl_download(l, destfile=paste0(hdf.dir,"/",basename(l)),handle = c.handle)
  }
  close(req)
  tif.dir<-file.path(AppRoot,"tif")
  dir.create(tif.dir,recursive = T,showWarnings = F)
  hdf.files<-list.files(hdf.dir,recursive = T,full.names = T,pattern = "\\.hdf$")
  modExtractHDF(hdf.files,AppRoot=tif.dir,overwrite=overwrite)
  modExtractHDF(list.files("CloudRemoval/MOD35/hdf",full.names = T,recursive = T),
                AppRoot=tif.dir,
                bFilter=c("Cloud_Mask"),
                overwrite=overwrite,
                s_srs=CRS("+init=epsg:4326")
  )
}

modGetHour<-function(str){
  return(gsub("\\.","",gsub(".*\\s*(\\.\\d{4}\\.).*", "\\1", str)))
}
