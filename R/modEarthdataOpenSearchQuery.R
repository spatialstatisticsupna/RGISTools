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
                                   verbose=FALSE){
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


modDownloadAtmosphere<-function(startDate,
                                     endDate,
                                     extent,
                                     product,#MOD35_L2
                                     collection=NULL,
                                     verbose=FALSE,
                                     overwrite=FALSE,
                                     ...){
  arg<-list(...)
  AppRoot<-defineAppRoot(...)
  #create the earthdata search query
  #startDate=as.Date("2017208","%Y%j");endDate=as.Date("2017218","%Y%j");extent=ex.navarre;
  loc<-earthdataOpenSearchQuery(startDate,endDate,extent,product=product)
  #perform the search
  c.handle = new_handle()
  req <- curl(loc, handle = c.handle)
  html<-readLines(req)
  # select the download urls
  cloudres<-gsub("\"/>","",gsub('.*href=\\"',"",html[grepl("https://ladsweb.modaps.eosdis.nasa.gov/archive/",html)]))
  cloudres<-cloudres[grepl(".hdf",cloudres)]
  # define download folder
  hdf.dir<-file.path(AppRoot,"hdf")
  dir.create(hdf.dir,recursive = TRUE,showWarnings = FALSE)
  # Download the images
  for(l in cloudres){
    out.hdf<-paste0(hdf.dir,"/",basename(l))
    if((!file.exists(out.hdf))||overwrite){
      message(paste0("Downloading file: ",basename(out.hdf)))
      curl_download(l, destfile=out.hdf,handle = c.handle)
    }else{
      message(paste0("File already exists! File: ",basename(out.hdf)))
    }
  }
  message(paste0("All the images downloaded in ",hdf.dir," directory."))
  close(req)
  # define tif folder
  tif.dir<-file.path(AppRoot,"tif")
  dir.create(tif.dir,recursive = TRUE,showWarnings = FALSE)
  hdf.files<-list.files(hdf.dir,recursive = TRUE,full.names = TRUE,pattern = "\\.hdf$")
  modExtractHDF(hdf.files,
                AppRoot=tif.dir,
                overwrite=overwrite,
                ...)
}

modGetHour<-function(str){
  return(gsub("\\.","",gsub(".*\\s*(\\.\\d{4}\\.).*", "\\1", str)))
}





