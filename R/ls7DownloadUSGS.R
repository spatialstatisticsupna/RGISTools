.ls7DownloadUSGS<-function(sceneID,
                           rfolder,
                           usgs.handler,
                           overwrite=FALSE,
                           ...){
  arg<-list(...)
  if("verbose"%in%names(arg)){
    verbose=arg$verbose
  }else{verbose=F}
  url<-paste0(getRGISToolsOpt("USGS.url"),"download/12267/",sceneID,"/STANDARD/EE/")
  pth<-paste0(rfolder,"/",sceneID,".tar.gz")
  if(overwrite){
    file.remove(pth,showWarnings=FALSE)
  }
  print(paste0("Downloading file in: ",pth))
  if(verbose){message(paste0("download url: ",url))}
  curl_download(url, destfile=pth,handle = usgs.handler)
}

