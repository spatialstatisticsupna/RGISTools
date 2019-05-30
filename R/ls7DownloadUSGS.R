.ls7DownloadUSGS<-function(sceneID,
                           rfolder,
                           usgs.handler,
                           ...){
  arg<-list(...)
  if("verbose"%in%names(arg)){
    verbose=arg$verbose
  }else{verbose=F}
  url<-paste0(getRGISToolsOpt("USGS.url"),"download/12267/",sceneID,"/STANDARD/EE/")
  print(paste0("Downloading file in: ",rfolder,"/",sceneID,".tar.gz"))
  if(verbose){message(paste0("download url: ",url))}
  curl_download(url, destfile=paste0(rfolder,"/",sceneID,".tar.gz"),handle = usgs.handler)
}

