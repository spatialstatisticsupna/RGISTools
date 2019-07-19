lsEarthExplorerdownload<-function(searchres,username,password,cookies.file,untar,downDir,AppRoot,downPath,verbose,overwrite){
  #start usgs session
  handler<-startUSGSsession(username,password,cookies.file,verbose)
  if(verbose)
    print("USGS session started, downloading images...")
  for(scene in searchres$sceneID){
    if(!file.exists(paste0(downPath,"/",scene,".tar.gz"))){
      if(grepl("LC8",searchres[1,]$sceneID)){
        .ls8DownloadUSGS(scene,downPath,handler,verbose=verbose,overwrite=overwrite)
      }else if(grepl("LE7",searchres[1,]$sceneID)){
        .ls7DownloadUSGS(scene,downPath,handler,verbose=verbose,overwrite=overwrite)
      }
    }
    #Unzip in downDir when available
    if(untar){
      print(paste0("Untar ",scene," file."))
      untarDir<-file.path(AppRoot,downDir,"untar",scene)
      if(overwrite){
        file.remove(untarDir,showWarnings=FALSE,recursive=TRUE)
      }
      dir.create(untarDir,recursive=T,showWarnings=FALSE)
      untar(paste0(downPath,"/",scene,".tar.gz"),exdir=untarDir)
      #Flag is true, so remove compressed files
      if(raw.rm){
        file.remove(paste0(downPath,"/",scene,".tar.gz"))
      }
    }
  }
}