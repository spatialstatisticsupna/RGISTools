lsEarthExplorerdownload<-function(searchres,username,password,cookies.file,untar,raw.rm,downDir,AppRoot,downPath,verbose,overwrite,...){
  #start usgs session
  handler<-startUSGSsession(username,password,cookies.file,verbose)
  if(verbose)
    message("USGS session started, downloading images...")
  for(scene in searchres$sceneID){
    if(!file.exists(paste0(downPath,"/",scene,".tar.gz"))){
      if(grepl("LC8",searchres[1,]$sceneID)){
        .ls8DownloadUSGS(scene,downPath,handler,verbose=verbose,overwrite=overwrite)
      }else if(grepl("LE7",searchres[1,]$sceneID)){
        .ls7DownloadUSGS(scene,downPath,handler,verbose=verbose,overwrite=overwrite)
      }
    }else{
      message(paste0(scene," file already exists! Not downloading."))
    }
    #Unzip in downDir when available
    untarDir<-file.path(AppRoot,downDir,"untar",scene)
    if((untar&!file.exists(untarDir))|(untar&overwrite)){
      message(paste0("Untar ",scene," file."))
      if(overwrite){
        file.remove(untarDir,showWarnings=FALSE,recursive=TRUE)
      }
      dir.create(untarDir,recursive=TRUE,showWarnings=FALSE)
      
      if("bFilter"%in%names(arg)){
        flist<-untar(downPath,list=TRUE)
        flist<-flist[Reduce("|", lapply(paste0(arg$bFilter,"\\.TIF$"),grepl,flist))]
        untar(tarfile=downPath,
              files=flist,
              exdir = untarDir)
      }else{
        untar(paste0(downPath,"/",scene,".tar.gz"),exdir=untarDir)
      }
      
      
      
      #Flag is true, so remove compressed files
      if(raw.rm){
        file.remove(paste0(downPath,"/",scene,".tar.gz"))
      }
    }
  }
}