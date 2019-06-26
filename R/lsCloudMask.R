# src<-"D:/Downscaling/Landsat8/untar"
# library(RGISTools)
# getRGISToolsOpt("SEN2BANDS")
# sensitivity 0-1
# lsCloudMask(src,overwrite=T)
lsCloudMask<-function(src,sensitivity=2800,overwrite=FALSE,verbose=F,...){
  arg<-list(...)
  AppRoot<-defineAppRoot(...)
  imgdir.list<-list.dirs(src)[-1]

  for(id in imgdir.list){
    #id<-imgdir.list[2]
    tif.list<-list.files(id,pattern = "\\.tif$",full.names = T,ignore.case = T)
    qc<-getRGISToolsOpt("LS8BANDS")["quality"]
    qcmask<-tif.list[grepl(qc,tif.list)]
    if(length(qcmask)==0){
      message(paste0("No cloud mask found for date ",genGetDates(basename(id))))
      next
    }
    out.img<-gsub(paste0(qc,".tif"),"CLD.tif",qcmask,ignore.case = T)
    
    if(!file.exists(out.img)|overwrite){
      message("Creating cloud mask for tile ",dirname(qcmask))
      
      ras.cloud<-readAll(raster(qcmask))
      mn<-minValue(ras.cloud)
      if(verbose){
        message(paste0("Minimun: ",mn))
      }
      ras.cloud[ras.cloud<=mn]<-NA
      ras.cloud[ras.cloud<sensitivity]<-NA
      ras.cloud[ras.cloud>=sensitivity]<-1
      
      print(spplot(ras.cloud))
      writeRaster(ras.cloud,out.img,overwrite=overwrite)
      rm(ras.cloud)
    }else{
      message(paste0("Cloud mask of date ",genGetDates(basename(id))," already exists."))
    }
  }
  message(paste0("Cloud mask in ",src," for all the tiles."))
}
