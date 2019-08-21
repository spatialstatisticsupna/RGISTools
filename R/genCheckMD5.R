genCheckMD5<-function(path.file,oficial.md5,verbose=FALSE,...){
  file.md5<-md5sum(path.file)
  file.md5<-toupper(file.md5)
  if(file.md5==oficial.md5){
    if(verbose){
      message(paste0("File md5:",file.md5))
      message(paste0("Oficial md5:",oficial.md5))
      message("Return TRUE")
    }
    return(TRUE)
  }else{
    if(verbose){
      message(paste0("File md5:",file.md5))
      message(paste0("Oficial md5:",oficial.md5))
      message("Return FALSE")
    }
    return(FALSE)
  }
}
