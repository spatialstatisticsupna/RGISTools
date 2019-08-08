genCheckMD5<-function(path.file,oficial.md5,verbose=FALSE,...){
  file.md5<-md5sum(path.file)
  file.md5<-toupper(file.md5)
  if(file.md5==oficial.md5){
    if(verbose){
      print(paste0("File md5:",file.md5))
      print(paste0("Oficial md5:",oficial.md5))
      print("Return TRUE")
    }
    return(TRUE)
  }else{
    if(verbose){
      print(paste0("File md5:",file.md5))
      print(paste0("Oficial md5:",oficial.md5))
      print("Return FALSE")
    }
    return(FALSE)
  }
}
