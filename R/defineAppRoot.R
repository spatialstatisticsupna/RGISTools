defineAppRoot<-function(...){
  arg<-list(...)
  if(!"AppRoot"%in%names(arg)){
    AppRoot=getRGISToolsOpt("AppRoot")
    if(is.null(AppRoot))
      AppRoot=getwd()
  }else{
    AppRoot=arg$AppRoot
  }
  return(AppRoot)
}
as.Date<-function(x,...){
  if(is.numeric(x)){
    return(base::as.Date("1970-01-01")+x)
  }else{
    return(base::as.Date(x,...))
  }
}
pathWinLx<-function(str){
  if(substr(str,1,2)=="\\"){
    return(paste0("\\",gsub("\\","/",substr(str,3,nchar(str)),fixed = TRUE)))
  }else{
    return(gsub("\\","/",str,fixed = TRUE))
  }
}
