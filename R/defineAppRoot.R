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
  res<-c()
  for(str1 in str){
    if(substr(str1,1,2)=="\\"){
      res<-c(res,paste0("\\",gsub("\\","/",substr(str1,3,nchar(str1)),fixed = TRUE)))
    }else{
      res<-c(res,gsub("\\","/",str1,fixed = TRUE))
    }
  }
  return(res)
}
