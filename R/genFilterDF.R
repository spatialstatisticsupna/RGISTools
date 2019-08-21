genFilterDF<-function(df,verbose=FALSE,...){
  arg<-list(...)
  if(verbose)
    message(paste0("[",paste(names(arg)[!names(arg)%in%names(df)], collapse=", "),
                 "] arguments not found and removed from filter."))
  arg<-arg[names(arg)%in%names(df)]
  for(a in names(arg)){
    df<-df[unlist(df[a])==unlist(arg[a]),]
  }
  return(df)
}
