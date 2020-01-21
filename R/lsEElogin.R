loginEEAPI<-function(username,password,verbose=FALSE){
  jsonquery<-list("username"=username,
                  "password"=password,
                  "authType"="EROS",
                  "catalogId"="EE")
  post.res <- POST(url = paste0(getRGISToolsOpt("LS.EE.API"), "login"),
                   body = paste0('jsonRequest=',toJSON(jsonquery)),
                   content_type("application/x-www-form-urlencoded; charset=UTF-8"))
  res <- content(post.res)
  if(res$error!=""){
    stop(res$error)
  }
  if(verbose)message('Logged in to EE API.')
  setRGISToolsOpt("LS.EE.KEY",res$data)
}


logoutEEAPI<-function(verbose=FALSE){
  jsonquery<-list("apikey"=getRGISToolsOpt("LS.EE.KEY"))
  if(!is.null(jsonquery$apikey)){
    post.res <- POST(url = paste0(getRGISToolsOpt("LS.EE.API"), "logout"),
                     body = URLencode(paste0('jsonRequest=',toJSON(jsonquery))),
                     content_type("application/x-www-form-urlencoded; charset=UTF-8"))
    res <- content(post.res)
    if(res$error!=""){
      if(verbose)message('Logged out from EE API.')
      setRGISToolsOpt("LS.EE.KEY",NULL)
    }else{
      stop(res$error)
    }
  }else{
    if(verbose)message('You are not logged in EE API.') 
  }
}
