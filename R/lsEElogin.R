loginEEAPI<-function(username,password){
  jsonquery<-NULL
  jsonquery$username<-username
  jsonquery$password<-password
  jsonquery$authType<-"EROS"
  jsonquery$catalogId<-"EE"
  post.res <- POST(url = paste0(getRGISToolsOpt("LS.EE.API"), "login"),
                   body = paste0('jsonRequest=',toJSON(jsonquery)),
                   content_type("application/x-www-form-urlencoded; charset=UTF-8"))
  res <- content(post.res)
  if(res$error!=""){
    stop(res$error)
  }
  message('Logged in to EE API.')
  setRGISToolsOpt("LS.EE.KEY",res$data)
}


logoutEEAPI<-function(){
  jsonquery<-NULL
  jsonquery$apikey<-getRGISToolsOpt("LS.EE.KEY")
  if(!is.null(jsonquery$apikey)){
    post.res <- POST(url = paste0(getRGISToolsOpt("LS.EE.API"), "logout"),
                     body = URLencode(paste0('jsonRequest=',toJSON(jsonquery))),
                     content_type("application/x-www-form-urlencoded; charset=UTF-8"))
    res <- content(post.res)
    if(res$error!=""){
      message('Logged out from EE API.')
      setRGISToolsOpt("LS.EE.KEY",NULL)
    }else{
      stop(res$error)
    }
  }else{
    message('You are not logged in EE API.') 
  }
}
