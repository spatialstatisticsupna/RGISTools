lsEspaGetOrders<-function(username=NULL,password=NULL,c.handle=NULL){
  if(is.null(c.handle)){
    if(is.null(username)|is.null(username)){
      stop("c.handle or username and password are null.")
    }else{
      stopifnot(class(username)=="character")
      stopifnot(class(password)=="character")
      c.handle<-lsEspaCreateConnection(username,password)
    }
  }
  r <- curl_fetch_memory(paste0(getRGISToolsOpt("LS.ESPA.API"),getRGISToolsOpt("LS.ESPA.API.v"),"/list-orders"), c.handle)
  order.list<-fromJSON(rawToChar(r$content))
  return(order.list)
}