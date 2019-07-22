#' Title
#'
#' @param username 
#' @param password 
#' @param c.handle 
#'
#' @return
#'
#' @examples
lsEspaGetOrderImages<-function(username=NULL,password=NULL,c.handle=NULL,order.list=NULL,verbose=TRUE){
  if(is.null(c.handle)){
    if(is.null(username)|is.null(username)){
      stop("c.handle or username and password are null.")
    }else{
      stopifnot(class(username)=="character")
      stopifnot(class(password)=="character")
      c.handle<-lsEspaCreateConnection(username,password)
    }
  }
  if(is.null(order.list))
    order.list<-lsEspaGetOrders(c.handle=c.handle)
  img.list<-list()
  for(ol in order.list){
    r <- curl_fetch_memory(paste0(getRGISToolsOpt("LS.ESPA.API"),getRGISToolsOpt("LS.ESPA.API.v"),"/order/",ol), c.handle)
    json_data<-fromJSON(rawToChar(r$content))
    if(json_data$note==getRGISToolsOpt("LS.ESPA.Request")){
      all.response<-unlist(json_data,recursive=T)
      img.list[[ol]]<-list(OrderedImages=unname(all.response[grepl("inputs",names(all.response))]),
                           Status=json_data$status)
    }else{
      if(verbose)message(paste0(ol," is not an RGISTools request, not adding for downloading..."))
    }
  }
  return(img.list)
}