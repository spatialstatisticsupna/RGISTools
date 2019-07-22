#' Title
#'
#' @param images 
#' @param username 
#' @param password 
#' @param c.handle 
#'
#' @return
#'
#' @examples
lsEspaUpdateOrders<-function(images,username=NULL,password=NULL,c.handle=NULL){
  if(is.null(c.handle)){
    if(is.null(username)|is.null(username)){
      stop("c.handle or username and password are null.")
    }else{
      stopifnot(class(username)=="character")
      stopifnot(class(password)=="character")
      c.handle<-lsEspaCreateConnection(username,password)
    }
  }
  new.orders<-lsEspaGetOrders(c.handle=c.handle)
  new.orders<-new.orders[!new.orders%in%names(images)]
  if(length(new.orders)>0){
    new.images<-lsEspaGetOrderImages(new.orders,c.handle)
    images<-append(images,new.images)
  }
  for(order in 1:length(images)){
    if(images[[order]]$Status%in%c("ordered")){
      r <- curl_fetch_memory(paste0(getRGISToolsOpt("LS.ESPA.API"),getRGISToolsOpt("LS.ESPA.API.v"),"/order/",names(images)[order]), 
                             c.handle)
      json_data<-fromJSON(rawToChar(r$content))
      if(json_data$note==getRGISToolsOpt("LS.ESPA.Request")){
        all.response<-unlist(json_data,recursive=T)
        images[[order]]<-list(OrderedImages=unname(all.response[grepl("inputs",names(all.response))]),
                              Status=json_data$status)
      }
    }
  }
  return(images)
}