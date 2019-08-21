#' Updates the status of a request made to ESPA
#' 
#' \code{lsEspaUpdateOrders} checks the current status of a request made to EROS
#' Centre Science Processing Architecture (ESPA) to pre-process Landsat level-1
#' images
#' 
#' This function is part of a group of functions used to pre-process Landsat
#' level 1 images. The pre-processing is carried out by ESPA on demand. 
#' \code{lsEspaUpdateOrders} uses the ID numbers gathered by 
#' \code{\link{lsEspaGetOrderImages}} regarding previous order requests
#' to check the processing status. The function has to be run repeatedly 
#' until the status message states “complete”. All the status messages and their 
#' interpretation can be found in the ESPA's API 
#' \href{https://www.usgs.gov/media/files/eros-science-processing-architecture-demand-interface-user-guide}{User Guide}.
#' 
#' @param orders a list of the requested orders as returned by
#' \code{\link{lsEspaGetOrderImages}}.
#' @param username USGS's `EarthExplorer' username.
#' @param password USGS's `EarthExplorer' password.
#' @param c.handle a curl handler created with the package `\code{curl}' to
#' stablish a connection with a preset password and username. This argument
#' is mandatory if \code{username} and \code{password} are not defined.
#' @param verbose logical argument. If \code{TRUE}, the function prints the
#' running steps and warnings.
#'
#' @examples
#' \dontrun{
#' src <- paste0(tempdir(),"/Path_for_downloading_folder")
#' # search Landsat 7 level-1
#' search.res <- ls7Search(startDate = as.Date("01-01-2017", "%d-%m-%Y"),
#'                         endDate = as.Date("15-01-2017", "%d-%m-%Y"),
#'                         lonlat = c(-1.64323, 42.81687),
#'                         AppRoot = src)
#' # request to ESPA the prepocessing of level-1 images to get the surface reflectance
#' orders <- lsEspaOrderImages(search.res = search.res,
#'                             username = "username", 
#'                             password = "password", 
#'                             product = 'sr',
#'                             verbose = FALSE)
#' # get an ID for our request
#' orders <- lsEspaGetOrderImages(username = "username", 
#'                                password = "password")
#' # follow up the status of the request
#' lsEspaUpdateOrders(orders = orders,
#'                    username = "username", 
#'                    password = "password")
#' }
lsEspaUpdateOrders<-function(orders,username=NULL,password=NULL,c.handle=NULL,verbose=FALSE){
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
  new.orders<-new.orders[!new.orders%in%names(orders)]
  if(length(new.orders)>0){
    new.images<-lsEspaGetOrderImages(order.list=new.orders,c.handle=c.handle,verbose=verbose)
    orders<-append(orders,new.images)
  }
  for(order in 1:length(orders)){
    if(orders[[order]]$Status%in%c("ordered")){
      r <- curl_fetch_memory(paste0(getRGISToolsOpt("LS.ESPA.API"),getRGISToolsOpt("LS.ESPA.API.v"),"/order/",names(orders)[order]), 
                             c.handle)
      json_data<-fromJSON(rawToChar(r$content))
      if(json_data$note==getRGISToolsOpt("LS.ESPA.Request")){
        all.response<-unlist(json_data,recursive=TRUE)
        orders[[order]]<-list(OrderedImages=unname(all.response[grepl("inputs",names(all.response))]),
                              Status=json_data$status)
      }
    }
  }
  return(orders)
}