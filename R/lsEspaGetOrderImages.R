#' Gets a first response from ESPA regarding a recent request
#' 
#' \code{lsEspaGetOrderImages} obtains the identification number and the status
#' of the request from the EROS Centre Science Processing Architecture (ESPA).
#' 
#' This function is part of a group of functions used to pre-process Landsat
#' level-1 images. The pre-processing is carried out by ESPA on demand. 
#' \code{lsEspaGetOrderImages} takes the identification (ID) number of a
#' request carried out by \code{\link{lsEspaOrderImages}}. This ID is used to
#' follow up the processing status with \code{\link{lsEspaUpdateOrders}}. All the 
#' status messages and their interpretation can be found in the ESPA's API 
#' \href{https://www.usgs.gov/media/files/eros-science-processing-architecture-demand-interface-user-guide}{User Guide}.
#'
#' @param username USGS's `EarthExplorer' username.
#' @param password USGS's `EarthExplorer' password.
#' @param c.handle a curl handler created with the package `\code{curl}' to
#' stablish a connection with a preset password and username. This argument
#' is mandatory if \code{username} and \code{password} are not defined.
#' @param order.list a list of orders from \code{\link{lsEspaOrderImages}}
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#'
#' @examples
#' \dontrun{
#' wdir <- paste0(tempdir(),"/Path_for_downloading_folder")
#' # search Landsat 7 level-2
#' sres <- ls7Search(startDate = as.Date("01-01-2017", "%d-%m-%Y"),
#'                   endDate = as.Date("07-01-2017", "%d-%m-%Y"),
#'                   lonlat = c(-1.64323, 42.81687),
#'                   AppRoot = wdir)
#' # request to ESPA the pre-pocessing of level-2 images to
#' # get the surface reflectance
#' order <- lsEspaOrderImages(search.res = sres,
#'                            username = "username", 
#'                            password = "password", 
#'                            product = 'sr',
#'                            verbose = FALSE)
#' # get an ID for our request
#' lsEspaGetOrderImages(username = "username", 
#'                      password = "password")
#' }
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
      all.response<-unlist(json_data,recursive=TRUE)
      img.list[[ol]]<-list(OrderedImages=unname(all.response[grepl("inputs",names(all.response))]),
                           Status=json_data$status)
    }else{
      if(verbose)message(paste0(ol," is not an RGISTools request, not adding for downloading..."))
    }
  }
  return(img.list)
}