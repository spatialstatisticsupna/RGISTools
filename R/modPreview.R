#' Preview MODIS satellite images
#'
#' \code{modPreview} shows a preview of the \code{n}-th image from a set of 
#' search results.
#'
#' The function shows a preview of the \code{n}-th output image from a search
#' in the MODIS archives (\code{\link{modSearch}}), with 
#' \code{resType = "browseurl"}). The preview is downloaded from the
#' \href{https://earthdata.nasa.gov}{`EarthData' Platform}.
#' Please, be aware that only some images may have a preview.
#'
#' @param searchres a vector with the results from \code{\link{modSearch}}.
#' @param n a \code{numeric} argument identifying the location of the image in
#' \code{searchres}.
#' @param size a \code{numeric} argument specifying the size of the preview to
#' be displayed, in pixels.
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # retrieve jpg images covering Navarre region between 2011 and 2013
#' searchres <- modSearch(product = "MOD09GA",
#'                       startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                       endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                       collection = 6,
#'                       resType = "browseurl",
#'                       extent = ex.navarre)
#'                       
#' modPreview(searchres,1)
#' modPreview(searchres,2,add.Layer=T)
#' }
modPreview<-function(searchres,n,lpos=c(3,2,1),add.Layer=FALSE,showWarnings = FALSE,...){
  ser<-searchres[n]
  tmp <- tempfile()
  download.file(ser,tmp,mode="wb")
  pic<-stack(tmp)

  pr<-modGetPathRow(ser)
  ho<-as.numeric(substr(pr,2,3))
  ve<-as.numeric(substr(pr,5,6))
  
  extent(pic)<-extent(mod.tiles[mod.tiles$Name==paste0("h:",ho," v:",ve),])
  proj4string(pic)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
  
  if(showWarnings){
    return(genMapViewSession(pic,lpos,lname=paste0("MOD_",ho,"_",ve,"_D",format(modGetDates(ser),"%Y%j")),add.Layer=add.Layer,...))
  }else{
    return(suppressWarnings(genMapViewSession(pic,lpos,lname=paste0("MOD_",ho,"_",ve,"_D",format(modGetDates(ser),"%Y%j")),add.Layer=add.Layer,...)))
  }
}
