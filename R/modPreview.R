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
#' modPreview(searchres, 1)
#' modPreview(searchres, 1, size = 600)
#' }
modPreview<-function(searchres,n,size=NULL){
  ser<-searchres[n]
  tmp <- tempfile()
  download.file(ser,tmp,mode="wb")
  pic<-image_read(tmp)
  pic <- image_resize(pic, size)
  message(pic)
  file.remove(tmp)
  message(paste0("Printing the image ",basename(ser),"."))
}
