#' Allows Modis satellite images preview before its download
#'
#' \code{modPreview} shows a preview of the n image from a set of search results.
#'
#' The function shows a preview of an image resulting from a search in Modis imagery metadata.
#' A search with \code{\link{modSearch}} has to be carried out before proceeding with the preview.
#' The preview is downloaded from Modis website. Please, be aware that only some images have this feature.
#'
#'
#' @param searchres a vector with the results from a search of Modis images provided by the functions modSearch with \code{resType="browseurl"} argument.
#' @param n the number of the image of interest in the search vector.
#' @param size a number specifying the size of the preview to be displayed, in pixels.
#'
#' @examples
#' \dontrun{
#' data(ex.navarre)
#' # Retrieve jpg images covering Navarre region between 2011 and 2013
#' searchres <- modSearch(product = "MOD09GA",
#'                       startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                       endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                       collection = 6,
#'                       resType = "browseurl",
#'                       extent = ex.navarre)
#' modPreview(searchres, 1)
#' modPreview(searchres, 1, size=600)
#' }
modPreview<-function(searchres,n,size=NULL){
  ser<-searchres[n]
  tmp <- tempfile()
  download.file(ser,tmp,mode="wb")
  pic<-image_read(tmp)
  pic <- image_resize(pic, size)
  print(pic)
  file.remove(tmp)
  message(paste0("Printing the image ",basename(ser),"."))
}
