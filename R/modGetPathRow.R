#' Returns the pathrow of Modis tile in "hXXvYY" string format
#'
#' \code{\link{modGetPathRow}} reads the official name of a modis image and returns the path and row data
#' as Modis name convention of horizontal and vertical "\code{hXXvYY}".
#'
#' @param str the full path or official image name of the Modis image from which the path and row is extracted.
#'
#' @return an string with the path and row of the image in "\code{hXXvYY}" format.
#'
#' @examples
#' #example of getting date from Landsat-8 image name
#' str<-"MYD09GA.A2003136.h17v04.005.2008324054225"
#' pr<-modGetPathRow(str)
#' print(pr)
#'
modGetPathRow<-function(str){
  return(gsub(".*\\s*(h\\d{2}v\\d{2}).*", "\\1", str))
}
