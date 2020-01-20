#' Return the pathrow of a tile of MODIS images
#'
#' \code{modGetPathRow} reads the official name of a MODIS image and returns the
#' tile's path and row number, in 'hXXvYY' format (MODIS naming convention).
#'
#' @param str the full path(s) or official name(s) of the MODIS images from
#' which the tile's path and row numbers are retrieved.
#'
#' @return a string with the path and row in "\code{hXXvYY}" format.
#'
#' @examples
#' # getting the path and row number of the tile of a Landsat-8 image
#' files.mod <- "MYD09GA.A2003136.h17v04.005.2008324054225"
#' pr.mod <- modGetPathRow(files.mod)
#' print(pr.mod)
#'
modGetPathRow<-function(str){
  return(gsub(".*\\s*(h\\d{2}v\\d{2}).*", "\\1", str))
}
