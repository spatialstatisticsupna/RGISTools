#' Return the pathrow of a tile of Landsat-7 or Landsat-8 images
#'
#' \code{lsGetPathRow} reads the official name of a Landsat-7 or Landsa-8 image
#'  and returns the tile’s path and row number, in "PPPRRR" format (Landsat
#'  naming convention).
#'
#' @param str the full path(s) or official name(s) of the Landsat-7 or Landsa-8
#' images from which the tile’s path and row numbers are retrieved.
#'
#' @return a string with the path and row in "\code{PPPRRR}" format.
#'
#' @examples
#' # example of getting date from Landsat-8 image name
#' str <- c("LE72000302011066ASN00",
#'          "LE72000302011066ASN00")
#' pr <- lsGetPathRow(str)
#' print(pr)
#'
lsGetPathRow<-function(str){
  str<-basename(str)
  return(substr(str,4,9))
}
