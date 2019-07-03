#' Return the pathrow of Landsat tile in "\code{hXXvYY}" string format
#'
#' \code{lsGetPathRow} reads the name of a Lansat 7 or 8 image and returns the path and row.
#'
#' @param str the full path or name of the Lansat 7 or 8 image from which the path and row will be extracted.
#'
#' @return an string with the path and row of the image in "\code{ppprrr}" format.
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
