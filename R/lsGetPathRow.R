#' Return the pathrow of a tile of Landsat-7 or Landsat-8 images
#'
#' \code{lsGetPathRow} reads the official name of a Landsat-7 or Landsat-8 image
#' and returns the path and row of its tile.
#'
#' @param str path to the folder with the Landsat-7 or 
#' Landsat-8 multispectral images.
#'
#' @return a string with the path and row in "\code{ppprrr}" format.
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
