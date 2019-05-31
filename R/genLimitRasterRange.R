#' Limits the maximum and minimum values of a raster image
#'
#' \code{genLimitRasterRange} limits the maximum and minimum values of a raster image to a given range.
#'
#' This is a generic function to limit the maximun and minimun values in a raster image.
#'
#' @param r \code{Raster}* type object.
#' @param ... argument to allow function nestering.
#' \itemize{
#'   \item \code{mx} maximun value in raster.
#'   \item \code{mn} minimun value in raster.
#'   \item \code{AppRoot} the path where the RData will be saved.
#' }
#'
#' @examples
#' #generate random images
#' img <- matrix(1:16, ncol = 4, byrow = TRUE)
#' r<-raster(img)
#' #asign the limit of the data in the raster stack
#' r2<-genLimitRasterRange(r,mn=4,mx=10)
#' #plot limited data
#' spplot(stack(r,r2))
genLimitRasterRange<-function(r,...){
  arg<-list(...)
  stopifnot("mx"%in%names(arg)|
            "mn"%in%names(arg))
  if("mx"%in%names(arg))
    r[r>arg$mx]<-arg$mx
  if("mn"%in%names(arg))
    r[r<arg$mn]<-arg$mn
  return(r)
}
