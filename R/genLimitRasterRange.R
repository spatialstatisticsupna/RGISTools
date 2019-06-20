#' Limits the maximum and minimum values of a raster image
#'
#' \code{genLimitRasterRange} limits the maximum and the minimum values of a raster image to a given range.
#'
#' This is a generic function to limit the maximun and the minimun values in a \code{raster} image.
#'
#' @param r \code{Raster}* type object.
#' @param mx maximun value in the \code{raster}.
#' @param mn minimun value in the \code{raster}.
#' @param rm.values logical argument. If \code{FALSE} \code{mx} and  \code{mn} values are asigned to the 
#'   higher values than \code{mx} and lower values than \code{mn} respectively. If \code{TRUE}, values out of 
#'   \code{mn}-\code{mx} are removed.
#'
#' @examples
#' # generate random images
#' img <- matrix(1:16, ncol = 4, byrow = TRUE)
#' r <- raster(img)
#' # asign the limit of the data in the raster stack
#' r2 <- genLimitRasterRange(r, mn = 4, mx = 10)
#' r3 <- genLimitRasterRange(r, mn = 4, mx = 10, rm.values = TRUE)
#' # plot limited data
#' spplot(stack(r, r2 ,r3))
genLimitRasterRange<-function(r,mx=NULL,mn=NULL,rm.values=FALSE){
  stopifnot(!(is.null(mx)&is.null(mn)))
  if(rm.values){
    mx.val<-NA
    mn.val<-NA
  }else{
    mx.val<-mx
    mn.val<-mn
  }
  if(!is.null(mx)){
    r[r>mx]<-mx.val
  }
  if(!is.null(mn)){
    r[r<mn]<-mn.val
  }
  return(r)
}
