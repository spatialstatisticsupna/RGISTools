#' Limit the maximum and the minimum values of a raster
#'
#' \code{genLimitRasterRange} limits the maximum and the minimum values of a
#' \code{Raster*} class object to a given range.
#'
#' This is a generic function to limit the maximun and the minimun values in a
#' \code{Raster*} object.
#'
#' @param r \code{Raster*} type object.
#' @param mx maximun value in the \code{Raster*}.
#' @param mn minimun value in the \code{Raster*}.
#' @param rm.values logical argument. If \code{FALSE}, the \code{mx} and 
#' \code{mn} values are assigned to the pixels above or below the \code{mx} and 
#' \code{mn} thresholds respectively. If \code{TRUE}, values outside the 
#' \code{mn} - \code{mx} range are replaced by \code{NA}.
#'
#' @return a \code{Raster*} with values comprised within a range.
#'
#' @examples
#' # generate random images
#' img <- matrix(1:16, ncol = 4, byrow = TRUE)
#' r <- raster(img)
#' # assign the limit of the data in the raster stack
#' r2 <- genLimitRasterRange(r, mn = 4, mx = 10)
#' r3 <- genLimitRasterRange(r, mn = 4, mx = 10, rm.values = TRUE)
#' # plot limited data
#' spplot(stack(r, r2 ,r3))
genLimitRasterRange<-function(r,mx=NULL,mn=NULL,rm.values=FALSE){
  message("Function deprecated, use 'clamp' from 'raster' package")
  stopifnot(!(is.null(mx)&is.null(mn)))
  if(rm.values){
    r<-clamp(r,lower=mn,upper=mx)
    #mx.val<-NA
    #mn.val<-NA
  }else{
    r<-clamp(r,lower=mn,upper=mx,useValues=FALSE)
    #mx.val<-mx
    #mn.val<-mn
  }
  # if(!is.null(mx)){
  #   #r<-calc(r,fun = function(x,mx.val=mx.val){x[x>mx.val]<-mx.val;return(x)})
  #   
  #    
  #   #r[r>mx]<-mx.val
  # }
  # if(!is.null(mn)){
  #   r<-calc(r,fun = function(x,mn.val){x[x>mn.val]<-mn.val;return(x)},mn.val=mn)
  #   #r[r<mn]<-mn.val
  # }
  return(r)
}
