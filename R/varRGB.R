#' Generates an RGB image from 3 spectral bands
#'
#' \code{varRGB} creates RGB \code{RasterStack} scaling the range of the images to 0-255 color range.
#'
#' The function rescales the original reflectance values to a range of 0-255. The functions
#' rear ranges the RGB bands to create a stack with a RGB image ready to visualize with plotRGB.
#' Bands may contain reflectance outliers which cause the image to look dark. Use the range
#' argument to remove the outliers and get a better-looking image.
#'
#' @param red the red band of the capture in \code{raster} format.
#' @param green the green band of the capture in \code{raster} format.
#' @param blue the blue band of the capture in \code{raster} format.
#' @param q.range a vector with the minimum and maximum reflectance quantiles being considered.
#' @param rPath file path were resulting RGB image is saved.
#' @param cutline \code{SpatialPolygonsDataFrame} for cutting the image by a region.
#'
#' @examples
#' # dir path of cropped and cutted Modis image in the region of navarre as example
#' img.dir <- system.file("ExNavarra", package = "RGISTools")
#' # list all tif files
#' img.files <- list.files(img.dir, pattern="\\.tif$", recursive = TRUE, full.names = TRUE)
#' # select the red, blue and nir bands
#' red <- raster(img.files[1])
#' blue <- raster(img.files[3])
#' green <- raster(img.files[4])
#'
#' q.range=c(0.001,0.999)
#' image<-varRGB(red,green,blue,q.range)
#' print(plotRGB(image))
varRGB<-function(red,green,blue,q.range=c(),rPath=NULL,cutline=NULL){
  rgb<-list(red,green,blue)
  names(rgb)<-c("red","green","blue")

  if(!is.null(q.range)){
    rgb<-lapply(rgb,FUN = function(r,q.range){q<-raster::quantile(r,q.range,na.rm=T);r<-genLimitRasterRange(r,mn=q[1],mx=q[2]);return(r)},q.range)
    names(rgb)<-c("red","green","blue")
  }
  rgb<-lapply(rgb,raster::stretch, minv=0, maxv=255)
  image<-raster::stack(rgb)
  if(!is.null(cutline)){
    cutline<-spTransform(cutline,CRSobj=crs(image))
    image<-raster::mask(image,cutline)
  }
  if(!is.null(rPath)){
    writeRaster(image,rPath)
  }else{
    return(image)
  }
}
