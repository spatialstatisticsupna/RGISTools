#' Generate an RGB image from 3 spectral bands
#'
#' \code{varRGB} creates red-green-blue (RGB) images as a \code{RasterStack} by
#' scaling the pixel values to 0-255 color range.
#'
#' The function rescales the original reflectance values to a range of 0-255.
#' The function re-arranges the RGB bands to create a stack ready to visualize
#' with plotRGB. Bands may contain outliers which cause the image to look dark.
#' Use the \code{q.range} argument to remove the outliers and get a 
#' better-looking image.
#'
#' @param red a \code{raster} with the red band of the capture.
#' @param green a \code{raster} with the green band of the capture.
#' @param blue a \code{raster} with the blue band of the capture.
#' @param q.range a vector with the minimum and maximum reflectance quantiles
#' being considered.
#' @param rPath the file path where the resulting RGB image is saved.
#' @param region a \code{Spatial*}, projected \code{raster*}, or \code{sf} class object 
#' defining the area of interest for image masking.
#'
#' @examples
#' # path to the cropped and cutted MODIS images for the region of Navarre
#' wdir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all the tif files
#' files.mod <- list.files(wdir, pattern="\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print the MOD09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' 
#' # select the red, blue and NIR bands
#' img.mod.red <- raster(files.mod[1])
#' img.mod.blue <- raster(files.mod[3])
#' img.mod.green <- raster(files.mod[4])
#'
#' q.range=c(0.001,0.999)
#' img.mod.rgb<-varRGB(img.mod.red,img.mod.green,img.mod.blue,q.range)
#' print(plotRGB(img.mod.rgb))
varRGB<-function(red,green,blue,q.range=c(),rPath=NULL,region=NULL){

  rgb<-list(red,green,blue)
  names(rgb)<-c("red","green","blue")
  bclass<-unlist(lapply(rgb,FUN = function(x){return(class(x)=="stars")}))
  if(bclass[1]){rgb[[1]]<-as(rgb[[1]],"Raster")}
  if(bclass[2]){rgb[[2]]<-as(rgb[[2]],"Raster")}
  if(bclass[3]){rgb[[3]]<-as(rgb[[3]],"Raster")}
  
  if(!is.null(q.range)){
    rgb<-lapply(rgb,FUN = function(r,q.range){q<-raster::quantile(r,q.range,na.rm=TRUE);r<-clamp(r,lower=q[1],upper=q[2]);return(r)},q.range)
    names(rgb)<-c("red","green","blue")
  }
  rgb<-lapply(rgb,raster::stretch, minv=0, maxv=255)
  image<-raster::stack(rgb)
  if(!is.null(region)){
    region<-transform_multiple_proj(region,projection(image))
    region<-as(region, 'Spatial')
    image<-raster::mask(image,region)
  }
  if(!is.null(rPath)){
    writeRaster(image,rPath)
  }else{
    if(any(bclass)){
      return(st_as_stars(image))
    }else{
      return(image)
    }
  }
}
