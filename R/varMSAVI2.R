#' calculates the modified soil-adjusted vegetation index (MSAVI2) from raster bands
#'
#' \code{varMSAVI2} computes the the MSAVI index from nir and red bands.
#'
#' MSAVI is soil adjusted vegetation indices that seek to address some of the limitation of NDVI when
#' applied to areas with a high degree of exposed soil surface. The problem with the original
#' soil-adjusted vegetation index (SAVI) is specifing the
#' correction factor (L) through trial-and-error based. MSAVI2 is an evolution MSAVI to more
#' reliably and simply calculate a soil brightness correction factor.
#' This function is used within
#' \code{\link{ls7FolderToVar}}, \code{\link{ls8FolderToVar}}, \code{\link{modFolderToVar}} and \code{\link{senFolderToVar}}.
#'
#' @param nir the red band of the capture in \code{raster} format
#' @param red the nir band of the capture in \code{raster} format
#'
#' @return MSAVI2 in \code{raster} format
#'
#' @examples
#' # dir path of cropped and cutted modis image in the region of navarre as example
#' img.dir <- system.file("ExNavarra", package = "RGISTools")
#' # list all tif files
#' img.files <- list.files(img.dir,pattern="\\.tif$",recursive = TRUE,full.names = TRUE)
#' #select the red and nir bands
#' red <- raster(img.files[1])
#' nir <- raster(img.files[2])
#' # calculate the msavi2 image
#' msavi2 <- varMSAVI2(red,nir)
#' # plot the image
#' spplot(msavi2)
varMSAVI2<-function(nir, red){
  msavi<-(2*nir+1-sqrt((2*nir+1)^2-8*(nir-red)))/2
  return(msavi)
}
