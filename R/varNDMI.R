#' calculates normalized difference moisture (water) index (NDMI)
#'
#' \code{varNDMI} computes the NDMI index from nir and swir1 bands.
#'
#' NDMI used to monitor changes in water content of leaves, using near-infrared (NIR) and short-wave infrared (SWIR).
#' This function is used within
#' \code{\link{ls7FolderToVar}}, \code{\link{ls8FolderToVar}}, \code{\link{modFolderToVar}} and \code{\link{senFolderToVar}}.
#'
#' @param nir the nir band of the capture in \code{raster} format
#' @param swir1 the swir1 band of the capture in \code{raster} format
#'
#' @return NDMI in \code{raster} format
#'
#' @examples
#' # dir path of cropped and cutted modis image in the region of navarre as example
#' img.dir <- system.file("ExNavarra", package = "RGISTools")
#' # list all tif files
#' img.files <- list.files(img.dir,pattern="\\.tif$",recursive = TRUE,full.names = TRUE)
#' #select the nir and swir1 bands
#' nir <- raster(img.files[2])
#' swir1 <- raster(img.files[6])
#' # calculate the ndmi image
#' ndmi <- varNDMI(nir,swir1)
#' # plot the image
#' spplot(ndmi)
varNDMI<-function(nir,swir1){
  ndmi<-(nir-swir1)/(nir+swir1)
  return(ndmi)
}
