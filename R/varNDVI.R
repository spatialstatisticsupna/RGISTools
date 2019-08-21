#' Calculate the normalized difference vegetation index (NDVI)
#'
#' \code{varNDVI} computes the normalized difference vegetation index (NDVI)
#' from the red an near-infrared (NIR) bands.
#'
#' The normalized difference vegetation index (NDVI) is the most widely used 
#' index for monitoring vegetation dynamics. The NDVI reflex vegetation vigour
#' and it is closed related to the amount of photosynthetically active radiation
#' absorbed \insertCite{rouse1972monitoring}{RGISTools}. This function is used
#' within \code{\link{ls7FolderToVar}}, \code{\link{ls8FolderToVar}},
#' \code{\link{modFolderToVar}} and \code{\link{senFolderToVar}}.
#'
#' @references \insertRef{rouse1972monitoring}{RGISTools}
#'
#' @param red a \code{raster} with the red band of the capture.
#' @param nir a \code{raster} with the NIR band of the capture.
#'
#' @return A NDVI image in \code{raster} format.
#'
#' @examples
#' # path to the cropped and cutted MODIS images for the region of Navarre
#' img.dir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all the tif files
#' img.files <- list.files(img.dir, pattern="\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print the MOD09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' 
#' # select the red and NIR bands
#' red <- raster(img.files[1])
#' nir <- raster(img.files[2])
#' # calculate the NDVI image
#' ndvi <- varNDVI(red,nir)
#' # plot the image
#' spplot(ndvi,col.regions=rev(terrain.colors(20)))
varNDVI<-function(red, nir){
  ndvi <- (nir - red) / (nir + red)
  return(ndvi)
}
