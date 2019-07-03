#' Calculate the normalized difference vegetation index (NDVI) from raster bands
#'
#' \code{varNDVI} computes the the NDVI index from nir and red bands.
#'
#' The normalized difference vegetation index (NDVI) is the most widely used index for monitoring vegetation dynamics. 
#' The NDVI reflex vegetation vigour and it is closed related to the amount of photosynthetically 
#' absorbs active radiation \insertCite{rouse1972monitoring}{RGISTools}.
#' This function is used within
#' \code{\link{ls7FolderToVar}}, \code{\link{ls8FolderToVar}}, \code{\link{modFolderToVar}} and \code{\link{senFolderToVar}}.
#'
#' @references \insertRef{rouse1972monitoring}{RGISTools}
#'
#' @param red the red band of the capture in \code{raster} format.
#' @param nir the nir band of the capture in \code{raster} format.
#'
#' @return NDVI in \code{raster} format.
#'
#' @examples
#' # dir path of cropped and cutted Modis image in the region of Navarre as example
#' img.dir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all tif files
#' img.files <- list.files(img.dir, pattern="\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print Modis 09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' 
#' # select the red and nir bands
#' red <- raster(img.files[1])
#' nir <- raster(img.files[2])
#' # calculate the ndvi image
#' ndvi <- varNDVI(red,nir)
#' # plot the image
#' spplot(ndvi,col.regions=rev(terrain.colors(20)))
varNDVI<-function(red, nir){
  ndvi <- (nir - red) / (nir + red)
  return(ndvi)
}
