#' Calculates the enhanced vegetation index (EVI) from raster bands
#'
#' \code{varEVI} computes the the EVI index from blue, nir and red bands.
#'
#' The Enhanced Vegetation Index (EVI) is a vegetation indicator that improves sensitivity 
#' towards high biomass densities compared to NDVI.
#' This function is used within
#' \code{\link{ls7FolderToVar}}, \code{\link{ls8FolderToVar}}, \code{\link{modFolderToVar}} and \code{\link{senFolderToVar}}.
#'
#' @references \insertRef{huete2002overview}{RGISTools}
#'
#' @param blue the blue band of the capture in \code{raster} format.
#' @param red the red band of the capture in \code{raster} format.
#' @param nir the nir band of the capture in \code{raster} format.
#'
#' @return EVI in \code{raster} format.
#'
#' @examples
#' # dir path of cropped and cutted Modis image in the region of navarre as example
#' img.dir <- system.file("ExNavarra", package = "RGISTools")
#' # list all tif files
#' img.files <- list.files(img.dir, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
#' # select the red, blue and nir bands
#' red <- raster(img.files[1])
#' blue <- raster(img.files[3])
#' nir <- raster(img.files[2])
#' # calculate the evi image
#' evi <- varEVI(blue, red, nir)
#' # plot the image
#' spplot(evi)
varEVI<-function(blue,red,nir){
  evi <- 2.5*((nir - red) / (nir+6 * red-7.5*blue+1))
  return(evi)
}
