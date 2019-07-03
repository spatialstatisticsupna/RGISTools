#' Calculate the modified soil-adjusted vegetation index (MSAVI2) from raster bands
#'
#' \code{varMSAVI2} computes the the MSAVI index from nir and red bands.
#'
#' The Modified Soil Adjusted Vegetation Index 2 (MSAVI2) is a vegetation indicator that 
#' removes the effect of background variations \insertCite{qi1994modified}{RGISTools}.
#' This function is used within
#' \code{\link{ls7FolderToVar}}, \code{\link{ls8FolderToVar}}, \code{\link{modFolderToVar}} and \code{\link{senFolderToVar}}.
#'
#' @references \insertRef{qi1994modified}{RGISTools}
#'
#' @param red the red band of the capture in \code{raster} format.
#' @param nir the nir band of the capture in \code{raster} format.
#'
#' @return MSAVI2 in \code{raster} format.
#'
#' @examples
#' # dir path of cropped and cutted Modis image in the region of Navarre as example
#' img.dir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all tif files
#' img.files <- list.files(img.dir, pattern="\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print Modis 09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' # select the red and nir bands
#' red <- raster(img.files[1])
#' nir <- raster(img.files[2])
#' # calculate the msavi2 image
#' msavi2 <- varMSAVI2(red, nir)
#' # plot the image
#' spplot(msavi2,col.regions=rev(topo.colors(20)))
varMSAVI2<-function(red, nir){
  msavi<-(2*nir+1-sqrt((2*nir+1)^2-8*(nir-red)))/2
  return(msavi)
}
