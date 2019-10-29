#' Calculate the modified soil-adjusted vegetation index (MSAVI2)
#'
#' \code{varMSAVI2} computes the modified soil-adjusted vegetation index 2
#' (MSAVI2) from the near-infrared (NIR) and red bands.
#'
#' The modified soil adjusted vegetation index 2 (MSAVI2) is a vegetation 
#' indicator that removes the effect from background variations
#' \insertCite{qi1994modified}{RGISTools}. This function is used within
#' \code{\link{ls7FolderToVar}}, \code{\link{ls8FolderToVar}},
#' \code{\link{modFolderToVar}} and \code{\link{senFolderToVar}}.
#'
#' @references \insertRef{qi1994modified}{RGISTools}
#'
#' @param red a \code{raster} with the red band of the capture.
#' @param nir a \code{raster} with the NIR band of the capture.
#'
#' @return A MSAVI2 image in \code{raster} format.
#'
#' @examples
#' # path to the cropped and cutted MODIS images for the region of Navarre
#' wdir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all the tif files
#' files.mod <- list.files(wdir, pattern="\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print the MOD09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' # select the red and NIR bands
#' img.mod.red <- raster(files.mod[1])
#' img.mod.nir <- raster(files.mod[2])
#' # calculate the MSAVI2 image
#' img.mod.msavi2 <- varMSAVI2(img.mod.red, img.mod.nir)
#' # plot the image
#' spplot(img.mod.msavi2,col.regions=rev(topo.colors(20)))
varMSAVI2<-function(red, nir){
  msavi<-(2*nir+1-sqrt((2*nir+1)^2-8*(nir-red)))/2
  return(msavi)
}
