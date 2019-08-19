#' Calculate the enhanced vegetation index (EVI)
#'
#' \code{varEVI} computes the EVI index from the blue, nir and red bands.
#'
#' The Enhanced Vegetation Index (EVI) is a vegetation indicator that improves
#' sensitivity towards high biomass densities compared to NDVI 
#' \insertCite{huete2002overview}{RGISTools}. This function is used within
#' \code{\link{ls7FolderToVar}}, \code{\link{ls8FolderToVar}}, 
#' \code{\link{modFolderToVar}} and \code{\link{senFolderToVar}}.
#'
#' @references \insertRef{huete2002overview}{RGISTools}
#'
#' @param blue a \code{raster} with the blue band of the capture.
#' @param red a \code{raster} with the red band of the capture.
#' @param nir a \code{raster} with the NIR band of the capture.
#' @param scfun a function to re-scale the original pixel values into 
#' reflectance (0-1).
#'
#' @return An EVI image in \code{raster} format.
#'
#' @examples
#' # path to the cropped and cutted MODIS images for the region of Navarre
#' img.dir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all the tif files
#' img.files <- list.files(img.dir, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print the MOD09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' scale.factor <- 0.0001
#' 
#' # select the red, blue and nir bands
#' red <- raster(img.files[1]) * scale.factor
#' blue <- raster(img.files[3]) * scale.factor
#' nir <- raster(img.files[2]) * scale.factor
#' # calculate the EVI without scale
#' evi.1 <- varEVI(blue, red, nir)
#' # calculate the EVI scaling 0-1
#' evi.2 <- varEVI(blue, red, nir,scfun=getRGISToolsOpt("MOD09SCL"))
#' evi.12 <- stack(evi.1,evi.2)
#' # plot the image
#' spplot(evi.12,col.regions=rev(terrain.colors(20)),at = c(seq(0,1,0.05)))
varEVI<-function(blue,red,nir,scfun=function(r){r}){
  blue=scfun(blue)
  red=scfun(red)
  nir=scfun(nir)
  evi <- 2.5*((nir - red) / (nir+6 * red-7.5*blue+1))
  return(evi)
}

