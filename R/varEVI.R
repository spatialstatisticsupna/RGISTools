#' Calculate the enhanced vegetation index (EVI) from raster bands
#'
#' \code{varEVI} computes the EVI index from blue, nir and red bands.
#'
#' The Enhanced Vegetation Index (EVI) is a vegetation indicator that improves sensitivity 
#' towards high biomass densities compared to NDVI \insertCite{huete2002overview}{RGISTools}.
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
#' # dir path of cropped and cutted Modis image in the region of Navarre as example
#' img.dir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all tif files
#' img.files <- list.files(img.dir, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print Modis 09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' scale.factor <- 0.0001
#' 
#' # select the red, blue and nir bands
#' red <- raster(img.files[1]) * scale.factor
#' blue <- raster(img.files[3]) * scale.factor
#' nir <- raster(img.files[2]) * scale.factor
#' # calculate the evi without scale
#' evi.1 <- varEVI(blue, red, nir)
#' # calculate the evi scaling 0-1
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

