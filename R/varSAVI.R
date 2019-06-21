#' Calculates soil-adjusted vegetation index (SAVI) from raster bands
#'
#' \code{varSAVI} computes the the SAVI index from red and nir bands.
#'
#' The Soil Adjusted Vegetation Index (SAVI) is a vegetation index engineered to remove 
#' the influence of the soil background effect \insertCite{huete1988soil}{RGISTools}.  
#' This function is used within
#' \code{\link{ls7FolderToVar}}, \code{\link{ls8FolderToVar}}, \code{\link{modFolderToVar}} and \code{\link{senFolderToVar}}.
#'
#' @references \insertRef{huete1988soil}{RGISTools}
#'
#' @param red the red band of the capture in \code{raster} format.
#' @param nir the nir band of the capture in \code{raster} format.
#' @param L value of 0.5 in reflectance space was found to minimize soil brightness variations.
#'
#' @return SAVI in \code{raster} format
#'
#' @examples
#' # dir path of cropped and cutted Modis image in the region of navarre as example
#' img.dir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all tif files
#' img.files <- list.files(img.dir, pattern="\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print Modis 09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' 
#' # select the red and nir bands
#' red <- raster(img.files[1])
#' nir <- raster(img.files[2])
#' # calculate the ndwi image
#' savi <- varSAVI(red,nir)
#' # plot the image
#' spplot(savi,col.regions=rev(topo.colors(20)))
varSAVI<-function(red,nir,L=0.5){
  savi<-((nir-red)/(nir+red+L))*(1+L)
  return(savi)
}
