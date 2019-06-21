#' Calculates normalized burn ratio (NBR)
#'
#' \code{varNBR} computes the NBR index from nir and swir2 bands.
#'
#' The Normalized Burn Ratio (NBR) is an index to identify burned areas by 
#' comparing its value before and after the fire event. Is calculated using nir and swir2 bands \insertCite{garcia1991mapping}{RGISTools}.
#' This function is used within
#' \code{\link{ls7FolderToVar}}, \code{\link{ls8FolderToVar}}, \code{\link{modFolderToVar}} and \code{\link{senFolderToVar}}.
#'
#' @references \insertRef{garcia1991mapping}{RGISTools}
#'
#' @param nir the nir band of the capture in \code{raster} format
#' @param swir2 the swir2 band of the capture in \code{raster} format
#'
#' @return NBR in \code{raster} format
#'
#' @examples
#' # dir path of cropped and cutted Modis image in the region of navarre as example
#' img.dir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all tif files
#' img.files <- list.files(img.dir, pattern="\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print Modis 09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' 
#' # select the nir and swir2 bands
#' nir <- raster(img.files[2])
#' swir2 <- raster(img.files[7])
#' # calculate the nbr image
#' nbr <- varNBR(nir, swir2)
#' # plot the image
#' spplot(nbr)
varNBR<-function(nir,swir2){
  nbr<-(nir-swir2)/(nir+swir2)
  return(nbr)
}


