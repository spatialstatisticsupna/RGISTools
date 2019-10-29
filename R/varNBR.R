#' Calculate normalized burn ratio (NBR)
#'
#' \code{varNBR} computes the normalized burn ratio (NBR) from the
#' near-infrared (NIR) and shortwave-infrared 2 (SWIR2) bands.
#'
#' The normalized burn ratio (NBR) is an index to identify burned areas by 
#' comparing its value before and after the fire event. Is calculated using the
#' NIR and SWIR2 bands \insertCite{garcia1991mapping}{RGISTools}.This function
#' is used within \code{\link{ls7FolderToVar}}, \code{\link{ls8FolderToVar}}, 
#' \code{\link{modFolderToVar}} and \code{\link{senFolderToVar}}.
#'
#' @references \insertRef{garcia1991mapping}{RGISTools}
#'
#' @param nir a \code{raster} with the NIR band of the capture.
#' @param swir2 a \code{raster} with the SWIR2 band of the capture.
#'
#' @return A NBR image in \code{raster} format.
#'
#' @examples
#' # path to the cropped and cutted MODIS images for the region of Navarre
#' wdir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all the tif files
#' files.mod <- list.files(wdir, pattern="\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print the MOD09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' 
#' # select the NIR and SWIR2 bands
#' files.mod.nir <- raster(files.mod[2])
#' files.mod.swir2 <- raster(files.mod[7])
#' # calculate the NBR image
#' files.mod.nbr <- varNBR(files.mod.nir, files.mod.swir2)
#' # plot the image
#' spplot(files.mod.nbr,col.regions=rev(heat.colors(20)))
varNBR<-function(nir,swir2){
  nbr<-(nir-swir2)/(nir+swir2)
  return(nbr)
}


