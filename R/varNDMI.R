#' Calculate normalized difference moisture (water) index (NDMI)
#'
#' \code{varNDMI} computes the normalized difference moisture index (NDMI) from
#' the near-infrared (NIR) and shortwave-infrared 1 (SWIR1) bands.
#'
#' The normalized difference moisture index (NDMI) is an index that represents
#' the water stress levels of the canopy, using the NIR and SWIR
#' \insertCite{gao1995normalized}{RGISTools}.
#' This function is used within \code{\link{ls7FolderToVar}}, 
#' \code{\link{ls8FolderToVar}}, \code{\link{modFolderToVar}} and
#' \code{\link{senFolderToVar}}.
#'
#' @references \insertRef{gao1995normalized}{RGISTools}
#'
#' @param nir a \code{raster} with the nir band of the capture.
#' @param swir1 a \code{raster} with the swir1 band of the capture.
#'
#' @return A NDMI image in \code{raster} format.
#'
#' @examples
#' # path to the cropped and cutted MODIS images for the region of Navarre
#' img.dir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all the tif files
#' img.files <- list.files(img.dir, pattern="\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print the MOD09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' 
#' # select the NIR and SWIR1 bands
#' nir <- raster(img.files[2])
#' swir1 <- raster(img.files[6])
#' # calculate the NDMI image
#' ndmi <- varNDMI(nir,swir1)
#' # plot the image
#' spplot(ndmi)
varNDMI<-function(nir,swir1){
  ndmi<-(nir-swir1)/(nir+swir1)
  return(ndmi)
}
