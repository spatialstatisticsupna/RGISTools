#' Calculates the normalized difference water index (NDWI)
#'
#' \code{varNDWI} Calculate the normalized difference water index (NDWI) from
#' the green and near-infrared (NIR) bands.
#'
#' The normalized difference water index (NDWI) is a ratio between the green
#' and near-infrared bands of the spectrum that was developed to detect open
#' water areas and minimize  the influence of the soil and vegetation variations 
#' \insertCite{mcfeeters1996use}{RGISTools}. This function is used within
#' \code{\link{ls7FolderToVar}}, \code{\link{ls8FolderToVar}}, 
#' \code{\link{modFolderToVar}} and \code{\link{senFolderToVar}}.
#'
#' @references \insertRef{mcfeeters1996use}{RGISTools}
#'
#' @param green a \code{raster} with the green band of the capture.
#' @param nir a \code{raster} with the NIR band of the capture.
#'
#' @return A NDWI image in \code{raster} format.
#'
#' @examples
#' # path to the cropped and cutted MODIS images for the region of Navarre
#' wdir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all the tif files
#' files.mod <- list.files(wdir, pattern="\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print the MOD09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' 
#' # select the green and NIR bands
#' img.mod.green <- raster(files.mod[4])
#' img.mod.nir <- raster(files.mod[2])
#' # calculate the NDWI image
#' img.mod.ndwi <- varNDWI(img.mod.green,img.mod.nir)
#' # plot the image
#' spplot(img.mod.ndwi,col.regions=rev(rainbow(20)))
varNDWI<-function(green,nir){
  ndwi<-(green-nir)/(green+nir)
  return(ndwi)
}
