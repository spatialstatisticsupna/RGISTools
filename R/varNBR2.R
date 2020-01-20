#' Calculate the normalized burn ratio 2 (NBR2)
#'
#' \code{varNBR2} computes the NBR2 index from the shortwave infrared 1 (SWIR1)
#' and shortwave infrared 2 (SWIR2).
#'
#' The normalized burn ratio 2 (NRB2) is an index to identify burned areas. 
#' In contrast to NBR, NRB2 highlights the sensitivity to water in vegetation
#' \insertCite{lutes2006firemon}{RGISTools}. This function is used within
#' \code{\link{ls7FolderToVar}}, \code{\link{ls8FolderToVar}},
#' \code{\link{modFolderToVar}} and \code{\link{senFolderToVar}}.
#'
#' @references \insertRef{lutes2006firemon}{RGISTools}
#'
#' @param swir1 a \code{raster} with the the SWIR1 band of the capture.
#' @param swir2 a \code{raster} with the the SWIR2 band of the capture.
#'
#' @return A NBR2 image in \code{raster} format.
#'
#' @examples
#' # path to the cropped and cutted MODIS images for the region of Navarre
#' wdir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all the tif files
#' files.mod <- list.files(wdir, pattern="\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print the MOD09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' 
#' # select the SWIR1 and SWIR2 bands
#' img.mod.swir1 <- raster(files.mod[6])
#' img.mod.swir2 <- raster(files.mod[7])
#' # calculate the NBR2 image
#' img.mod.nbr2 <- varNBR2(img.mod.swir1,img.mod.swir2)
#' # plot the image
#' spplot(img.mod.nbr2,col.regions=rev(heat.colors(20)))
varNBR2<-function(swir1,swir2){
  nbr2<-(swir1-swir2)/(swir1+swir2)
  return(nbr2)
}

