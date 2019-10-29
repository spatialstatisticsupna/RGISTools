#' Calculates soil-adjusted vegetation index (SAVI)
#'
#' \code{varSAVI} Calculate the soil-adjusted vegetation index (SAVI) from the
#' red and near-infrared (NIR) bands.
#'
#' The soil adjusted vegetation index (SAVI) is an indicator engineered to remove 
#' the influence of the soil background effect \insertCite{huete1988soil}{RGISTools}.  
#' This function is used within \code{\link{ls7FolderToVar}}, 
#' \code{\link{ls8FolderToVar}}, \code{\link{modFolderToVar}} and
#' \code{\link{senFolderToVar}}.
#'
#' @references \insertRef{huete1988soil}{RGISTools}
#'
#' @param red a \code{raster} with the red band of the capture.
#' @param nir a \code{raster} with the NIR band of the capture.
#' @param L a constant to remove soil background effect. A value of 0.5 is
#' recommended in the literature.
#' @param scfun a function to re-scale the original pixel values into 
#' reflectance (0-1).
#'
#' @return A SAVI image in \code{raster} format.
#'
#' @examples
#' # path to the cropped and cutted MODIS images for the region of Navarre
#' wdir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all the tif files
#' files.mod <- list.files(wdir, pattern="\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print the MOD09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' 
#' # select the red and NIR bands
#' img.mod.red <- raster(files.mod[1])
#' img.mod.nir <- raster(files.mod[2])
#' # calculate the SAVI image
#' img.mod.savi <- varSAVI(img.mod.red,img.mod.nir,scfun=getRGISToolsOpt("MOD09SCL"))
#' # plot the image
#' spplot(img.mod.savi,col.regions=rev(topo.colors(20)))
varSAVI<-function(red,nir,L=0.5,scfun=function(r){r}){
  red=scfun(red)
  nir=scfun(nir)
  savi<-((nir-red)/(nir+red+L))*(1+L)
  return(savi)
}
