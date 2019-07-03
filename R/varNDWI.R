#' Calculates normalized difference water index (NDWI)
#'
#' \code{varNDWI} Calculate the NDWI index from green and nir bands.
#'
#' The Normalized Difference Water Index (NDWI) is a ratio between bands of the spectrum 
#' that was developed to detect open water areas and minimize the influence of the soil 
#' and vegetation variations \insertCite{mcfeeters1996use}{RGISTools}.
#' This function is used within
#' \code{\link{ls7FolderToVar}}, \code{\link{ls8FolderToVar}}, \code{\link{modFolderToVar}} and \code{\link{senFolderToVar}}.
#'
#' @references \insertRef{mcfeeters1996use}{RGISTools}
#'
#' @param green the green band of the capture in \code{raster} format.
#' @param nir the swir1 band of the capture in \code{raster} format.
#'
#' @return NDWI in \code{raster} format.
#'
#' @examples
#' # dir path of cropped and cutted Modis image in the region of Navarre as example
#' img.dir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all tif files
#' img.files <- list.files(img.dir, pattern="\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print Modis 09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' 
#' # select the green and nir bands
#' green <- raster(img.files[4])
#' nir <- raster(img.files[2])
#' # calculate the ndwi image
#' ndwi <- varNDWI(green,nir)
#' # plot the image
#' spplot(ndwi,col.regions=rev(rainbow(20)))
varNDWI<-function(green,nir){
  ndwi<-(green-nir)/(green+nir)
  return(ndwi)
}
