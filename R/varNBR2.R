#' Calculates normalized burn ratio 2 (NBR2)
#'
#' \code{varNBR2} computes the NBR2 index from swir1 and swir2 bands.
#'
#' The Normalized Burn Ratio 2 (NRB) is an index to identify burned areas. 
#' In contrast to NBR highlight the sensitivity to water in vegetation.
#' This function is used within
#' \code{\link{ls7FolderToVar}}, \code{\link{ls8FolderToVar}}, \code{\link{modFolderToVar}} and \code{\link{senFolderToVar}}.
#'
#' @param swir1 the nir band of the capture in \code{raster} format
#' @param swir2 the swir2 band of the capture in \code{raster} format
#'
#' @return NBR2 in \code{raster} format
#'
#' @examples
#' # dir path of cropped and cutted modis image in the region of navarre as example
#' img.dir <- system.file("ExNavarra", package = "RGISTools")
#' # list all tif files
#' img.files <- list.files(img.dir,pattern="\\.tif$",recursive = TRUE,full.names = TRUE)
#' #select the swir1 and swir2 bands
#' swir1 <- raster(img.files[6])
#' swir2 <- raster(img.files[7])
#' # calculate the nbr2 image
#' nbr2 <- varNBR2(swir1,swir2)
#' # plot the image
#' spplot(nbr2)
varNBR2<-function(swir1,swir2){
  nbr2<-(swir1-swir2)/(swir1+swir2)
  return(nbr2)
}

