#' Time series of Digital Elevation Model (DEM) for the region of Navarre in Spain
#'
#' Geographically projected \code{RasterStack} with the DEM values for
#' the region of Navarre, for using as example in \code{genSmoothingCovIMA}. \code{RasterStack} 
#' projection: \code{+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs}.
#'
#' @format The \code{RasterStack} contains 6 time-periods, with the same DEM:
#' \describe{
#'   \item{name}{each layer is named with the date of the period in julian format as "\code{YYYYJJJ}"}.
#'   \item{size}{113 rows by 105 columns and 6 layers}.
#' }
#' @name ex.dem.navarre
#' @docType data
#' @keywords data
NULL

#' NDVI of the tile containing Navarre in Spain
#'
#' Geographically projected \code{RasterBrick} object of the Normalized Difference Vegetation Index (NDVI) in Navarre.
#'
#' @format The \code{RasterBrick} contains 6 time-periods:
#' \describe{
#'   \item{name}{the name of each layer contains the date of the period in julian format as "\code{YYYYJJJ}"}.
#'   \item{size}{each layer contains 113 rows and 105 columns}.
#' }
#' @name ex.ndvi.navarre
#' @docType data
#' @keywords data
NULL

#' Geographically projected spatial polygon of Navarre in Spain to be used in examples
#'
#' \code{SpatialPolygonsDataFrame} containing the region of Navarre projected in longitude/latitude.
#' @name ex.navarre
#' @docType data
#' @keywords data
NULL


#' @docType package
#' @bibliography system.file("REFERENCES.bib", package = "RGISTools")