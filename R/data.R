#' Time series of Digital Elevation Model (DEM) for the region of Navarre in Spain
#'
#' Geographically projected \code{RasterStack} with the DEM values for
#' the region of Navarre, to use as example in \code{genSmoothingIMA}.
#'
#' @format The \code{RasterStack} contains 6 time-periods, with the same DEM:
#' \describe{
#'   \item{name}{each layer is named with the date of the period in julian format as "\code{YYYYJJJ}"}.
#'   \item{size}{113 rows by 105 columns and 6 layers}.
#' }
"ex.dem.navarre"

#' NDVI time series of the tile containing Navarre in Spain
#'
#' Geographically projected \code{RasterBrick} object with the NDVI values for the region of Navarre.
#'
#' @format The \code{RasterBrick} contains 6 time-periods:
#' \describe{
#'   \item{name}{the name of each layer contains the date of the period in julian format as "\code{YYYYJJJ}"}.
#'   \item{size}{each layer contains 113 rows and 105 columns}.
#' }
"ex.ndvi.navarre"

#' Geographically projected spatial polygon of Navarre in Spain to use in examples
#'
#' \code{SpatialPolygonsDataFrame} containing the region of Navarre projected in longitude/latitude.
"ex.navarre"

#' @docType package
#' @bibliography system.file("REFERENCES.bib", package = "RGISTools")