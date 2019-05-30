#' An example time-series of Digital Elevation Model (DEM) for the region of Navarre in Spain
#'
#' Geographically projected \code{RasterBrick} object with the DEM values for
#' the region of Navarre, to use as example in \code{genSmoothingIMA}
#'
#' @format The \code{RasterBrick} contains 6 time-periods, with the same DEM:
#' \describe{
#'   \item{name}{The of each capture contains the date of the period in julian format as YYYYJJJ}
#'   \item{size}{Each layer contains 113 rows and 105 columns}
#' }
#' @source \url{http://www.diamondse.info/}
"ex.dem.navarre"

#' An example time-series of NDVI for the region of Navarre in Spain
#'
#' Geographically projected \code{RasterBrick} object with the NDVI values for the region of Navarre
#'
#' @format The \code{RasterBrick} contains 6 time-periods:
#' \describe{
#'   \item{name}{The of each capture contains the date of the period in julian format as YYYYJJJ}
#'   \item{size}{Each layer contains 113 rows and 105 columns}
#' }
#' @source \url{http://www.diamondse.info/}
"ex.ndvi.navarre"

#' Geographically projected spatial polygon of the region of Navarre in Spain
#'
#' \code{SpatialPolygonsDataFrame} containing the region of Navarre projected in latitude/longitude
"navarre"
