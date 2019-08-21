#' Return the capturing date from the name of a raster layer
#'
#' \code{genGetDates} extracts the date of one or several images when the name
#' of the layer includes the date in the "\code{YYYYJJJ}" format.
#'
#' The function reads a date from a \code{character} class object in year-julian
#' ("\code{YYYYJJJ}") format and returns a \code{Date} class object.
#'
#' @param str \code{character} argument containing the date in julian format "\code{YYYYJJJ}".
#' @param ... arguments for nested functions:
#' \itemize{
#'   \item \code{format} the format of the date being returned.
#' }
#'
#' @return a \code{Date} class object with the date of the image.
#'
#' @examples
#' img <- matrix(1:16, ncol = 4, byrow = TRUE)
#' r <- raster(img)
#' names(r) <- c("RandomImage_2018034")
#'
#' spplot(r)
#' genGetDates(names(r), format = "%Y%j")
#'
genGetDates<-function(str, ...){
  arg<-list(...)
  if("date.format"%in%names(arg)){
    return(format(as.Date(gsub(".*\\s*(\\d{7}).*", "\\1", str),"%Y%j"),format=arg$date.format))
  }else{
    return(as.Date(gsub(".*\\s*(\\d{7}).*", "\\1", str),"%Y%j"))
  }
}


