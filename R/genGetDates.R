#' Return the date of the capure from the name of the image
#'
#' \code{genGetDates} extracts the date of one or several images when the name of the layer includes julian date in format "\code{YYYYJJJ}".
#'
#' The function reads a date from a \code{character} class variable in year-julian (\code{YYYYJJJ}) format and returns an R date class variable.
#'
#' @param str \code{character} argument containing the date in julian format "\code{YYYYJJJ}".
#' @param ... argument for function nestering.
#' \itemize{
#'   \item \code{date.format} modify the format of the date being returned.
#' }
#'
#' @examples
#' img <- matrix(1:16, ncol = 4, byrow = TRUE)
#' r <- raster(img)
#' names(r) <- c("RandomImage_2018034")
#'
#' spplot(r)
#' genGetDates(names(r))
#'
genGetDates<-function(str, ...){
  arg<-list(...)
  if("date.format"%in%names(arg)){
    return(format(as.Date(gsub(".*\\s*(\\d{7}).*", "\\1", str),"%Y%j"),format=arg$date.format))
  }else{
    return(as.Date(gsub(".*\\s*(\\d{7}).*", "\\1", str),"%Y%j"))
  }
}


