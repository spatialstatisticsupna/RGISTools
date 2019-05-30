#' Returns the capure date from name
#'
#' \code{genGetDates} extracts the date of one or several images when the date is included in "\%Y\%j" date format.
#'
#' The function reads a date from a character type variable in year-julian format and returns an R date format variable
#'
#' @param str String containing the date in "\%Y\%j" format
#' @param ... argument to allow function nestering
#' \itemize{
#'   \item \code{date.format} modify the format of the date being returned
#' }
#'
#' @examples
#' library(raster)
#'
#' img <- matrix(1:16, ncol = 4, byrow = TRUE)
#' r<-raster(img)
#' names(r)<-c("RandomImage_2018034")
#'
#' spplot(r)
#' genGetDates(names(r))
#'
genGetDates<-function(str,...){
  arg<-list(...)
  if("date.format"%in%names(arg)){
    return(format(as.Date(gsub(".*\\s*(\\d{7}).*", "\\1", str),"%Y%j"),format=arg$date.format))
  }else{
    return(as.Date(gsub(".*\\s*(\\d{7}).*", "\\1", str),"%Y%j"))
  }
}

as.Date<-function(x,...){
  if(is.numeric(x)){
    return(base::as.Date("1970-01-01")+x)
  }else{
    return(base::as.Date(x,...))
  }
}
