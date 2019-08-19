#' Subset a RasterStack given a range of dates
#'
#' \code{genFilterStack} filters the raster layers within a range of dates.
#'
#' This is a helper function used by other functions in this package.
#'
#' @param r the \code{RasterStack} to be filtered.
#' @param ... arguments for nested functions:
#' \itemize{
#'   \item \code{startDate} a \code{Date} class object with the starting date of the study period.
#'   \item \code{endDate} a \code{Date} class object with the ending date of the study period.
#'   \item \code{AppRoot} the path where the RData will be saved.
#' }
#' @examples
#' # generate random images
#' img <- matrix(1:16, ncol = 4, byrow = TRUE)
#' r <- raster(img)
#' r <- stack(r, r, r, r, r, r)
#' names(r) <- paste0("RandomImage_201803", 1:6)
#' # print the names and dates of the random images
#' print(names(r))
#' genGetDates(names(r))
#' # example of filtering the raster stack
#' r2 <- genFilterStack(r = r,
#'                      startDate = as.Date("2018-02-02", "%Y-%m-%d"),
#'                      endDate = as.Date("2018-02-04", "%Y-%m-%d"))
#' # print the names and the number of layers of the filtered stack
#' genGetDates(names(r2))
#' nlayers(r2)
genFilterStack<-function(r,...){
  arg<-list(...)
  stopifnot("startDate"%in%names(arg)|
              "endDate"%in%names(arg))
  if("startDate"%in%names(arg)){
    dates<-genGetDates(names(r))
    r<-raster::subset(r,which(dates>=arg$startDate))
  }
  if("endDate"%in%names(arg)){
    dates<-genGetDates(names(r))
    r<-raster::subset(r,which(dates<=arg$endDate))
  }
  return(r)
}




