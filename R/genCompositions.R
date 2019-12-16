#' Create image compositions from a time series of satellite images
#'
#' \code{genCompositions} combines a series of satellite images to create compositions.
#'
#' The layer of the composite image takes its name from the first image used in
#' the composition.
#'
#' \code{genCompositions} reduces the number of images but improves the total
#' quality of the time-series by removing cloulds and outliers. One widespread
#' compositing technique is the maximum value composition (MVC). This technique
#' allocates in each pixel of the composite the maximum value (fun = max) that
#' the pixel reaches during a time period (\code{n}, \code{by.days = TRUE}).
#'
#' @param rstack a \code{RasterStack}, where layer names contain the capturing
#' date of an image in "\code{YYYYJJJ}" format.
#' @param n number of images combined in the aggregation. Only required if 
#' \code{by} is not provided.
#' @param fun the function used to create the composite, such as \code{max},
#' \code{min}, \code{mean}, ...
#' @param by character argument. Accepts \code{"month"} or \code{"year"} for creating monthly
#' or yearly composites. Only required if \code{n} is provided.
#' @param by.days logical argument. If \code{FALSE}, \code{n} indicates the 
#' number of consucutive images being aggregated. If \code{TRUE}, the function
#' aggregates the imagery within every \code{n} days. The aggregation requires
#' at least one image avaiable.
#' @param ... arguments for nested functions:
#' \itemize{
#'   \item \code{AppRoot} the path where the images will be saved in the
#'   GTiff format.
#' }
#' 
#' @return a \code{RasterStack} with the time series of the composite images.
#'
#' @examples
#' # loading NDVI images of Navarre
#' data("ex.ndvi.navarre")
#' # Ploting the images: clouds are found
#' genPlotGIS(ex.ndvi.navarre)
#' # the first composite image is made with images 1, 2 and 3, 
#' # and the second composite image is made with images 4, 5 and 6
#' composite.NDVI.a <- genCompositions(rstack = ex.ndvi.navarre,
#'                                     n = 3,
#'                                     fun = max)
#' genPlotGIS(composite.NDVI.a)
#' # when by.days=TRUE, the first composite image is made with images 1, 2 and 3, 
#' # the second with image 4, and the third with images 5 and 6.
#' composite.NDVI.3a <- genCompositions(rstack = ex.ndvi.navarre,
#'                                      n = 3,
#'                                      by.days = TRUE,
#'                                      fun = max)
#' # Check that the clouds were removed
#' genPlotGIS(composite.NDVI.3a)
genCompositions<-function(rstack,by,fun,n,by.days=FALSE,verbose=FALSE,...){
  args<-list(...)
  dates<-genGetDates(names(rstack))
  if(!missing(by)){
    
    if(verbose){message("Omitting n and by.day argument...")}
    switch (by,
            "month" = {
              alldates<-format(dates,"%Y%m")
              alldates<-as.factor(alldates)
              idx<-as.integer(alldates)
              comp<-stackApply(rstack,idx,fun=fun, ...)
              alldates<-as.Date(paste0(unique(as.character(alldates)),"01"),"%Y%m%d")
              names(comp)<-paste0("Comp_",format(alldates,"%Y%j"))
            },
            "year" = {
              alldates<-format(dates,"%Y")
              alldates<-as.factor(alldates)
              idx<-as.integer(alldates)
              comp<-stackApply(rstack,idx,fun=fun, ...)
              alldates<-as.Date(paste0(unique(as.character(alldates)),"0101"),"%Y%m%d")
              names(comp)<-paste0("Comp_",format(alldates,"%Y%j"))
            },
            stop("by argument only accepts 'month' or 'years'.")
    )
  }else{
    if(by.days){
      # Create compositions using dates
      dates<-genGetDates(names(rstack))
      mxdate<-max(dates)
      mndate<-min(dates)
      alldates<-as.Date(mndate:mxdate)
      n.ped<-length(alldates)
      rnp<-ceiling(n.ped/n)
      idx<-rep(1:rnp,each=n)[1:length(alldates)]
      idx<-idx[alldates%in%dates]
      comp<-stackApply(rstack,idx,fun=fun, ...)
      names(comp)<-paste0("Comp_",n,"_",format(alldates,"%Y%j")[seq(1,n.ped,n)[unique(idx)]])
    }else {
      alldates<-format(dates,"%Y%m")
      alldates<-as.factor(alldates)
      idx<-as.integer(alldates)
      comp<-stackApply(rstack,idx,fun=fun, ...)
      alldates<-as.Date(paste0(unique(as.character(alldates)),"01"),"%Y%m%d")
      names(comp)<-paste0("Comp_",format(alldates,"%Y%j"))
    }
  }
  if("AppRoot"%in%names(args)){
    writeRaster(comp,filename=paste0(pathWinLx(args$AppRoot),"/",names(comp),".tif"),bylayer=TRUE)
  }else{
    return(comp)
  }
}
