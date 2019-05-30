#' Creates image compositions from a time series of satellite images
#'
#' \code{genCompositions} combines satellite images over a temporal window to create composites
#'
#' This function supports temporal aggregations with different function specifying \code{fun} argument.
#' THe number of images aggregated is defined using \code{n.days} argument.
#'
#' Composites reduce the number of images, but improve the quality of the images,
#' since clouds and potential outliers are removed. One of the most used
#' techniques is the Maximum Value Composition (\code{MVC}). The \code{MVC} applied over
#' images of remote sensing indices. Clouds and outliers usually appear as
#' very low values the index. Hence, the \code{MVC} builds a composite using the
#' maximum value a pixel over a period of time. Write \code{fun = max} to use the
#' \code{MVC} technique. Other functions are also supported
#'
#' @param rstack a \code{RasterStack} where every layer is named after the date it was captured in \%Y\%j format
#' @param n.days number of days considered in the temporal window
#' @param fun the function used to create the composite, such as \code{max}, \code{min}, \code{mean}, ...
#' @param by.periods flag to specify the number of images to aggregate instead a temporal window
#' @param ... accepts \code{AppRoot} the path where the RData will be saved and argument for nested functions
#'
#' @examples
#' # load a time series of NDVI images over Navarre
#' data("ex.ndvi.navarre")
#' # show the images: clouds are found
#' genPlotGIS(ex.ndvi.navarre)
#' # Composites dividing the series in two periods and using MVC
#' composite.NDVI.1 <- genCompositions(ex.ndvi.navarre,
#'                                     n.days = 2,
#'                                     by.periods = TRUE,
#'                                     fun = max)
#' # Check that the clouds were removed
#' genPlotGIS(composite.NDVI.1)
#' # Equivalent: Use 3 images for every composite
#' composite.NDVI.2 <- genCompositions(ex.ndvi.navarre,
#'                                     n.days = 3,
#'                                     fun = max)
#' genPlotGIS(composite.NDVI.2)
genCompositions<-function(rstack,n.days,fun,by.periods=F,...){
  args<-list(...)
  AppRoot<-defineAppRoot(...)
  #rstack<-stack(list.files("Z:/ImagenesSatelite/MODIS_v6/NDVI",full.names=T,pattern="\\.tif"))
  if(!by.periods){
    # Create compositions using dates
    dates<-genGetDates(names(rstack))
    mxdate<-max(dates)
    mndate<-min(dates)
    alldates<-as.Date(mndate:mxdate)
    n.ped<-length(alldates)
    rnp<-ceiling(n.ped/n.days)
    idx<-rep(1:rnp,each=n.days)[1:length(alldates)]
    idx<-idx[alldates%in%dates]
    comp<-stackApply(rstack,idx,fun=fun)
    names(comp)<-paste0("Comp_",n.days,"_",format(alldates,"%Y%j")[unique(idx)])
  }else{
    # Create compositions using periods
    n.ped<-nlayers(rstack)
    rnp<-ceiling(n.ped/n.days)
    idx<-rep(1:rnp,each=n.days)[1:n.ped]
    comp<-stackApply(rstack,idx,fun=fun)
    names(comp)<-paste0(names(rstack)[unique(idx)],"_Comp_",n.days)
  }
  if("AppRoot"%in%names(args)){
    writeRaster(comp,filename=paste0(AppRoot,"/",names(comp),".tif"),bylayer=T)
  }else{
    return(comp)
  }
}
