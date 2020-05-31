#' Fill data gaps and smooth outliers in a time series of satellite images using
#' covariates
#'
#' \code{genSmoothingCovIMA} runs the image mean anomaly (IMA) algorithm
#' with covariates \insertCite{militino2018improving}{RGISTools}.
#'
#' This filling/smoothing method was developed by 
#' \insertCite{militino2018improving;textual}{RGISTools}. This technique 
#' decomposes a time series of images into a new series of mean and anomaly 
#' images. The procedure applies the filling/smoothing algorithm with covariates
#' over the anomaly images. The procedure requires a proper definition of a
#' temporal neighbourhood for the target image and aggregation factor.
#'
#' @references \insertRef{militino2018improving}{RGISTools}
#'
#' @param rStack a \code{RasterStack} class argument containing a time series of
#' satellite images. Layer names should contain the date of the image in
#'  "\code{YYYYJJJ}" format.
#' @param cStack a \code{RasterStack} class argument containing a time series of
#' covariates.
#' @param r.dates a \code{vector} argument containing the dates of the layers in rstack 
#' @param Img2Process a \code{vector} class argument defining the images to be
#' filled/smoothed.
#' @param nDays a \code{numeric} argument with the number of previous and 
#' subsequent days that define the temporal neighborhood.
#' @param nYears a \code{numeric} argument with the number of previous and
#' subsequent years that define the temporal neighborhood.
#' @param aFilter a \code{vector} with the lower and upper quantiles that define
#' the outliers of the anomalies. Ex. c(0.05,0.95).
#' @param fact a \code{numeric} argument with an aggregation factor of the
#' anomalies carried out before the interpolation.
#' @param fun a \code{function} used to aggregate the image of anomalies. Both
#' \code{mean}(default) or \code{median} are acceptted.
#' @param snow.mode logical argument. If \code{TRUE}, the filling process will
#' be parallelized using the `\code{raster}' package.
#' @param out.name the name of the folder containing the filled/smoothed images
#' when saved in the Hard Disk Drive (HDD).
#' @param ... arguments for nested functions:
#' \itemize{
#'   \item \code{AppRoot} the path where the filled/smoothed time series of 
#'   images are saved as GTiff.
#' }
#' 
#' @return a \code{RasterStack} with the filled/smoothed images. 
#'
#' @examples
#' \dontrun{
#' set.seed(0)
#' # load example ndvi and dem data of Navarre
#' data(ex.ndvi.navarre)
#' data(ex.dem.navarre)
#' # plot example data
#' genPlotGIS(ex.ndvi.navarre)
#' genPlotGIS(ex.dem.navarre)
#'
#' # distorts 5% of the original ndvi data by 
#' # altering 50% its values
#' for(x in c(2,5)){
#'   aux <- sampleRandom(ex.ndvi.navarre[[x]],
#'                       ncell(ex.ndvi.navarre) * 0.05,
#'                       cells = TRUE,
#'                       na.rm = TRUE)
#'   ex.ndvi.navarre[[x]][aux[,1]] <- aux[,2] * 1.5
#' }
#' genPlotGIS(ex.ndvi.navarre)
#'
#' # smoothing the image using the DEM as covariate
#' smth.ndvi <- genSmoothingCovIMA(rStack = ex.ndvi.navarre,
#'                                 cStack = ex.dem.navarre,
#'                                 Img2Process = c(2))
#' # plot the distorted 1, smoothed 1, 
#' # distorted 5, smoothed 5 images
#' plot(stack(ex.ndvi.navarre[[2]],
#'                  smth.ndvi[[1]],
#'                  ex.ndvi.navarre[[5]],
#'                  smth.ndvi[[2]]))
#' }
genSmoothingCovIMA <- function (rStack,
                             cStack,
                             Img2Process=NULL,
                             nDays=3,
                             nYears=1,
                             r.dates,
                             fact=5,
                             fun=mean,
                             aFilter=c(.05,.95),
                             snow.mode=FALSE,
                             out.name="out",
                             ...)
{
  arg<-list(...)
  # rStack<-target.2011.2013
  # cStack<-target.covs.2011.2013
  #chequea que los datos de entrada tengan el formato correcto
  stopifnot(class(rStack)%in%c("RasterStack","RasterBrick"))
  stopifnot(class(cStack)%in%c("RasterStack","RasterBrick"))
  stime<-Sys.time()
  if(snow.mode){
    beginCluster()
  }

  # days in rStack
  if(!missing(r.dates)){
    if(length(r.dates)!=nlayers(rStack))stop("dates and rStack must have the same length.")
    days<-r.dates
  }else{
    days<-genGetDates(names(rStack))
  }
  
  oday<-order(days)
  
  if(all(is.na(days))){stop("The name of the layers has to include the date and it must be in julian days (%Y%j) .")}
  # ensure the images order
  rStack<-raster::subset(rStack,oday)
  days<-days[oday]
  
  # analyse covariates dates
  daysc<-genGetDates(names(cStack))
  ocday<-order(daysc)
  cStack<-raster::subset(cStack,ocday)

  years<-unique(format(days[oday],"%Y"))

  nyears<-length(years)
  nPed<-length(days)

  #stop if fractional is not 0
  stopifnot(nPed%%1==0)

  #the covariates needs the following name convention variable_%Y%j
  #get cov names
  covnames<-unique(gsub(".*\\s*(\\d{7}).*", "", unlist(strsplit(names(cStack),"_"))))
  covnames<-covnames[!covnames%in%""]
  #check all the dates in covariates matches with target dates
  for(cov in covnames){
    cdates<-names(cStack)[grepl(cov,names(cStack))]
    if(!all(genGetDates(cdates)%in%days)){
      stop("Covariates dates not matching with target dates")
    }
  }

  # select images to predict
  if(is.null(Img2Process)){
    Img2Process<-1:nlayers(rStack)
  }else{
    aux<-Img2Process[Img2Process%in%1:nlayers(rStack)]
    if(is.null(aux)){stop("Target images in Img2Fill do not exist.")}
    if(length(aux)!=length(Img2Process)){warning("Some of target images in Img2Fill do not exist in rStack.")}
    Img2Process<-aux
  }

  if("AppRoot"%in%names(args)){
    args$AppRoot<-pathWinLx(args$AppRoot)
    dir.create(args$AppRoot,recursive=TRUE,showWarnings = FALSE)
  }
    



  fillstack<-raster::stack()
  for(p in Img2Process){
    #Identificamos las imágenes a predecir
    target.date<-days[p]
    message(paste0("Smoothing image of date ",target.date))
    neighbours<-dateNeighbours(rStack,
                               target.date,
                               r.dates=days,
                               nPeriods=nDays,
                               nYears=nYears)



    # calculate mean image
    meanImage<-raster::calc(neighbours,fun=fun,na.rm=TRUE)

    # get target image
    targetImage<-raster::subset(rStack,which(format(days,"%Y%j")%in%format(target.date,"%Y%j")))

    # get covs for target image
    cov.targetImage<-raster::subset(cStack,which(format(genGetDates(names(cStack)),"%Y%j")%in%format(target.date,"%Y%j")))

    # calculate anomaly for target
    anomaly<-targetImage-meanImage

    # calculate anomaly for covs
    cname<-names(cov.targetImage)
    cov.targetImage<-cov.targetImage-meanImage
    names(cov.targetImage)<-cname

    # remove extreme values
    qrm<-raster::quantile(anomaly,aFilter,na.omit=TRUE)
    anomaly[anomaly<qrm[1]|anomaly>qrm[2]]<-NA

    # reduce the resolution for tps
    aggAnomaly<-raster::aggregate(anomaly, fact=fact,fun=fun)
    aggCovs<-raster::aggregate(cov.targetImage, fact=fact,fun=fun)

    xy <- data.frame(xyFromCell(aggCovs, 1:ncell(aggCovs)))
    v <- getValues(aggAnomaly)

    values<-getValues(aggCovs)

    #remove locations where covariables has NA values
    if(length(covnames)==1){
      xy<-xy[!is.na(values),]
      v<-v[!is.na(values)]
      values[!is.na(values)]
    }else{
      not.na <- !apply(values, 1, function(x){any(is.na(x))})
      xy<-xy[not.na,]
      v<-v[not.na,]
      values<-values[not.na,]
    }


    # Tps model
    tps<-suppressWarnings(Tps(xy,v,Z=values))

    # smooth anomaly
    if(snow.mode){
      target.prediction <- clusterR(cov.targetImage, raster::interpolate, args=list(model=tps,xyOnly=FALSE,fun=interpfun))
    }else{
      target.prediction <- raster::interpolate(cov.targetImage, tps,xyOnly=FALSE,fun=interpfun)
    }

    target.prediction<-target.prediction+meanImage
    # write filled images
    if("AppRoot"%in%names(args)){
      writeRaster(target.prediction,paste0(args$AppRoot,"/",out.name,"_",format(target.date,"%Y%j"),".tif"))
    }else{
      fillstack<-addLayer(fillstack,target.prediction)
    }
  }
  if(snow.mode){
    endCluster()
  }
  names(fillstack)<-names(rStack)[Img2Process]
  etime<-Sys.time()
  message(paste0(length(Img2Process)," images processed in ",MinSeg(etime,stime)))
  return(fillstack)
}

interpfun <- function(model, x, ...) {
  predict(model, x[,1:2], Z=x[,3:ncol(x)], ...)
}

