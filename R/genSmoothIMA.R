#' Uses covariates for smoothing outliers in a time series of satellite images
#'
#' \code{genSmoothingIMA} is the implementation of a spatio-temporal smoothing
#' method that uses covariates. The methodology is explained in \insertCite{militino2019interpolation}{RGISTools}.
#'
#' The procedure uses spatio temporal data to decompose each image in the time series as a mean
#' image plus a residual or anomaly.The procedure smoothed the anomaly using covariates to
#' smooth the outliers in the anomaly. See more information in \insertCite{militino2019interpolation}{RGISTools}.
#'
#' This function provides a set of arguments to configure the run of the precedure, such as the images to fill (\code{Img2Process}),
#' the number of periods or years in the definition of the spatio temporal neighbourhoood, or the size of the
#' aggregation. All these particularities are explained in the publication.
#'
#' @references \insertRef{militino2018improving}{RGISTools}
#'
#' @param rStack a \code{RasterStack} containing a time series of satellite images.
#' @param cStack a \code{RasterStack} containing a time series of covariates.
#' @param Img2Process a vector defining the images to smooth.
#' @param nPeriods number of previous and subsequent days used in the neighborhood used to define the neighborhood
#' @param nYears number of years used to define the neighborhood.
#' @param aFilter a vector with the lower and upper extreme values for filtering the anomalies. Ex. c(0.05,0.95).
#' @param fun a function used to calculate the aggregated image. Both the \code{mean} or \code{median} functions are acceptable.
#' @param snow.mode logical argument. If \code{TRUE} the filling process will be parallelized by \code{raster} package.
#' @param fact an aggregation factor to be used to reduce the anomalies before smoothing.
#' @param out.name the name of the images if the result is written in the HDD.
#' @param ... argument to allow function nestering.
#' \itemize{
#'   \item \code{AppRoot} the path where the images will be saved in tif format.
#' }
#'
#' @examples
#' set.seed(0)
#' # load example ndvi and dem data of Navarre
#' data(ex.ndvi.navarre)
#' data(ex.dem.navarre)
#' # plot example data
#' genPlotGIS(ex.ndvi.navarre)
#' genPlotGIS(ex.dem.navarre)
#'
#' # Distorts 5% of the original ndvi data by 
#' # altering 50% its values
#' for(x in c(2,5)){
#'   aux <- sampleRandom(ex.ndvi.navarre[[x]],
#'                       ncell(ex.ndvi.navarre)*0.05,
#'                       cells=TRUE,
#'                       na.rm = TRUE)
#'   ex.ndvi.navarre[[x]][aux[,1]]<-aux[,2]*1.5
#' }
#' genPlotGIS(ex.ndvi.navarre)
#'
#' # smoothing the image using dem as covariate
#' smth.ndvi<-genSmoothingIMA(rStack=ex.ndvi.navarre,
#'                            cStack=ex.dem.navarre,
#'                            Img2Process=c(2,5))
#' # plot the distorted 1, smoothed 1, 
#' # distorted 5, smoothed 5 images
#' plot(stack(ex.ndvi.navarre[[2]],
#'                  smth.ndvi[[1]],
#'                  ex.ndvi.navarre[[5]],
#'                  smth.ndvi[[2]]))
genSmoothingIMA <- function (rStack,
                             cStack,
                             Img2Process=NULL,
                             fun=mean,
                             nPeriods=3,
                             nYears=1,
                             fact=5,
                             out.name="out",
                             aFilter=c(),
                             snow.mode=FALSE,
                             ...)
{
  arg<-list(...)
  AppRoot<-defineAppRoot()
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
  days<-genGetDates(names(rStack))
  oday<-order(days)

  # ensure the images order
  rStack<-raster::subset(rStack,oday)

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

  if(!file.exists(AppRoot))
    dir.create(AppRoot,recursive=T)



  fillstack<-raster::stack()
  for(p in Img2Process){
    #Identificamos las imágenes a predecir
    target.date<-days[p]
    message(paste0("Smoothing image of date ",target.date))
    neighbours<-dateNeighbours(rStack,
                               target.date,
                               nPeriods=nPeriods,
                               nYears=nYears)



    # calculate mean image
    meanImage<-raster::calc(neighbours,fun=fun,na.rm=T)

    # get target image
    targetImage<-raster::subset(rStack,which(format(genGetDates(names(rStack)),"%Y%j")%in%format(target.date,"%Y%j")))

    # get covs for target image
    cov.targetImage<-raster::subset(cStack,which(format(genGetDates(names(cStack)),"%Y%j")%in%format(target.date,"%Y%j")))

    # calculate anomaly for target
    anomaly<-targetImage-meanImage

    # calculate anomaly for covs
    cname<-names(cov.targetImage)
    cov.targetImage<-cov.targetImage-meanImage
    names(cov.targetImage)<-cname

    # remove extreme values
    qrm<-raster::quantile(anomaly,aFilter,na.omit=T)
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
      dir.create(args$writeRaster,showWarnings = F,recursive = T)
      writeRaster(target.prediction,paste0(AppRoot,"/",out.name,"_",format(target.date,"%Y%j"),".tif"))
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

