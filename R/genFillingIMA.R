#' Fills the gaps in a time series of satellite images
#'
#' \code{genFillingIMA} is the implementation of a spatio-temporal method called Image Mean Anomaly
#' for gap filling published in  (\url{http://dx.doi.org/10.1109/TGRS.2019.2904193}).
#'
#' The time series is decomposed into a mean and a residual (anomaly)
#' components. Instead of smoothing images, the procedure applies
#' the smoothing algorithm over the anomaly.
#'
#' The arguments configure the smoothing procedure:
#' \code{Img2Fill} identifies the images to filled, \code{nPeriods} defines
#' the spatio temporal neighbourhoood considered when smoothing
#' a pixel, and \code{aggfact} sets the level of spatial aggregation.
#' See more information in \insertCite{militino2019interpolation}{RGISTools}.
#'
#' @references \insertRef{militino2019interpolation}{RGISTools}
#'
#' @param imgTS the time series of images to be filled.
#' @param Img2Fill a vector defining the images to fill.
#' @param aFilter two element vector defining the quantiles to filter the anomaly. Ex. c(0.05,0.95).
#' @param fact an aggregation factor to be used to reduce the anomalies before the interpolation.
#' @param nPeriods number of periods used in the temporal window.
#' @param nYears number of years used in the temporal window.
#' @param fun function used to calculate the mean image, \code{mean}, \code{median} are acceptable functions.
#' @param snow.mode logical argument. If \code{TRUE} the filling process will be parallelized by \code{raster} package.
#' @param predictSE logical argument. If \code{TRUE} the filling process will calculate the standard error instead the prediction.
#' @param factSE the fact used in the standard error prediction to reduce the processing time.
#' @param out.name the name of the output images.
#' @param ... argument to allow function nestering.
#' \itemize{
#'   \item \code{AppRoot} if a path is assigned its save the filled time series of images in this location.
#' }
#' 
#' @return \code{RasterStack} object with the smoothed time series.
#' 
#' 
#' @examples
#' # load an example of NDVI time series in Navarre
#' data(ex.ndvi.navarre)
#' # plot data example
#' genPlotGIS(ex.ndvi.navarre)
#'
#' # fill the gaps
#' ndvi.filled<-genFillingIMA(ex.ndvi.navarre,
#'                            Img2Fill=c(1,2))
#' # Show the filled images
#' genPlotGIS(ndvi.filled)
#' # plot comparison of the cloud and the filled images
#' ndvi.comp<-stack(ex.ndvi.navarre[[1]],ndvi.filled[[1]],
#'                  ex.ndvi.navarre[[2]],ndvi.filled[[2]])
#' genPlotGIS(ndvi.comp,layout=c(2,2))
genFillingIMA<-function(imgTS,
                        Img2Fill=NULL,
                        aFilter=c(.05,.95),
                        fact=5,
                        nPeriods=3,
                        nYears=1,
                        fun=mean,
                        factSE=8,
                        snow.mode=FALSE,
                        predictSE=FALSE,
                        out.name="outname",
                        ...){
  args<-list(...)
  AppRoot<-defineAppRoot()
  stime<-Sys.time()
  if(snow.mode){
    beginCluster()
  }
  if("AppRoot"%in%names(args)){
    dir.create(paste0(AppRoot,"/",out.name),showWarnings = TRUE,recursive = TRUE)
  }
  # select images to predict
  if(is.null(Img2Fill)){
    Img2Fill<-1:nlayers(imgTS)
  }else{
    aux<-Img2Fill[Img2Fill%in%1:nlayers(imgTS)]
    if(is.null(aux)){stop("Target images in Img2Fill do not exist.")}
    if(length(aux)!=length(Img2Fill)){warning("Some of target images in Img2Fill do not exist in imgTS.")}
    Img2Fill<-aux
  }
  alldates<-genGetDates(names(imgTS))
  fillstack<-raster()
  for(i in Img2Fill){
    # get target date
    target.date<-alldates[i]
    message(paste0("Predicting period ",target.date))

    # define temporal neighbourhood
    neighbours<-dateNeighbours(ts.raster=imgTS,
                               target.date=target.date,
                               nPeriods=nPeriods,
                               nYears=nYears)
    # calculate mean image
    meanImage<-raster::calc(neighbours,fun=fun,na.rm=T)
    # get target image
    targetImage<-raster::subset(imgTS,which(format(genGetDates(names(imgTS)),"%Y%j")%in%format(target.date,"%Y%j")))
    # calculate anomaly
    anomaly<-targetImage-meanImage
    # remove extreme values
    qrm<-raster::quantile(anomaly,aFilter)
    anomaly[anomaly<qrm[1]|anomaly>qrm[2]]<-NA
    # reduce the resolution for tps
    aggAnomaly<-raster::aggregate(anomaly, fact=fact,fun=fun)

    # Tps model
    xy <- data.frame(xyFromCell(aggAnomaly, 1:ncell(aggAnomaly)))
    v <- getValues(aggAnomaly)
    tps <- suppressWarnings(Tps(xy, v))

    # smooth anomaly
    if(snow.mode){
      if(!predictSE){
        anomaly.prediction <- clusterR(anomaly, raster::interpolate, args=list(model=tps,fun=predict))
        # add mean image to predicted anomaly
        target.prediction<-anomaly.prediction+meanImage
      }else{
        se.size<-raster::aggregate(anomaly, fact=factSE,fun=fun)
        target.prediction <- clusterR(se.size, raster::interpolate, args=list(model=tps,fun=fields::predictSE))
      }

    }else{
      if(!predictSE){
        anomaly.prediction <- raster::interpolate(object=anomaly, model=tps,fun=predict)
        # add mean image to predicted anomaly
        target.prediction<-anomaly.prediction+meanImage
      }else{
        se.size<-aggregate(anomaly, fact=factSE,fun=fun)
        target.prediction <- raster::interpolate(object=se.size, model=tps,fun=fields::predictSE)
      }
    }

    # write filled images
    if("AppRoot"%in%names(args)){
      writeRaster(target.prediction,paste0(AppRoot,"/",out.name,"/",format(target.date,"%Y%j"),".tif"))
    }
    fillstack<-addLayer(fillstack,target.prediction)
  }
  if(snow.mode){
    endCluster()
  }
  names(fillstack)<-names(imgTS)[Img2Fill]
  etime<-Sys.time()
  message(paste0(length(Img2Fill)," images processed in ",MinSeg(etime,stime)))
  return(fillstack)
}

dateNeighbours<-function(ts.raster,
                         target.date,
                         nPeriods=1,
                         nYears=1){
  targetyear<-as.integer(format(target.date,"%Y"))
  tempolarPeriods<-format(as.Date((target.date-nPeriods):(target.date+nPeriods)),"%j")
  if("365"%in%tempolarPeriods&!"366"%in%tempolarPeriods){tempolarPeriods=c(tempolarPeriods,"366")}
  temporalYears<-(targetyear-nYears):(targetyear+nYears)
  temporalWindow<-paste0(rep(temporalYears,each=length(tempolarPeriods)),
                         rep(tempolarPeriods,length(temporalYears)))
  return(raster::subset(ts.raster,which(format(genGetDates(names(ts.raster)),"%Y%j")%in%temporalWindow)))
}

MinSeg=function(fim, ini){
  dif=as.numeric(difftime(fim, ini, units='mins'))
  return(paste0(sprintf('%02dm', as.integer(dif)), " ",
                sprintf('%02.0fs', (dif-as.integer(dif))*60),"."))
}
