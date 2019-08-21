#' Fill data gaps and smooth outliers in a time series of satellite images
#' 
#' \code{genSmoothingIMA} is the implementation of a spatio temporal method 
#' called image mean anomaly (IMA) for gap filling and smoothing satellite
#' data \insertCite{militino2019interpolation}{RGISTools}.
#'
#' This filling/smoothing method was developed by 
#' \insertCite{militino2019interpolation;textual}{RGISTools}. This technique decomposes
#' a time series of images into a new series of mean and anomaly images. The
#' procedure applies the smoothing algorithm over the anomaly images. The 
#' procedure requires a proper definition of a temporal neighbourhood for the
#' target image and aggregation factor.
#'
#' @references \insertRef{militino2019interpolation}{RGISTools}
#'
#' @param imgTS a \code{RasterStack} class argument containing a time series of
#' satellite images. Layer names should contain the date of the image in
#' "\code{YYYYJJJ}" format.
#' @param Img2Fill a \code{vector} class argument defining the images to be 
#' filled/smoothed.
#' @param nDays a \code{numeric} argument with the number of previous and 
#' subsequent days that define the temporal neighborhood.
#' @param nYears a \code{numeric} argument with the number of previous and 
#' subsequent years that define the temporal neighborhood.
#' @param aFilter a \code{vector} with the lower and upper quantiles that define
#' the outliers of the anomalies. Ex. c(0.05,0.95).
#' @param fact a \code{numeric} argument with an aggregation factor of the 
#' anomalies before the interpolation.
#' @param fun a \code{function} used to aggregate the image of anomalies. Both
#' \code{mean} (default) or \code{median} are acceptted.
#' @param snow.mode logical argument. If \code{TRUE}, the filling process will
#' be parallelized using the `\code{raster}' package.
#' @param predictSE calculate the standard error instead the prediction.
#' @param factSE the \code{fact} used in the standard error prediction.
#' @param out.name the name of the folder containing the smoothed/filled images
#' when saved in the Hard Disk Device (HDD). 
#' @param ... arguments for nested functions:
#' \itemize{
#'   \item \code{AppRoot} the path where the filled/smoothed time series of
#'   images will be saved in GTiff format.
#' }
#' 
#' @return a \code{RasterStack} with the filled/smoothed images.
#' 
#' 
#' @examples
#' # load an example of NDVI time series in Navarre
#' data(ex.ndvi.navarre)
#' # the 2 images to be filled and the neighbourhood
#' genPlotGIS(ex.ndvi.navarre)
#'
#' # filled images
#' ndvi.filled <- genSmoothingIMA(ex.ndvi.navarre,
#'                                Img2Fill = c(1,2))
#' # show the filled images
#' genPlotGIS(ndvi.filled)
#' # plot comparison of the cloud and the filled images
#' ndvi.comp <- stack(ex.ndvi.navarre[[1]], ndvi.filled[[1]],
#'                    ex.ndvi.navarre[[2]], ndvi.filled[[2]])
#' genPlotGIS(ndvi.comp, layout=c(2, 2))
genSmoothingIMA<-function(imgTS,
                          Img2Fill = NULL,
                          nDays = 3,
                          nYears=1,
                          fact = 5,
                          fun=mean,
                          aFilter = c(.05,.95),
                          factSE=8,
                          predictSE=FALSE,
                          snow.mode=FALSE,
                          out.name="outname",
                          ...){
  args<-list(...)
  stime<-Sys.time()
  if(snow.mode){
    beginCluster()
  }
  if("AppRoot"%in%names(args)){
    args$AppRoot<-pathWinLx(args$AppRoot)
    dir.create(paste0(args$AppRoot,"/",out.name),showWarnings = TRUE,recursive = TRUE)
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
  if(all(is.na(alldates))){stop("The name of the layers has to include the date and it must be in julian days (%Y%j) .")}
  fillstack<-raster()
  for(i in Img2Fill){
    # get target date
    target.date<-alldates[i]
    message(paste0("Predicting period ",target.date))

    # define temporal neighbourhood
    neighbours<-dateNeighbours(ts.raster=imgTS,
                               target.date=target.date,
                               nPeriods=nDays,
                               nYears=nYears)
    message(paste0("   - Size of the neighbourhood: ",nlayers(neighbours)))
    # calculate mean image
    meanImage<-raster::calc(neighbours,fun=fun,na.rm=TRUE)
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
      writeRaster(target.prediction,paste0(args$AppRoot,"/",out.name,"/",format(target.date,"%Y%j"),".tif"))
    }else{
      fillstack<-addLayer(fillstack,target.prediction)
    }
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
