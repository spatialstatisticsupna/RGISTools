#' Preview Landsat-7 or Landsat-8 satellite images
#'
#' \code{lsPreview} shows a preview of the \code{n}-th image from a set of 
#' search results on an interactive map. 
#'
#' The function shows a preview of the \code{n}-th output image from a search
#' in the Landsat archives (\code{\link{ls7Search}} or \code{\link{ls8Search}},
#' with \code{browseAvailable = "Y"}). The preview is downloaded from 
#' \href{https://www.usgs.gov/land-resources/nli/landsat/bulk-metadata-service}{USGS Bulk Metadata Service}.
#' Please, be aware that only some images may have a preview.
#'
#' @param searchres a \code{data.frame} with the results from 
#' \code{\link{ls7Search}} or \code{\link{ls8Search}}.
#' @param dates a vector of \code{Date}s being considered
#'   for previewing. This argument is mandatory if 
#'   \code{n} is not defined.
#' @param n a \code{numeric} argument identifying the location of the image in
#' \code{searchres}.
#' @param lpos vector argument. Defines the position of the red-green-blue
#' layers to enable false color visualization.
#' @param add.Layer logical argument. If \code{TRUE}, the function plots the 
#' image on an existing map. Allows combinations of images on a map using 
#' \code{\link{senPreview}} and \code{\link{modPreview}} functions.
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param ... arguments for nested functions:
#'  \itemize{
#'   \item arguments allowed by the \code{viewRGB} function from the 
#'   \code{mapview} packages are valid arguments.
#' }
#' @return this function does not return anything. It displays a preview of
#'  one of the search results.
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' wdir <- file.path(tempdir(),"Path_for_downloading_folder")
#' # retrieve jpg images covering Navarre between 2011 and 2013
#' sres <- ls7Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                   endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                   extent = ex.navarre,
#'                   precise = TRUE,
#'                   browseAvaliable = "Y",
#'                   AppRoot = wdir)
#' lsPreview(sres, 1)
#' # filter the images with less than 1% pixels covered by clouds
#' sres.cloud.free = subset(sres, sres$cloudCover < 1)
#' lsPreview(sres.cloud.free, 1)
#' lsPreview(sres.cloud.free, 2,add.Layer = TRUE)
#' # plot all the images in one date
#' lsPreview(sres.cloud.free,dates=as.Date("2013-09-04"))
#' }
lsPreview<-function(searchres,n,dates,lpos=c(3,2,1),add.Layer=FALSE,verbose = FALSE,...){
  if(class(searchres)!="ls7res"&&class(searchres)!="ls8res"&&class(searchres)!="lsres"){stop("A response from landsat 7-8 search function is needed.")}
  class(searchres)<-"data.frame"
  if(missing(dates)){
    return(.lsPreviewRecursive(searchres=searchres,n=n,lpos=lpos,add.Layer=add.Layer,verbose=verbose,...))
  }else{
    searchres<-searchres[as.Date(unlist(searchres$acquisitionDate))%in%dates,]
    if(nrow(searchres)>0){
      .lsPreviewRecursive(searchres=searchres,n=1,lpos=lpos,add.Layer=add.Layer,verbose=verbose,...)
      if(nrow(searchres)>1){
        for(x in 2:nrow(searchres)){
          .lsPreviewRecursive(searchres=searchres,n=x,lpos=lpos,add.Layer=T,verbose=verbose,...)
        }
      }
    return(getRGISToolsOpt("GMapView"))
    }else{
      stop("There is no image for preview in ")
    }
    
  }
}
.lsPreviewRecursive<-function(searchres,n,dates,lpos,add.Layer,verbose,...){
  ser<-searchres[n,]
  tmp <- tempfile()
  if(verbose){
    download.file(unlist(ser$browseURL),tmp,mode="wb")
  }else{
    suppressWarnings(download.file(unlist(ser$browseURL),tmp,mode="wb"))
  }
  r<-stack(tmp)
  lat<-unlist(ser[grepl("Latitude",names(ser))])
  lon<-unlist(ser[grepl("Longitude",names(ser))])
  extent(r)<-extent(min(lon),max(lon),min(lat),max(lat))
  projection(r)<-st_crs(4326)$proj4string
  
  if(verbose){
    return(genMapViewSession(r,lpos,lname=paste0("LS_",ser["path"],ser["row"],"_D",format(as.Date(unlist((ser["acquisitionDate"]))),"%Y%j")),add.Layer=add.Layer,...))
  }else{
    return(suppressWarnings(genMapViewSession(r,lpos,lname=paste0("LS_",ser["path"],ser["row"],"_D",format(as.Date(unlist((ser["acquisitionDate"]))),"%Y%j")),add.Layer=add.Layer,...)))
  }
}


