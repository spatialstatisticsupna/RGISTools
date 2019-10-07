#' Preview Landsat-7 or Landsat-8 satellite images
#'
#' \code{lsPreview} shows a preview of the \code{n}-th image from a set of 
#' search results.
#'
#' The function shows a preview of the \code{n}-th output image from a search
#' in the Landsat archives (\code{\link{ls7Search}} or \code{\link{ls8Search}}),
#' with \code{browseAvailable = "Y"}). The preview is downloaded from 
#' \href{https://www.usgs.gov/land-resources/nli/landsat/bulk-metadata-service}{USGS Bulk Metadata Service}.
#' Please, be aware that only some images may have a preview.
#'
#' @param searchres a \code{data.frame} with the results from 
#' \code{\link{ls7Search}} or \code{\link{ls8Search}}.
#' @param n a \code{numeric} argument identifying the location of the image in
#' \code{searchres}.
#' @param size a \code{numeric} argument specifying the size of the preview to
#' be displayed, in pixels.
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' src <- paste0(tempdir(),"/Path_for_downloading_folder")
#' # retrieve jpg images covering Navarre between 2011 and 2013
#' search.res <- ls7Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                         endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                         extent = ex.navarre,
#'                         precise = TRUE,
#'                         browseAvaliable = "Y",
#'                         AppRoot = src)
#' lsPreview(search.res, 1)
#' # filter the images with less than 1% pixels covered by clouds
#' search_cloudFree = subset(search.res, search.res$cloudCover < 1)
#' lsPreview(search_cloudFree, 1)
#' lsPreview(search_cloudFree, 2)
#' }
lsPreview<-function(searchres,n,lpos=c(3,2,1),add.Layer=FALSE,showWarnings = FALSE,...){
  ser<-searchres[n,]
  tmp <- tempfile()
  if(showWarnings){
    download.file(ser$browseURL,tmp,mode="wb")
  }else{
    suppressWarnings(download.file(ser$browseURL,tmp,mode="wb"))
  }
  
  r<-stack(tmp)
  lat<-unlist(ser[grepl("Latitude",names(ser))])
  lon<-unlist(ser[grepl("Longitude",names(ser))])
  extent(r)<-extent(min(lon),max(lon),min(lat),max(lat))
  proj4string(r)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
  
  if(showWarnings){
    return(genMapViewSession(r,lpos,lname=paste0("LS_",ser["path"],ser["row"],"_D",format(ser["acquisitionDate"],"%Y%j")),add.Layer=add.Layer,...))
  }else{
    return(suppressWarnings(genMapViewSession(r,lpos,lname=paste0("LS_",ser["path"],ser["row"],"_D",format(ser["acquisitionDate"],"%Y%j")),add.Layer=add.Layer,...)))
  }
}



