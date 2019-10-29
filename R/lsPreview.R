#' Preview Landsat-7 or Landsat-8 satellite images
#'
#' \code{lsPreview} shows a preview of the \code{n}-th image from a set of 
#' search results on a interactive map. 
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
#' @param lpos vector argument. Defines the position of the layers when RGB 
#' image is generated allowing false color visualizations.
#' @param add.Layer logical argument. If \code{TRUE}, the function plots the 
#' image over previous map. Allows combinations of images on a map using 
#' \code{\link{senPreview}} and \code{\link{modPreview}} functions.
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param ... arguments for nested functions:
#'  \itemize{
#'   \item arguments of \code{viewRGB} function from \code{mapview} packages are
#'   valid arguments
#' }
#' @return this function does not return anything. It previews on a map an image
#' from search result.
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' wdir <- paste0(tempdir(),"/Path_for_downloading_folder")
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
#' }
lsPreview<-function(searchres,n,lpos=c(3,2,1),add.Layer=FALSE,verbose = FALSE,...){
  ser<-searchres[n,]
  tmp <- tempfile()
  if(verbose){
    download.file(ser$browseURL,tmp,mode="wb")
  }else{
    suppressWarnings(download.file(ser$browseURL,tmp,mode="wb"))
  }
  
  r<-stack(tmp)
  lat<-unlist(ser[grepl("Latitude",names(ser))])
  lon<-unlist(ser[grepl("Longitude",names(ser))])
  extent(r)<-extent(min(lon),max(lon),min(lat),max(lat))
  projection(r)<-st_crs(4326)$proj4string
  
  if(verbose){
    return(genMapViewSession(r,lpos,lname=paste0("LS_",ser["path"],ser["row"],"_D",format(ser["acquisitionDate"],"%Y%j")),add.Layer=add.Layer,...))
  }else{
    return(suppressWarnings(genMapViewSession(r,lpos,lname=paste0("LS_",ser["path"],ser["row"],"_D",format(ser["acquisitionDate"],"%Y%j")),add.Layer=add.Layer,...)))
  }
}



