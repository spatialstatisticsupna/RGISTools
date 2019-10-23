#' Preview MODIS satellite images
#'
#' \code{modPreview} shows a preview of the \code{n}-th image from a set of 
#' search results on a interactive map. 
#'
#' The function shows a preview of the \code{n}-th output image from a search
#' in the MODIS archives (\code{\link{modSearch}}), with 
#' \code{resType = "browseurl"}). The preview is downloaded from the
#' \href{https://earthdata.nasa.gov}{`EarthData' Platform}.
#' Please, be aware that only some images may have a preview.
#'
#' @param searchres a vector with the results from \code{\link{modSearch}}.
#' @param n a \code{numeric} argument identifying the location of the image in
#' \code{searchres}.
#' @param lpos vector argument. Defines the position of the layers when RGB 
#' image is generated allowing false color visualizations.
#' @param add.Layer logical argument. If \code{TRUE}, the function plots the 
#' image over previous map. Allows combinations of images on a map using 
#' \code{\link{lsPreview}} and \code{\link{senPreview}} functions.
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param ... arguments for nested functions:
#'  \itemize{
#'   \item arguments of \code{viewRGB} function from \code{mapview} packages are
#'   valid arguments
#' }
#' 
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # retrieve jpg images covering Navarre region between 2011 and 2013
#' searchres <- modSearch(product = "MOD09GA",
#'                       startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                       endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                       collection = 6,
#'                       resType = "browseurl",
#'                       extent = ex.navarre)
#'                       
#' modPreview(searchres,1)
#' modPreview(searchres,2,add.Layer=T)
#' }
modPreview<-function(searchres,n,lpos=c(3,2,1),add.Layer=FALSE,verbose = FALSE,...){
  ser<-searchres[n]
  tmp <- tempfile()
  if(verbose){
    download.file(ser,tmp,mode="wb")
  }else{
    suppressWarnings(download.file(ser,tmp,mode="wb"))
  }
  pic<-stack(tmp)

  pr<-modGetPathRow(ser)
  ho<-as.numeric(substr(pr,2,3))
  ve<-as.numeric(substr(pr,5,6))
  
  extent(pic)<-extent(st_transform(mod.tiles[mod.tiles$Name==paste0("h:",ho," v:",ve),],crs=st_crs(54008)))
  projection(pic)<-st_crs(54008)$proj4string
  
  if(verbose){
    return(genMapViewSession(pic,lpos,lname=paste0("MOD_",ho,"_",ve,"_D",format(modGetDates(ser),"%Y%j")),add.Layer=add.Layer,...))
  }else{
    return(suppressWarnings(genMapViewSession(pic,lpos,lname=paste0("MOD_",ho,"_",ve,"_D",format(modGetDates(ser),"%Y%j")),add.Layer=add.Layer,...)))
  }
}
