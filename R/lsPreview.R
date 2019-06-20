#' Allows Landsat satellite images preview before its download
#'
#' \code{lsPreview} shows a preview of the n image from a set of search results
#'  \href{https://www.usgs.gov/land-resources/nli/landsat/bulk-metadata-service}{bulk-metadata-service}.
#'
#' The function shows a preview of an image resulting from a search in Landsat imagery metadata.
#' A search with \code{\link{ls7Search}} or \code{\link{ls8Search}} has to be carried before proceeding with the preview.
#' The preview is downloaded from Landsat’s website. Please, be aware that only some images have this feature.
#'
#'
#' @param searchres a data frame with  the results from a search of Landsat images provided by the functions \code{\link{ls7Search}} or \code{\link{ls8Search}}.
#' @param n a number of the image of interest in the search data frame.
#' @param size a number specifying the size of the preview to be displayed, in pixels.
#'
#' @examples
#' \dontrun{
#' data(ex.navarre)
#' # Retrieve jpg images covering Navarre region between 2011 and 2013
#' search.res <- ls7Search(startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                         endDate = as.Date("31-12-2013", "%d-%m-%Y"),
#'                         extent = ex.navarre,
#'                         precise = TRUE,
#'                         browseAvaliable = "Y")
#' lsPreview(search.res, 1)
#' # Filter the images with less than 1% pixels covered by clouds
#' search_cloudFree = subset(search.res, search.res$cloudCover < 1)
#' lsPreview(search_cloudFree, 1)
#' lsPreview(search_cloudFree, 2)
#' }
lsPreview<-function(searchres,n,size=NULL){
  ser<-searchres[n,]
  tmp <- tempfile()
  download.file(ser$browseURL,tmp,mode="wb")
  pic<-image_read(tmp)
  pic <- image_resize(pic, size)
  print(pic)
  file.remove(tmp)
  print(ser[c("acquisitionDate","sceneID","cloudCover","path","row")])
}
