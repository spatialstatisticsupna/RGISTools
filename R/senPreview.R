#' Allows a preview in R of Sentinel satellite images
#'
#' \code{senPreview} shows a preview of the \code{n} image from a set of \code{searchres}.
#'
#' The functions shows a preview of an image resulting from a search in Scihub platform.
#' A search with \code{\link{senSearch}} has to be done before proceeding with the preview.
#' The preview is downloaded from Scihub website, and the crediantials are needed.
#' Please, be aware that only some images have this feature.
#'
#'
#' @param searchres a data frame with the results from a search of Landsat images provided by the function \code{\link{senSearch}}.
#' @param username Scihub username.
#' @param password Scihub password.
#' @param n a number with the row corresponding to the image of interest in the search data frame.
#' @param size a number specifying the size of the preview to be displayed. The number determines pixels number.
#'
#' @examples
#' \dontrun{
#' data(ex.navarre)
#' # perform the search query
#' searchres <- senSearch(startDate = as.Date("2018210","%Y%j"),
#'                        endDate = as.Date("2018218","%Y%j"),
#'                        platform = "Sentinel-2",
#'                        intersects = ex.navarre,
#'                        product = "S2MSI1C",
#'                        username = "username",
#'                        password = "password")
#' # Preview some images
#' senPreview(searchres, 3, username = "username", password = "password")
#' senPreview(searchres, 1, username = "username", password = "password", 600)
#' }
senPreview<-function(searchres,username,password,n,size=NULL){
  ser<-searchres[n]
  ser<-gsub('$value',"Products('Quicklook')/$value",ser,	fixed = T)
  tmp <- tempfile()
  c.handle = new_handle()
  handle_setopt(c.handle,
                referer=getRGISToolsOpt("SCIHUBHUSURL"),
                useragent = getRGISToolsOpt("USERAGENT"),
                followlocation = TRUE ,
                autoreferer = TRUE ,
                username=username,
                password=password)
  image.url<-URLencode(ser)
  curl_download(image.url, destfile=tmp,handle = c.handle)
  pic<-image_read(tmp)
  pic <- image_resize(pic, size)
  print(pic)
  file.remove(tmp)
  message(paste0("Printing the image ",names(ser),"."))
}
