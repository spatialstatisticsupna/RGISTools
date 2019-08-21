#' Preview Sentinel-2 satellite images
#'
#' \code{senPreview} shows a preview of the \code{n}-th image from a set of
#' search results.
#'
#' The function shows a preview of the \code{n}-th output image from a search
#' in Sentinel archives (\code{\link{modSearch}}). The preview is downloaded from
#' `SciHub's' website. Please, be aware that only some images may have a preview.
#' Credentials from an ESA’s `SciHub' account are needed, which can be obtained 
#' \href{https://scihub.copernicus.eu/dhus/#/self-registration}{here}.
#'
#' @param searchres a vector with the results from \code{\link{senSearch}}.
#' @param username ESA’s `SciHub' username.
#' @param password ESA’s `SciHub' password.
#' @param n a \code{numeric} argument identifying the row of the image in
#' \code{searchres}.
#' @param size a \code{numeric} argument specifying the size of the preview to
#' be displayed, in pixels.
#'
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # perform the search query
#' searchres <- senSearch(startDate = as.Date("2018210","%Y%j"),
#'                        endDate = as.Date("2018218","%Y%j"),
#'                        platform = "Sentinel-2",
#'                        extent = ex.navarre,
#'                        product = "S2MSI1C",
#'                        username = "username",
#'                        password = "password")
#' # preview some images
#' senPreview(searchres, 3, username = "username", password = "password")
#' senPreview(searchres, 1, username = "username", password = "password", 600)
#' 
#' # show the dates in julian days
#' senGetDates(names(searchres),format="%Y%j")
#' }
senPreview<-function(searchres,username,password,n,size=NULL){
  ser<-searchres[n]
  ser<-gsub('$value',"Products('Quicklook')/$value",ser,	fixed = TRUE)
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
  message(pic)
  file.remove(tmp)
  message(paste0("Printing the image ",names(ser),"."))
}
