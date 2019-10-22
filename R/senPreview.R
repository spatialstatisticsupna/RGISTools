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
senPreview<-function(searchres,username,password,n,lpos=c(3,2,1),add.Layer=FALSE,showWarnings = FALSE,...){
  ser<-searchres[n]
  c.handle = new_handle()
  handle_setopt(c.handle,
                referer=getRGISToolsOpt("SCIHUBHUSURL"),
                useragent = getRGISToolsOpt("USERAGENT"),
                followlocation = TRUE ,
                autoreferer = TRUE ,
                username=username,
                password=password)
  bonquery<-gsub('/$value',"",ser,	fixed = TRUE)
  res<-curl(bonquery,handle = c.handle)
  html<-suppressWarnings(readLines(res))
  html<-html[grepl("coordinates",html)]
  html<-gsub("&lt;/gml:coordinates>","",html,fixed = T)
  html<-gsub("&lt;gml:coordinates>","",html,fixed = T)
  coor<-na.omit(as.numeric(unlist(lapply(unlist(strsplit(html,",")),strsplit," "))))
  lat<-coor[seq(1,length(coor),2)]
  lon<-coor[seq(2,length(coor),2)]
  
  ser<-gsub('$value',"Products('Quicklook')/$value",ser,	fixed = TRUE)
  tmp <- tempfile()
  
  image.url<-URLencode(ser)
  curl_download(image.url, destfile=tmp,handle = c.handle)
  r<-stack(tmp)
  extent(r)<-extent(min(lon),max(lon),min(lat),max(lat))
  projection(r)<-st_crs("+init=epsg:4326")$proj4string
  
  if(showWarnings){
    return(genMapViewSession(r,lpos,lname=paste0("SEN_",senGetTile(names(ser)),"_D",format(senGetDates(names(ser)),"%Y%j")),add.Layer=add.Layer,...))
  }else{
    return(suppressWarnings(genMapViewSession(r,lpos,lname=paste0("SEN_",senGetTile(names(ser)),"_D",format(senGetDates(names(ser)),"%Y%j")),add.Layer=add.Layer,...)))
  }
}
