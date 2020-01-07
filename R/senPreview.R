#' Preview Sentinel-2 satellite images
#'
#' \code{senPreview} shows a preview of the \code{n}-th image from a set of 
#' search results on an interactive map. 
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
#' @param dates a vector with the dates being considered
#'   for previewing. This argument is mandatory if 
#'   \code{n} is not defined.
#' @param n a \code{numeric} argument identifying the row of the image in
#' \code{searchres}.
#' @param lpos vector argument. Defines the position of the red-green-blue
#' layers to enable false color visualization.
#' @param add.Layer logical argument. If \code{TRUE}, the function plots the 
#' image on an existing map. Allows combinations of images on a map using 
#' \code{\link{lsPreview}} and \code{\link{modPreview}} functions.
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param ... arguments for nested functions:
#'  \itemize{
#'   \item arguments allowed by the \code{viewRGB} function from \code{mapview}
#'   packages are valid arguments
#' }
#' @return this function does not return anything. It displays a preview of one
#' of the search results.
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # perform the search query
#' sres <- senSearch(startDate = as.Date("2018210","%Y%j"),
#'                   endDate = as.Date("2018218","%Y%j"),
#'                   platform = "Sentinel-2",
#'                   extent = ex.navarre,
#'                   product = "S2MSI1C",
#'                   username = "username",
#'                   password = "password")
#' # preview some images
#' senPreview(sres, username = "username", password = "password",n=1)
#' senPreview(sres, username = "username", password = "password",n=3, add.Layer =TRUE)
#' 
#' # show the dates in julian days
#' senGetDates(names(sres),format="%Y%j")
#' 
#' senPreview(sres, 
#'            username = "username", 
#'            password = "password", 
#'            dates = senGetDates(names(sres[3])))
#' }
senPreview<-function(searchres,username,password,n,dates,lpos=c(3,2,1),add.Layer=FALSE,verbose = FALSE,...){
  if(missing(dates)){
    return(.senPreviewRecursive(searchres=searchres,username=username,password=password,n=n,lpos=lpos,add.Layer=add.Layer,verbose=verbose,...))
  }else{
    searchres<-searchres[senGetDates(names(searchres))%in%dates]
    if(length(searchres)>0){
      .senPreviewRecursive(searchres=searchres,username=username,password=password,n=1,lpos=lpos,add.Layer=add.Layer,verbose=verbose,...)
      if(length(searchres)>1){
        for(x in 2:length(searchres)){
          .senPreviewRecursive(searchres=searchres,username=username,password=password,n=x,lpos=lpos,add.Layer=T,verbose=verbose,...)
        }
      }
      return(getRGISToolsOpt("GMapView"))
    }else{
      stop("There is no image for preview in ")
    }
    
  }
}
.senPreviewRecursive<-function(searchres,username,password,n,lpos=c(3,2,1),add.Layer=FALSE,verbose = FALSE,...){
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
  projection(r)<-st_crs(4326)$proj4string
  
  if(verbose){
    return(genMapViewSession(r,lpos,lname=paste0("SEN_",senGetTile(names(ser)),"_D",format(senGetDates(names(ser)),"%Y%j")),add.Layer=add.Layer,...))
  }else{
    return(suppressWarnings(genMapViewSession(r,lpos,lname=paste0("SEN_",senGetTile(names(ser)),"_D",format(senGetDates(names(ser)),"%Y%j")),add.Layer=add.Layer,...)))
  }
}