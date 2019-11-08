#' Make a request to ESPA for pre-processing Landsat images
#' 
#' \code{lsEspaOrder} makes a request to the EROS Centre Science Processing
#' Architecture (ESPA) to further process level-1 Landsat scenes.
#' 
#' Landsat Level-1 images are pre-processed on demand by the EROS Centre Science
#' Processing Architecture (ESPA). An order is placed to ESPA with the level-1
#' images that must be pre-processed (\code{search.res} and the requested 
#' final product (\code{product}). The products are identified by the following
#' short-names: 
#' \itemize{
#'   \item \code{toa}: for top of atmosphere reflectance.
#'   \item \code{bt}: for brightness temperature (thermal band TOA processing).
#'   \item \code{sr}: for surface reflectance.
#'   \item \code{sr_ndvi}: the normalized difference vegetation index from
#'   surface reflectance imagery.
#'   \item \code{sr_evi}: the enhanced vegetation index from surface
#'   reflectance imagery.
#'   \item \code{sr_savi}: the soil adjusted vegetation index from surface
#'   reflectance imagery.
#'   \item \code{sr_msavi}: the modified soil adjusted vegetation index from
#'   surface reflectance imagery.
#'   \item \code{sr_ndmi}: the normalized difference moisture index from surface
#'   reflectance imagery.
#'   \item \code{sr_nbr}: the normalized burn ratio from surface reflectance
#'   imagery.
#'   \item \code{sr_nbr2}: the normalized burn ratio 2 index from surface
#'   reflectance imagery.
#'   \item \code{prixel_qa}: for pixel quality assurance.
#' }
#' 
#' @param search.res the results from \code{\link{ls7Search}} or
#' \code{\link{ls8Search}}. 
#' @param username USGS's `EarthExplorer' username.
#' @param password USGS's `EarthExplorer' password.
#' @param product the acronym of the requested product (see the details).
#' @param verbose logical argument. If \code{TRUE}, the function prints the
#' running steps and warnings.
#' @return this function does not return anything.Ask for processing Landsat level 2 
#' images before the downloading.
#' @examples
#' \dontrun{
#' wdir <- paste0(tempdir(),"/Path_for_downloading_folder")
#' # search Landsat 7 level-1
#' sres <- ls7Search(startDate = as.Date("01-01-2017", "%d-%m-%Y"),
#'                   endDate = as.Date("07-01-2017", "%d-%m-%Y"),
#'                   lonlat = c(-1.64323, 42.81687),
#'                   AppRoot = wdir)
#' # request to ESPA the pre-pocessing of level-1 images 
#' # to get the surface reflectance
#' order <- lsEspaOrderImages(search.res = sres,
#'                            username = "username", 
#'                            password = "password", 
#'                            product = 'sr',
#'                            verbose = FALSE)
#' }
lsEspaOrderImages<-function(search.res,username,password,product=c("sr","source_metadata"),verbose=FALSE){

  # prepare the connetion
  c.handle = lsEspaCreateConnection(username,password)
  # 1 request per image

  for(ids in search.res$LANDSAT_PRODUCT_ID){
    url.products = paste0(getRGISToolsOpt("LS.ESPA.API"),getRGISToolsOpt("LS.ESPA.API.v"),'/available-products/', ids)
    if(verbose){message(paste0("Product url: \n",url.products))}
    req<-curl(url.products,handle = c.handle)
    html.text<-readLines(req)
    if(verbose){message(paste0("ESPA response json: \n",html.text))}
    json_data <- rjson::fromJSON(paste(html.text, collapse=""))
    if(verbose){message(paste0("ESPA response r obj: \n",json_data))}
    
    json_data2<-unlist(json_data,recursive=TRUE)
    products<-json_data2[grepl("products",names(json_data2))]
    if(length(products)==0){
      warning(paste0("Defined products are not available for image ",ids))
      warning(paste0("Products ",paste(json_data2,collapse = ", ")))
      next
    }
    if(any(!(product%in%products))){
      product<-product[product%in%products]
      if(length(product)==0){
        warning(paste0("Defined products are not available for image ",ids))
        warning(paste0("Products ",paste(json_data2,collapse = ", ")))
        next
      }
    }
    if(grepl("maintenance",html.text,ignore.case=TRUE))
      stop("System is currently unavailable due to maintenance.")
    
    #create the query
    json_data[[1]]$products<-product#c("sr","source_metadata")#product
    json_post<-list(projection=list(lonlat=NA),
                    format="gtiff",
                    resampling_method="cc",
                    note=getRGISToolsOpt("LS.ESPA.Request"))
    json_post<-append(json_post,json_data)
    query<-toEspaJSON(json_post)
    
    # perform the query with curl?
    # handle_setheaders(c.handle,
    #                   "Content-Type" = "application/json") 
    # POST method
    # handle_setform(c.handle,
    #                body =charToRaw(toEspaJSON(json_post)))
    # r <- curl_fetch_memory("https://espa.cr.usgs.gov/api/v1/order", c.handle)
    # rawToChar(r$content)
    
    # perform the query with httr
    if(verbose){message(paste0("ESPA query: \n",query))}
    res = POST(paste0(getRGISToolsOpt("LS.ESPA.API"),getRGISToolsOpt("LS.ESPA.API.v"),"/order"),
                      authenticate(username, password),
                      body = as.character(query))
    if(verbose){message(paste0("ESPA Order: \n",res))}
    #print the response
    #message(data.frame(fromJSON(rawToChar(res$content))))
  }
  if(verbose)message(paste0("Check the orders in ",getRGISToolsOpt("LS.ESPA.API"),getRGISToolsOpt("LS.ESPA.API.v"),"/list-orders"))
}