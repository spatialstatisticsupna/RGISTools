#' Title
#'
#' @param search.res 
#' @param username 
#' @param password 
#' @param product 
#' @param verbose 
#'
#' @return
#'
#' @examples
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
    
    json_data2<-unlist(json_data,recursive=T)
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
    print(data.frame(fromJSON(rawToChar(res$content))))
  }
  if(verbose)message(paste0("Check the orders in ",getRGISToolsOpt("LS.ESPA.API"),getRGISToolsOpt("LS.ESPA.API.v"),"/list-orders"))
}