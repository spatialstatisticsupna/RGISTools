#' Downloads the images that have been pre-processed by ESPA
#' 
#' \code{lsEspaDownloadOrders} Downloads a set of images processed by the EROS Centre Science Processing 
#' Architecture (ESPA) through its API.
#' 
#' This function is part of a group of functions used to pre-process Landsat Level 1 images. 
#' The pre-processing is carried out by ESPA on demand. \code{\link{lsEspaDownloadOrders}}
#' downloads the images whose processing was completed according to \code{\link{lsEspaUpdateOrders}}. 
#' The function downloads and saves the imagery under the AppRoot directory. The function 
#' automatically creates two folders, called "raw" and "untar", to save the compressed and 
#' decompressed images respectively. The imagery is only decompressed when \code{untar = TRUE}.
#' 
#' @param orders a list of the requested images as returned by \code{\link{lsEspaGetOrderImages}}.
#' @param username login credentials to access the USGS EROS web service.
#' @param password login credentials to access the USGS EROS web service.
#' @param c.handle curl handler created with \code{curl} package containing the connection 
#' with password and username defined. This argument is mandatory if \code{username} and
#' \code{password} are not defined.
#' @param verbose logical argument. If TRUE, the function prints running stages and warnings.
#' @param n.attempts the number of attempts that the function has to carry out to download an image in case of corrupted files.
#' @param overwrite logical argument. If TRUE, overwrites the existing images with the same name.
#' @param untar logical argument. If TRUE, untars downloaded images.
#' @param ... argument for function nestering accepts:
#'  \itemize{
#'   \item \code{AppRoot} the directory to save the downloaded images.
#' }
#'
#' @examples
#' \dontrun{
#' src <- "Path_for_downloading_folder"
#' # Search Landsat 7 level-1
#' search.res <- ls7Search(startDate = as.Date("01-01-2017", "%d-%m-%Y"),
#'                         endDate = as.Date("15-01-2017", "%d-%m-%Y"),
#'                         lonlat = c(-1.64323, 42.81687),
#'                         AppRoot = src)
#' # Request to ESPA the prepocessing of level-1 images to get the surface reflectance
#' order <- lsEspaOrderImages(search.res = search.res,
#'                            username = "username", 
#'                            password = "password", 
#'                            product = 'sr',
#'                            verbose = FALSE)
#' # Get an ID for our request
#' orders <- lsEspaGetOrderImages(username = "username", 
#'                                password = "password")
#' # Follow up the status of the request
#' orders <- lsEspaUpdateOrders(orders = orders,
#'                              username = "username", 
#'                              password = "password")
#' # Saving directory
#' dir.ESPA <- file.path(src,"Landsat7","ESPA")
#' dir.create(dir.ESPA)
#' # Download when status says: complete
#' lsEspaDownloadOrders(orders = orders,
#'                      username = "username", 
#'                      password = "password",
#'                     untar = TRUE,
#'                     AppRoot = dir.ESPA)
#'}
lsEspaDownloadOrders<-function(orders,
                               username=NULL,
                               password=NULL,
                               c.handle=NULL,
                               verbose=FALSE,
                               overwrite=FALSE,
                               n.attempts=5,
                               untar=FALSE,
                               ...){
  AppRoot<-defineAppRoot(...)
  if(is.null(c.handle)){
    if(is.null(username)|is.null(username)){
      stop("c.handle or username and password are null.")
    }else{
      stopifnot(class(username)=="character")
      stopifnot(class(password)=="character")
      c.handle<-lsEspaCreateConnection(username,password)
    }
  }
  orders.name<-names(orders)
  
  if(length(orders)>0){
    while(TRUE){
      for(norder in 1:length(orders)){
        if(orders[[norder]]$Status=="complete"){
          #call to recursive function
          orders=lsDownEspa(orders=orders.name,
                            norder=norder,
                            images.order=orders,
                            c.handle=c.handle,
                            AppRoot=AppRoot,
                            verbose=verbose,
                            n.attempts=n.attempts,
                            untar=untar,
                            overwrite=overwrite)
        }
      }
      if(all(!unname(unlist(lapply(orders,function(x)return(x$Status))))%in%c("ordered","processing","complete"))){
        return(NULL)
      }
      message("Looking for processed images...")
      orders<-lsEspaUpdateOrders(orders=orders,c.handle=c.handle,verbose=verbose)
    }
  }else{message("There is no valid orders for downloading.")}
}


############################################################################
#  RECURSIVE FUNCTION
############################################################################
lsDownEspa<-function(orders,norder,c.handle,AppRoot,images.order,n.attempts,verbose=FALSE,overwrite=FALSE,untar=FALSE){
  r <- curl_fetch_memory(paste0(getRGISToolsOpt("LS.ESPA.API"),getRGISToolsOpt("LS.ESPA.API.v"),"/item-status/",orders[norder]), 
                         c.handle)
  jd<-rawToChar(r$content)
  json_data<-fromJSON(jd)
  if(verbose){message(paste0("ESPA order info: \n",json_data))}
  #durl<-json_data[[1]]$product_dload_url
  json_data<-unlist(json_data,recursive=T)
  durl<-json_data[grepl("product_dload_url",names(json_data))]
  if(verbose){message(paste0("Download url: \n",durl))}
  out.file.name<-file.path(AppRoot,basename(durl))
  if(((!file.exists(out.file.name))|overwrite)&n.attempts>0){#check md5
    message(paste0("Downloading ",basename(durl)," image."))
    curl_download(url=durl,destfile=out.file.name,handle =c.handle)
    md5.url<-unlist(json_data,recursive=T)
    md5.url<-md5.url[grepl("cksum_download_url",names(md5.url))]
    rmd5 <- curl_fetch_memory(md5.url, 
                              c.handle)
    md.file<-unlist(strsplit(rawToChar(rmd5$content)," "))
    
    if(genCheckMD5(out.file.name,toupper(md.file[1]))){
      images.order[[norder]]$Status<-"Downloaded"
    }else{
      message(paste0("ERROR CHECKING MD5 OF ",basename(durl)," IMAGE, TRYING THE DOWNLOAD PROCESS AGAIN."))
      file.remove(out.file.name)
      images.order=lsDownEspa(orders=orders,
                              norder=norder,
                              c.handle=c.handle,
                              AppRoot=AppRoot,
                              verbose=verbose,
                              overwrite=overwrite,
                              n.attempts=(n.attempts-1))
    }
  }else{
    if(n.attempts==0){
      message(paste0("ERROR DOWNLOADING ",toupper(basename(durl))," IMAGE!"))
      images.order[[norder]]$Status<-"Download error"
    }else{
      message(paste0("Image ",basename(durl)," already downloaded."))
      images.order[[norder]]$Status<-"Downloaded"
    }
    
  }
  #if untar=TRUE untar file from out.file.name to untar directory
  untar.dir<-file.path(dirname(dirname(out.file.name)),"untar",gsub(".tar.gz","",basename(out.file.name)))
  if((untar&!file.exists(untar.dir))|(untar&overwrite)){
    dir.create(untar.dir,showWarnings = FALSE)
    untar(out.file.name,exdir=untar.dir)
  }
  return(images.order)
}

