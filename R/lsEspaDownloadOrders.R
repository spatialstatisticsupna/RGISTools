#' Title
#'
#' @param images.order 
#' @param username 
#' @param password 
#' @param c.handle 
#' @param verbose 
#' @param n.attempts 
#' @param ... 
#' @param overwrite 
#' @param untar 
#'
#' @return
#'
#' @examples
lsEspaDownloadOrders<-function(images.order,
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
  orders<-names(images.order)
  
  if(length(orders)>0){
    while(TRUE){
      for(norder in 1:length(images.order)){
        if(images.order[[norder]]$Status=="complete"){
          #call to recursive function
          images.order=lsDownEspa(orders=orders,
                                  norder=norder,
                                  images.order=images.order,
                                  c.handle=c.handle,
                                  AppRoot=AppRoot,
                                  verbose=verbose,
                                  n.attempts=n.attempts,
                                  untar=untar,
                                  overwrite=overwrite)
        }
      }
      if(all(!unname(unlist(lapply(images.order,function(x)return(x$Status))))%in%c("ordered","processing","complete"))){
        return(NULL)
      }
      message("Looking for processed images...")
      images.order<-lsEspaUpdateOrders(images=images.order,c.handle=c.handle,verbose=verbose)
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
      #if untar=TRUE untar file from out.file.name to untar directory
      if(untar){
        untar.dir<-file.path(dirname(dirname(out.file.name)),"untar",gsub(".tar.gz","",basename(out.file.name)))
        dir.create(untar.dir,showWarnings = FALSE)
        untar(out.file.name,exdir=untar.dir)
      }
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
  return(images.order)
}

