#' Sinchronize Landsat-7 meta data file for image search.
#'
#' \code{\link{ls7LoadMetadata}} loads a data frame called "\code{.LS7MD}"
#'  with the names of the Landsat-7 images and their metadata. The metadata provides
#'  auxiliary information regarding Landsat-7 repository, such as image quality, acquisition
#'  data, cloud cover, etc. You can find a description of the metadata at
#'  \href{https://www.usgs.gov/land-resources/nli/landsat/bulk-metadata-service}{bulk-metadata-service}.
#'
#' All captures done by Landsat-7 are cataloged in a unique csv file. The size of the file
#' might be larger than 360MB. Therefore, the process of downloading and importing into R may take several
#' minutes (15 minutes in a Intel Core i7-4790, 16Gb of RAM and Hard Drive Device roughly). The function creates an
#' RData file with the csv metadata. Thus, every time \code{\link{ls7LoadMetadata}} is called,
#' this function loads the existing RData in the Approot. This is intended to reduce the loading
#' time of metadata in the future.
#'
#'
#'
#' @param update logical argument. If \code{TRUE}, updates the metadata file.
#' @param verbose logical argument. If \code{TRUE}, the function prints running stages and warnings.
#' @param omit.question logical argument. If \code{TRUE}, omits the question for ensuring the loading of the metadata file.
#' @param ... argument for function nestering:
#' \itemize{
#'   \item \code{AppRoot} this option specifies the downloading/searching directory of the metadata file. 
#' }
#'
#' @examples
#' \dontrun{
#' # creates a MetaData folder and downloads the csv on working directory
#' ls7LoadMetadata()
#'
#' # creates a MetaData folder and downloads the csv on "Path_for_downloading_folder" directory
#' ls7LoadMetadata(AppRoot = "Path_for_downloading_folder")
#'
#' # Force renew existing meta data csv
#' ls7LoadMetadata(update = TRUE)
#' # if .LS7MD is already loaded you can see print its data
#' ls(all.names = TRUE)
#' head(.LS7MD)
#' }
ls7LoadMetadata<-function(update=FALSE,verbose=TRUE,omit.question=TRUE,...){
  stopifnot(class(update)=="logical")
  arg<-list(...)
  AppRoot<-defineAppRoot(...)

  #meta data directory and metadata file
  mdRawdir<-file.path(AppRoot,getRGISToolsOpt("LS7DownloadDir"),getRGISToolsOpt("LS7META.dir"))
  
  if(!file.exists(mdRawdir)){
    dir.create(mdRawdir,recursive=T)
  }
  mdRdata<-file.path(mdRawdir,getRGISToolsOpt("LS7META.rdata"))
  mdRawURL<-getRGISToolsOpt("LS7META.csv")
  message(paste0("Looking for metadata in ",mdRdata))
  if(is.na(file.info(mdRdata)$ctime)|
     update){
    message("MetaData Rdata not found or out of date! \nThis task may take few minutes.")
    if(genAskForYN("Do you want to continue? (Y)es/(N)o: ",omit.question=omit.question)){
      st<-Sys.time()
      if(verbose)
        message("Downloading metadata file...")

      c.handle = new_handle()
      handle_setopt(c.handle,
                    useragent = getRGISToolsOpt("USERAGENT"))
      curl_download(mdRawURL,
                    destfile=paste0(mdRawdir,"/",basename(mdRawURL)),
                    handle = c.handle)
      if(verbose)
        message("Reading metadata csv file, this task may take more than 15 minutes...")
      gzLS7<-gzfile(paste0(mdRawdir,"/",basename(mdRawURL)),'rt')
      .LS7MD<-read.csv(gzLS7,header=T)
      close(gzLS7)

      if(verbose)
        message("Processing csv data...")
      .LS7MD$acquisitionDate<-as.Date(.LS7MD$acquisitionDate)
      .LS7MD$dateUpdated<-as.Date(.LS7MD$dateUpdated)
      .LS7MD$browseURL<-as.character(.LS7MD$browseURL)
      .LS7MD$sceneID<-as.character(.LS7MD$sceneID)
      .LS7MD$LANDSAT_PRODUCT_ID<-as.character(.LS7MD$LANDSAT_PRODUCT_ID)
      .LS7MD$sceneStartTime<-as.character(.LS7MD$sceneStartTime)
      .LS7MD$sceneStopTime<-as.character(.LS7MD$sceneStopTime)
      .LS7MD$cartURL<-as.character(.LS7MD$cartURL)

      save(file = mdRdata,list=c(getRGISToolsOpt("LS7META.var")))
      #assign(getRGISToolsOpt("LS7META.var"), .LS7MD,envir = globalenv())#as global variable
      .LS7MD<<-.LS7MD
      et<-Sys.time()

      message(paste0("MetaData downloaded and saved on HDD for future queries. \nElapsed time: ",et-st," minutes.\nFile Saved in ",mdRdata))
    }else{
      stop("Metadata not loaded!")
    }
  }else{
    print("MetaData Rdata found! loading...")
    load(mdRdata,envir=globalenv())
  }
}

ls7IsMetaData<-function(){
  return(getRGISToolsOpt("LS7META.var")%in%ls(all.names = T,envir=globalenv()))
}
