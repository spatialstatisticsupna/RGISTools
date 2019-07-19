#' Sinchronize Landsat-8 meta data file, for image search
#'
#' \code{ls8LoadMetadata} downloads the csv format file of metadata and loads a hidden data frame called .LS8MD into R.The metadata provides
#'  auxiliary information regarding Landsat-8 images repository such as image quality, acquisition
#'  data, cloud cover, etc. You can find a description of the metadata at
#'  \href{https://www.usgs.gov/land-resources/nli/landsat/bulk-metadata-service}{bulk-metadata-service}.
#'
#' All captures done by Landsat-8 are cataloged in a unique csv file. The size of the file
#' might be larger than 210MB. Therefore, the process of downloading and importing into R may take several
#' minutes (around 7 minutes in a Intel Core i7-4790, 16Gb of RAM and Hard Drive storage). The function creates an
#' RData file with the metadata csv. Thus, every time \code{ls8LoadMetadata} is called,
#' this function loads the existing RData in the \code{AppRoot}.
#'
#'
#' @param update logical argument. If \code{TRUE}, is mandatory to update the metadata file.
#' @param verbose logical argument. If \code{TRUE}, the function prints running stages and warnings.
#' @param omit.question logical argument. If \code{TRUE}, omits the question for ensuring the loading of the metadata file.
#' @param ... argument for function nestering
#' \itemize{
#'   \item \code{AppRoot} the directory where the metadata will be saved.
#' }
#'
#' @examples
#' \dontrun{
#' # creates a MetaData folder and downloads the csv on working directory
#' ls8LoadMetadata()
#'
#' # creates a MetaData folder and downloads the csv on "Path_for_metadata" directory
#' ls8LoadMetadata(AppRoot = "Path_for_metadata")
#'
#' # Force renew existing meta data csv
#' ls8LoadMetadata(update = TRUE)
#' 
#' # if .LS8MD is already loaded you can see print its data
#' ls(all.names = TRUE)
#' head(.LS8MD)
#' }
ls8LoadMetadata<-function(update=FALSE,verbose=TRUE,omit.question=FALSE,...){
  stopifnot(class(update)=="logical")
  #define AppRoot

  AppRoot<-defineAppRoot(...)

  #meta data directory and metadata file
  mdRawdir<-file.path(AppRoot,getRGISToolsOpt("LS8DownloadDir"),getRGISToolsOpt("LS8META.dir"))
  if(!file.exists(mdRawdir)){
    dir.create(mdRawdir,recursive=T)
  }
  mdRdata<-file.path(mdRawdir,getRGISToolsOpt("LS8META.rdata"))
  mdRawURL<-getRGISToolsOpt("LS8META.csv")
  message(paste0("Looking for metadata in ",mdRdata))
  if(!file.exists(mdRdata)|
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
        message("Reading metadata csv file, this task may take more than 7 minutes...")
      gzLS8<-gzfile(paste0(mdRawdir,"/",basename(mdRawURL)),'rt')

      #up to  minutes of data load
      .LS8MD<-read.csv(gzLS8,header=T)
      close(gzLS8)
      if(verbose)
        message("Processing csv data...")
      .LS8MD$acquisitionDate<-as.Date(.LS8MD$acquisitionDate)
      .LS8MD$dateUpdated<-as.Date(.LS8MD$dateUpdated)
      .LS8MD$browseURL<-as.character(.LS8MD$browseURL)
      .LS8MD$sceneID<-as.character(.LS8MD$sceneID)
      .LS8MD$LANDSAT_PRODUCT_ID<-as.character(.LS8MD$LANDSAT_PRODUCT_ID)
      .LS8MD$sceneStartTime<-as.character(.LS8MD$sceneStartTime)
      .LS8MD$sceneStopTime<-as.character(.LS8MD$sceneStopTime)
      .LS8MD$cartURL<-as.character(.LS8MD$cartURL)

      save(file = mdRdata,list=c(getRGISToolsOpt("LS8META.var")))
      #assign(getRGISToolsOpt("LS8META.var"), .LS8MD,envir = globalenv())#as global variable
      .LS8MD<<-.LS8MD
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

ls8IsMetaData<-function(){
  return(getRGISToolsOpt("LS8META.var")%in%ls(all.names = T,envir=globalenv()))
}

