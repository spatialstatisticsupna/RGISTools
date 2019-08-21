#' Load or update the Landsat-8 metadata file
#'
#' \code{ls8LoadMetadata} loads a \code{data.frame} called "\code{.LS8MD}"
#' with the names of the Landsat-8 images and their metadata. The metadata provides
#' auxiliary information, such as image quality, acquisition date, cloud cover,
#' etc. You can find a description of the metadata on the
#' \href{https://www.usgs.gov/land-resources/nli/landsat/bulk-metadata-service}{USGS's website}.
#'
#' All captures done by Landsat-8 are catalogued and documented in a unique csv
#' file. The size of the file could be larger than 210MB. The function downloads
#' and imports the metadata into R, which may take several minutes (roughly 7
#' minutes in a Intel Core i7-4790, 16Gb of RAM and Hard Drive Device). The 
#' function creates an RData file with the csv metadata. Thus, every time
#' \code{ls8LoadMetadata} is called, this function loads the existing RData from
#' the \code{AppRoot} directory, which aims to reduce the loading time of the 
#' metadata in the future.
#' 
#' @param AppRoot the directory where the metadata file should be located. 
#' @param update logical argument. If \code{TRUE}, updates the metadata file.
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param omit.question logical argument. If \code{TRUE}, the question about 
#' loading the metadata is omitted.
#' @param ... arguments for nested functions.
#'
#' @examples
#' \dontrun{
#'
#' # creates a MetaData folder and downloads the csv in the "Path_for_downloading_folder" directory
#' ls8LoadMetadata(AppRoot = paste0(tempdir(),"/Path_for_downloading_folder"))
#'
#' # update the metadata file
#' ls8LoadMetadata(AppRoot = paste0(tempdir(),"/Path_for_downloading_folder"), update = TRUE)
#' 
#' # if .LS8MD is already loaded you can see print its data
#' ls(all.names = TRUE)
#' head(.LS8MD)
#' }
ls8LoadMetadata<-function(AppRoot,update=FALSE,verbose=TRUE,omit.question=FALSE,...){
  stopifnot(class(update)=="logical")
  #define AppRoot
  arg<-list(...)
  AppRoot<-pathWinLx(AppRoot)
  #meta data directory and metadata file
  #meta data directory and metadata file
  mdRawdir<-file.path(AppRoot,getRGISToolsOpt("LS8DownloadDir"),getRGISToolsOpt("LS8META.dir"))
  if(!file.exists(mdRawdir)){
    dir.create(mdRawdir,recursive=TRUE)
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
      .LS8MD<-read.csv(gzLS8,header=TRUE)
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
    message("MetaData Rdata found! loading...")
    load(mdRdata,envir=globalenv())
  }

}

ls8IsMetaData<-function(){
  return(getRGISToolsOpt("LS8META.var")%in%ls(all.names = TRUE,envir=globalenv()))
}

