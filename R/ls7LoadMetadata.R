#' Load or update the Landsat-7 metadata file
#'
#' \code{ls7LoadMetadata} loads a \code{data.frame} called "\code{.LS7MD}"
#' with the names of the Landsat-7 images and their metadata. The metadata provides
#' auxiliary information, such as image quality, acquisition date, cloud cover,
#' etc. You can find a description of the metadata on the
#' \href{https://www.usgs.gov/land-resources/nli/landsat/bulk-metadata-service}{USGS's website}.
#'
#' All captures done by Landsat-7 are catalogued and documented in a unique csv
#' file. The size of the file could be larger than 360MB. The function downloads
#' and imports the metadata into `R', which may take several minutes (roughly 15
#' minutes in a Intel Core i7-4790, 16Gb of RAM and Hard Drive Device). The 
#' function creates an RData file with the csv metadata. Thus, every time
#' \code{ls7LoadMetadata} is called, this function loads the existing RData from
#' the \code{AppRoot} directory, which aims to reduce the loading time of the 
#' metadata in the future.
#'
#' @param update logical argument. If \code{TRUE}, updates the metadata file.
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param omit.question logical argument. If \code{TRUE}, the question about 
#' loading the metadata is omitted.
#' @param AppRoot the directory where the metadata file should be located. 
#' @param ... arguments for nested functions.
#'
#' @return this function does not return anything, but loads the “.LS7MD” 
#' \code{data.frame} on the environment of the `RGISTools' package.
#'
#' @examples
#' \dontrun{
#' # creates a MetaData folder and downloads the csv in the "Path_for_downloading_folder" directory
#' ls7LoadMetadata(AppRoot = file.path(tempdir(),"Path_for_downloading_folder"))
#'
#' # update the metadata file
#' ls7LoadMetadata(AppRoot = file.path(tempdir(),"Path_for_downloading_folder"), update = TRUE)
#' 
#' # get metadata data frame 
#' LS7MD <- getRGISToolsOpt("LS7METADATA")
#' head(LS7MD)
#' }
ls7LoadMetadata<-function(AppRoot,update=FALSE,verbose=TRUE,omit.question=TRUE,...){
  warning("Obsolete function, use lsSearch.")
  stopifnot(class(update)=="logical")
  arg<-list(...)
  AppRoot<-pathWinLx(AppRoot)
  #meta data directory and metadata file
  mdRawdir<-file.path(AppRoot,getRGISToolsOpt("LS7DownloadDir"),getRGISToolsOpt("LS7META.dir"))
  
  if(!file.exists(mdRawdir)){
    dir.create(mdRawdir,recursive=TRUE)
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
      .LS7MD<-read.csv(gzLS7,header=TRUE)
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
      setRGISToolsOpt("LS7METADATA", .LS7MD)
      
      et<-Sys.time()

      message(paste0("MetaData downloaded and saved on HDD for future queries. \nElapsed time: ",et-st," minutes.\nFile Saved in ",mdRdata))
    }else{
      stop("Metadata not loaded!")
    }
  }else{
    message("MetaData Rdata found! loading...")
    load(mdRdata)
    setRGISToolsOpt("LS7METADATA", .LS7MD)
  }
}

ls7IsMetaData<-function(){
  return(!is.null(getRGISToolsOpt("LS7METADATA")))
}
