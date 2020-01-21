#' Update EarthExplorer dataset names
#' 
#' The `EE.DataSets' option of 'RGISTools' contains all the dataset names
#' supported by EarthExplorer API. If these names changes, \code{\link{lsUpdateEEDataSets}}
#' updates these data. 
#'
#' @param username USGS's `EarthExplorer' username.
#' @param password USGS's `EarthExplorer' password.
#' @param logout logical argument. If \code{TRUE}, logges out from EarthExplorer
#' API
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#'
#' @return the dataset names in EarthExplorer API
#'
#' @examples
#' setRGISToolsOpt("EE.DataSets",NULL)
#' getRGISToolsOpt("EE.DataSets")
#' datasetNames<-lsUpdateEEDataSets(username = "username",
#'                                  password = "password")
#' 
#' getRGISToolsOpt("EE.DataSets")
lsUpdateEEDataSets<-function(username,password,logout=TRUE,verbose=FALSE){
  ApiKey<-getRGISToolsOpt("LS.EE.KEY")
  if(is.null(ApiKey)){
    loginEEAPI(username,password,verbose)
  }
  c.handle = new_handle()
  handle_setopt(c.handle,
                referer=getRGISToolsOpt("LS.EE.API"),
                useragent = getRGISToolsOpt("USERAGENT"),
                followlocation = TRUE ,
                autoreferer = TRUE)
  response<-curl(paste0(getRGISToolsOpt("LS.EE.API"),'datasets?jsonRequest={"apiKey":"', getRGISToolsOpt("LS.EE.KEY"), '"}'),
                       handle =c.handle)
  datasets<-fromJSON(suppressWarnings(readLines(response)))
  close(response)
  datasets<-datasets$data
  datasets<-unlist(lapply(datasets, function(x){x$datasetName}))
  
  if(logout){logoutEEAPI(verbose)}
  setRGISToolsOpt("EE.DataSets",datasets, env=optEnv)
  saveRDS(datasets,file.path(system.file("EE_data", package = "RGISTools"),"EE_DataSets.rds"))
  return(datasets)
}
