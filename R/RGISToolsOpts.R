optEnv <- new.env()

assign("AppRoot", NULL, env=optEnv)

# connection
assign("USERAGENT","Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:58.0) Gecko/20100101 Firefox/58.0", env=optEnv)

# Landsat-8 options
# new 16/04/2019 landsat metadata web page url https://www.usgs.gov/land-resources/nli/landsat/bulk-metadata-service
assign("LS8META.csv", "https://landsat.usgs.gov/landsat/metadata_service/bulk_metadata_files/LANDSAT_8_C1.csv.gz", env=optEnv)
assign("LS8META.dir","MetaData", env=optEnv)
assign("LS8META.rdata","LS8MD.RData", env=optEnv)
assign("LS8META.var",".LS8MD", env=optEnv)
assign("LS8DownloadDir","Landsat8", env=optEnv)
assign("LS8METADATA", NULL, env=optEnv)

# Landsat-7 options
assign("LS7META.csv", "https://landsat.usgs.gov/landsat/metadata_service/bulk_metadata_files/LANDSAT_ETM_C1.csv.gz", env=optEnv)
assign("LS7META.dir","MetaData", env=optEnv)
assign("LS7META.rdata","LS7MD.RData", env=optEnv)
assign("LS7META.var",".LS7MD", env=optEnv)
assign("LS7DownloadDir","Landsat7", env=optEnv)
assign("LS7METADATA", NULL, env=optEnv)

# Landsat lvl2
assign("LS.ESPA.API","https://espa.cr.usgs.gov", env=optEnv)
assign("LS.ESPA.API.v","/api/v1", env=optEnv)
assign("LS.ESPA.Request","RGISTools request", env=optEnv)

# Modis options
assign("MODINVENTORY.url","https://lpdaacsvc.cr.usgs.gov/services/inventory", env=optEnv)
# assign("USGS.url","https://ers.cr.usgs.gov/", env=optEnv)
assign("USGS.url","https://earthexplorer.usgs.gov/", env=optEnv)
assign("USGS.login","https://ers.cr.usgs.gov/login/", env=optEnv)
assign("MODDownloadDir","Modis", env=optEnv)
assign("MODHDFDir","hdf", env=optEnv)
assign("MODTIFDir","tiff", env=optEnv)

assign("EARTHDATA.opensearch","https://cmr.earthdata.nasa.gov/opensearch", env=optEnv)


# Sentinel options
assign("SCIHUBAPIURL","https://scihub.copernicus.eu/apihub", env=optEnv)
assign("SCIHUBHUSURL","https://scihub.copernicus.eu/dhus", env=optEnv)
assign("SENDownloadDir","Sentinel", env=optEnv)
assign("SEN1Dir","Sentinel-1", env=optEnv)
assign("SEN2Dir","Sentinel-2", env=optEnv)
assign("SEN3Dir","Sentinel-3", env=optEnv)
assign("SENRAWDir","raw", env=optEnv)
assign("SENUNZIPDir","unzip", env=optEnv)
assign("SENUNIMAGESDir","images", env=optEnv)
assign("SENUNZIPEXT",".zip", env=optEnv)

#bands
assign("LS7BANDS",c(blue='B1',green='B2',red='B3',nir='B4',swir1='B5',tirs1='B6_VCID_1',tirs2='B6_VCID_2',swir2='B7',panchromatic='B8',quality='BQA',cloud="CLD"), env=optEnv)
assign("LS8BANDS",c(bluecoastal='B1',blue='B2',green='B3',red='B4',nir='B5',swir1='B6',swir2='B7',panchromatic='B8',cirrus='B9',tirs1='B10',tirs2='B11',quality='BQA',cloud="CLD"), env=optEnv)
assign("MOD09BANDS",c(red='B01_1',nir='B02_1',blue='B03_1',green='B04_1',tirs1='B05_1',swir1='B06_1',swir2='B07_1',quality='QC'), env=optEnv)

assign("SEN1BANDS",NULL, env=optEnv)
#https://www.spectralcam.com/2019/02/12/maia-s2-and-sentinel-2-multispectral-images-for-agriculture/
assign("SEN2BANDS",c(bluecoastal='B01',blue='B02',green='B03',red='B04',vegrededge="B05",vegrededge1="B06",vegrededge2="B07",nir='B08',narrownir='B8A',watervapour="B09",cirrus="B10",swir1='B11',swir2='B12',cloud='CLD',snow='SNW',rgb="TCI",preview='PVI',watervap='WVP'), env=optEnv)
assign("SEN3BANDS",NULL, env=optEnv)


#scale functions
assign("MOD09SCL",function(r,sc=1/16000){((r+100)*sc)}, env=optEnv)

#' Change the default value of an RGISTools option
#'
#' \code{setRGISToolsOpt} changes the default value of an `\code{RGISTools}' 
#' configuration variable. This function can be jointly used with 
#' \code{\link{showRGISToolsOpt}} and \code{\link{getRGISToolsOpt}}.
#'
#' @param opt the name of the option to be change in \code{character} format.
#' @param value the new value of the selected option.
#' @param env the environment where the `\code{RGISTools}' option is saved.
#'
#' @return this function does not return anything.
#'
#' @examples
#' # list avaliable options names
#' showRGISToolsOpt()
#' # list the URL where the Landsat-7 metadata is located
#' getRGISToolsOpt("LS7META.dir")
#' # change the URL where the Landsat-7 metadata is located
#' setRGISToolsOpt("LS7META.dir", "NewMTDir")
#' # list the URL where the Landsat-7 metadata is located
#' getRGISToolsOpt("LS7META.dir")
setRGISToolsOpt <- function(opt,value,env=optEnv) {
  assign(opt, value, envir=env)
}

#' Get the default value of an RGISTools option
#'
#' \code{getRGISToolsOpt} gets the current value of an `\code{RGISTools}' 
#' configuration variable. This function can be jointly used with
#' \code{\link{setRGISToolsOpt}} and \code{\link{showRGISToolsOpt}}.
#'
#' @param opt the name of the `\code{RGISTools}' setting to be returned.
#' @param env the environment where the `\code{RGISTools}' option are saved.
#'
#' @return an option of `RGISTools’ configuration variable.
#'
#' @examples
#' # list avaliable options names
#' showRGISToolsOpt()
#' # list the Sentinel-2 bands
#' getRGISToolsOpt("SEN2BANDS")
#' # list the Landsat-8 bands
#' getRGISToolsOpt("LS8BANDS")
#' # list the MODIS09 bands
#' getRGISToolsOpt("MOD09BANDS")
getRGISToolsOpt <- function(opt,env=optEnv) {
  return(get(opt, envir=env))
}

#' Print the name of all RGISTools configuration variables
#' 
#' \code{showRGISToolsOpt} prints the name of all options in `\code{RGISTools}' package.
#' This function can be jointly used with \code{\link{setRGISToolsOpt}} and 
#' \code{\link{getRGISToolsOpt}}.
#'
#' @param env the environment where the `\code{RGISTools}' option are saved.
#' 
#' @return a \code{character} vector with the names of the configuration variables.
#' 
#' @examples
#' showRGISToolsOpt()
showRGISToolsOpt<-function(env=optEnv){
  return(ls(env))
}




