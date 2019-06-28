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

# Landsat-7 options
assign("LS7META.csv", "https://landsat.usgs.gov/landsat/metadata_service/bulk_metadata_files/LANDSAT_ETM_C1.csv.gz", env=optEnv)
assign("LS7META.dir","MetaData", env=optEnv)
assign("LS7META.rdata","LS7MD.RData", env=optEnv)
assign("LS7META.var",".LS7MD", env=optEnv)
assign("LS7DownloadDir","Landsat7", env=optEnv)

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

#' Changes the default value of an RGISTools configuration variable
#'
#' \code{setRGISToolsOpt} changes the default value of an \code{RGISTools} option.
#'
#' @param opt the name of the option to change in character format
#' @param value the new value of selected option
#' @param env the environment where the \code{RGISTools} option are saved
#'
#' @examples
#' # list avaliable options names
#' showRGISToolsOpt()
#' # list the url where the landsat-7 metadata is located
#' getRGISToolsOpt("LS7META.dir")
#' # change the url where the landsat-7 metadata is located
#' setRGISToolsOpt("LS7META.dir", "NewMTDir")
#' # list the url where the landsat-7 metadata is located
#' getRGISToolsOpt("LS7META.dir")
setRGISToolsOpt <- function(opt,value,env=optEnv) {
  assign(opt, value, envir=env)
}

#' Gets the current value of an RGISTools configuration variable
#'
#' \code{getRGISToolsOpt} gets the current value of an \code{RGISTools} option.
#'
#' @param opt the name of the option to get.
#' @param env the environment where the \code{RGISTools} option are saved.
#'
#' @examples
#' # list avaliable options names
#' showRGISToolsOpt()
#' # list the assignation of Sentinel bands
#' getRGISToolsOpt("SEN2BANDS")
#' # list the assignation of Landsat-8 bands
#' getRGISToolsOpt("LS8BANDS")
#' # list the assignation of Modis 09 bands
#' getRGISToolsOpt("MOD09BANDS")
getRGISToolsOpt <- function(opt,env=optEnv) {
  return(get(opt, envir=env))
}

#' Prints the name of all RGISTools configuration variables
#' 
#' \code{showRGISToolsOpt} prints the name of all options in \code{RGISTools} package.
#'
#' @param env the environment where the \code{RGISTools} option are saved.
#' @examples
#' showRGISToolsOpt()
showRGISToolsOpt<-function(env=optEnv){
  return(ls(env))
}




