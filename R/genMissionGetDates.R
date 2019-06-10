#' Returns the capturing date of a Landsat 7 or 8 image
#'
#' \code{lsGetDates} extracts the capturing date of one or several Landsat images,
#' given their file paths or names. The function returns a date class object.
#'
#'  The function works with names or file paths having either of the two file extensions,
#'  \code{.tar.gz} or \code{.tif} files. The function accepts more than on file path or file names.
#'  If so, they should be provided as a list. Dates are returned as '\code{YYYY-mm-dd}' by default.
#'  If another format is required, it can be modified through the argument \code{date.format}.
#'
#' @param str a string or a list of strings defining the file path(s) or file name(s).
#' @param ... argument to allow function nestering:
#' \itemize{
#'   \item \code{date.format} modify the format of the date being returned.
#' }
#'
#' @examples
#' #example of getting the date from the name of a Landsat-8 image
#' str<-"LC82000312017010LGN01_B1.TIF"
#' dt<-lsGetDates(str)
#' print(dt)
#' print(format(df,"%Y%j"))
#'
#' #example of getting the date from the name of a Landsat-7 and 8 image
#' str<-c("LE72330822017009ASN01","LC82000312017010LGN01_B1.TIF")
#' dt<-lsGetDates(str)
#' print(dt)
#' print(format(df,"%Y%j"))
#'
lsGetDates<-function(str,...){
  arg<-list(...)
  #str<-"LC82000312017010LGN01"
  #str<-"LE72330822017009ASN01"
  if("date.format"%in%names(arg)){
    return(format(as.Date(substr(basename(str),10,16),"%Y%j"),format=arg$date.format))
  }else{
    return(as.Date(substr(basename(str),10,16),"%Y%j"))
  }
}


#' Returns the capturing date of Sentinel image
#'
#' \code{senGetDates} extracts the capturing date of one or several Sentinel
#' images, given their file paths or names. The function returns a date class object.
#'
#' The function works with names or file paths having either of the two file extensions,
#'  \code{.jp2} or \code{.tif} files. The function accepts more than on file path or file names.
#'  If so, they should be provided as a list. Dates are returned as '\code{YYYY-mm-dd}' by default.
#'  If another format is required, it can be modified through the argument \code{date.format}.
#'
#' @param str a string or a list of strings defining the file path(s) or file name(s).
#' @param ... argument to allow function nestering:
#' \itemize{
#'   \item \code{date.format} modify the format of the date being returned.
#' }
#'
#' @examples
#' #example of getting date from Sentinel2 image name
#' str<-c("S2A_MSIL1C_20170102T111442_N0204_R137_T30TWN_20170102T111441.SAFE",
#'        "S2A_OPER_PRD_MSIL1C_PDMC_20160308T090616_R094_V20160305T110109_20160305T110109")
#' dt<-senGetDates(str)
#' print(dt)
#' print(format(dt,"%Y%j"))
#' senGetDates(str,date.format="%Y%j")
#'
senGetDates<-function(str,...){
  arg<-list(...)
  name.first<-gsub(".SAFE","",basename(str))

  # Sentinel-2 images
  if(substr(name.first,1,2)[1]=="S2"){
    sizes<-sapply(name.first,nchar)
    sTime<-c()
    for(s in 1:length(sizes)){
      if(sizes[s]!=78){#new name convention
        sTime<-c(sTime,gsub(".*?\\s*(\\d{8}T\\d{6}).*", "\\1", names(sizes[s])))
      }else{#old name convention
        sTime<-c(sTime,gsub(".*?V\\s*(\\d{8}T\\d{6}).*", "\\1", names(sizes[s])))
      }
    }
  }else{
    stop("Introduced image path is not supported Sentinel image name")
  }

  if("date.format"%in%names(arg)){
    return(format(as.Date(gsub(".*\\s*(\\d{8}).*", "\\1", sTime),"%Y%m%d"),format=arg$date.format))
  }else{
    return(as.Date(gsub(".*\\s*(\\d{8}).*", "\\1", sTime),"%Y%m%d"))
  }
}



#' Returns the capturing date of Modis image
#'
#' \code{modGetDates} extracts the capturing date of one or several Modis
#' images, given their file paths or names. The function returns a date class object.
#'
#' The function works with names or file paths having either of the two file extensions,
#'  \code{.hdf} or \code{.tif} files. The function accepts more than on file path or file names.
#'  If so, they should be provided as a list. Dates are returned as '\code{YYYY-mm-dd}' by default.
#'  If another format is required, it can be modified through the argument \code{date.format}.
#'
#' @param str a string or a list of strings defining the file path(s) or file name(s).
#' @param ... argument to allow function nestering:
#' \itemize{
#'   \item \code{date.format} modify the format of the date being returned.
#' }
#'
#' @examples
#' # example of getting date from Sentinel2 image name
#' imgPath <- 'MYD13A2.A2016361.h17v04.006.2017285133407.hdf'
#' modGetDates(imgPath)
#'
#' # example of a list of the full file paths of Modis images, mixing .hdf and .tif files
#' imgsPaths<-list('MYD13A2.A2013297.h17v04.006.2015269230726.hdf',
#'                 'MYD13A2.A2013313.h17v04.006.2015271071143.tif')
#' modGetDates(imgsPaths)
modGetDates<-function(str,...){
  arg<-list(...)
  if("date.format"%in%names(arg)){
    return(format(as.Date(gsub(".*\\As*(\\d{7}).*", "\\1", str),"%Y%j"),format=arg$date.format))
  }else{
    return(as.Date(gsub(".*\\As*(\\d{7}).*", "\\1", str),"%Y%j"))
  }
}


