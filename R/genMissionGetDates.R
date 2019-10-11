#' Return the capturing dates of Landsat-7 or Landsat-8 images
#'
#' \code{lsGetDates} reads the official name of a Landsat-7 or Landsat-8 image
#' and returns the capturing date, as a \code{Date} class object.
#'
#' The function works with file names (or their paths) regardless of their
#' extension. The function accepts more than on file path, which can be passed
#' as a \code{character} vector. Dates are returned as `\code{YYYY-mm-dd}' by
#' default. If another format is required, it can be modified through the 
#' argument \code{format}.
#'
#' @param str the full path(s) or official name(s) of the Landsat-7 or
#' Landsat-8 images from which the capturing date is retrieved.
#' @param ... arguments for nested functions:
#' \itemize{
#'   \item \code{format} the format of the date being returned.
#' }
#'
#' @examples
#' # getting the capturing date from the name of a Landsat-8 image
#' str <- "LC82000312017010LGN01.tar.gz"
#' dt <- lsGetDates(str)
#' print(dt)
#' print(format(dt, "%Y%j"))
#'
#' # getting the capturing date from the name of a Landsat-7 and a Landsat-8
#' # image
#' str <- c("LE72330822017009ASN01")
#' dt <- lsGetDates(str)
#' print(dt)
#'
lsGetDates<-function(str,...){
  arg<-list(...)
  bname<-basename(str)
  str<-gsub("\\..*","",bname)
  sizes<-sapply(str,nchar)
  sday<-c()
  for(s in 1:length(sizes)){
    if(sizes[s]==21){#new name convention
      sday<-c(sday,as.Date(substr(basename(str[s]),10,16),"%Y%j"))
    }else{#old name convention
      sday<-c(sday,as.Date(substr(basename(str[s]),11,18),"%Y%m%d"))
    }
  }
  sday<-as.Date(sday)
  if("format"%in%names(arg)){
    return(format(sday,format=arg$format))
  }else{
    return(as.Date(sday,"%Y%j"))
  }
}


#' Return the capturing dates of Sentinel-2 images
#'
#' \code{senGetDates} reads the official name of a Sentinel-2 image and returns
#' the capturing date, as a \code{Date} class object.
#'
#' The function works with file names (or their paths) regardless of their
#' extension. The function accepts more than on file path, which can be passed
#' as a \code{characters} vector. Dates are returned as `\code{YYYY-mm-dd}' by
#' default. If another format is required, it can be modified through the
#' argument \code{format}.
#'
#' @param str the full path(s) or official name(s) of the Sentinel
#' images from which the capturing date is retrieved.
#' @param ... arguments for nested functions:
#' \itemize{
#'   \item \code{format} the format of the date being returned.
#' }
#'
#' @examples
#' # getting the capturing date from the name of Sentinel-2 images
#' str <- c("S2A_MSIL1C_20170102T111442_N0204_R137_T30TWN_20170102T111441.SAFE",
#'        "S2A_OPER_PRD_MSIL1C_PDMC_20160308T090616_R094_V20160305T110109_20160305T110109")
#' dt <- senGetDates(str)
#' print(dt)
#' print(format(dt,"%Y%j"))
#' senGetDates(str, format = "%Y%j")
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

  if("format"%in%names(arg)){
    return(format(as.Date(gsub(".*\\s*(\\d{8}).*", "\\1", sTime),"%Y%m%d"),format=arg$format))
  }else{
    return(as.Date(gsub(".*\\s*(\\d{8}).*", "\\1", sTime),"%Y%m%d"))
  }
}



#' Return the capturing dates of MODIS images
#'
#' \code{modGetDates} reads the official name of MODIS images and returns its
#' capturing date, as \code{Date} class object.
#'
#' The function works with file names (or their paths) regardless of their
#' extension. The function accepts more than on file path, which can be passed
#' as a \code{list} of \code{characters}. Dates are returned as 
#' `\code{YYYY-mm-dd}' by default. If another format is required, it can be
#' modified through the argument \code{format}.
#'
#' @param str the full path(s) or official name(s) of MODIS
#' images from which the capturing date is retrieved.
#' @param ... arguments for nested functions:
#' \itemize{
#'   \item \code{format} the format of the date being returned.
#' }
#'
#' @examples
#' # getting the capturing date from the name of a MODIS image
#' imgPath <- 'MYD13A2.A2016361.h17v04.006.2017285133407.hdf'
#' modGetDates(imgPath)
#'
#' # a list of the full file paths of MODIS images, mixing .hdf and .tif files
#' imgsPaths<-list('MYD13A2.A2013297.h17v04.006.2015269230726.hdf',
#'                 'MYD13A2.A2013313.h17v04.006.2015271071143.tif')
#' modGetDates(imgsPaths, format = "%Y%j")
modGetDates<-function(str,...){
  arg<-list(...)
  dt<-as.Date(gsub(".*\\As*(\\d{7}).*", "\\1", str),"%Y%j")
  if(!any(is.na(dt))){
    if("format"%in%names(arg)){
      return(format(dt,format=arg$format))
    }else{
      return(dt)
    }
  }else{
    return(genGetDates(str,...))
  }
}


