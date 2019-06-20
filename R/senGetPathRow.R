#' Returns the tile of Sentinel-2 images "TTTSSS" string format
#'
#' \code{senGetTile} reads the official name of a Sentinel image and returns its assigned tile. Get tiles information \href{https://sentinel.esa.int/web/sentinel/missions/sentinel-2/news/-/asset_publisher/Ac0d/content/sentinel-2-level-1c-product-tiling-grid-released}{here}.
#'
#' @param str the full path or official image name of the Sentinel image from which the tile is extracted.
#'
#' @return an string with the path and row of the image in "\code{TTTSSS}" format.
#'
#' @examples
#' #example of getting date from Sentinel2 image name
#' str <- c("S2A_MSIL1C_20170102T111442_N0204_R137_T30TWN_20170102T111441.SAFE",
#'          "S2A_OPER_PRD_MSIL1C_PDMC_20160308T090616_R094_V20160305T110109_20160305T110109")
#' pr <- senGetTile(str)
#' print(pr)
#'
senGetTile<-function(str){
  name.first<-gsub(".SAFE","",basename(str))
  # Sentinel-2 images
  if(substr(name.first,1,2)[1]=="S2"){
    sizes<-sapply(name.first,nchar)
    sTime<-c()
    for(s in 1:length(sizes)){
      if(sizes[s]!=78){#new name convention
        sTime<-c(sTime,gsub(".*\\s*([A-Za-z]{1}\\d{2}[A-Za-z]{3}).*", "\\1", names(sizes[s])))
      }else{#old name convention
        sTime<-c(sTime,"")
      }
    }
  }else{
    stop("Introduced image path is not supported Sentinel image name")
  }
  
  return(sTime)
}


#' Returns the relative orbit of the Sentinel-2 satellite 
#'
#' \code{senGetOrbit} reads the official name of a Sentinel image and returns relative orbit. Get relative orbit information \href{https://sentinel.esa.int/web/sentinel/missions/sentinel-2/satellite-description/orbit}{here}.
#'
#' @param str the full path or official image name of the Sentinel image from which the relative orbit is extracted.
#'
#' @return an string with the relative orbit of the image in 
#'
#' @examples
#' #example of getting date from Sentinel2 image name
#' str <- c("S2A_MSIL1C_20170102T111442_N0204_R137_T30TWN_20170102T111441.SAFE",
#'          "S2A_OPER_PRD_MSIL1C_PDMC_20160308T090616_R094_V20160305T110109_20160305T110109")
#' pr <- senGetOrbit(str)
#' print(pr)
#'
senGetOrbit<-function(str){
  name.first<-gsub(".SAFE","",basename(str))

  # Sentinel-2 images
  if(substr(name.first,1,2)[1]=="S2"){
    sizes<-sapply(name.first,nchar)
    sTime<-c()
    for(s in 1:length(sizes)){
      if(sizes[s]!=78){#new name convention
        sTime<-c(sTime,gsub(".*\\s*(N\\d{4}_R\\d{3}).*", "\\1", names(sizes[s])))
      }else{#old name convention
        sTime<-c(sTime,gsub(".*?\\s*(R\\d{3}).*", "\\1", names(sizes[s])))
      }
    }
  }else{
    stop("Introduced image path is not supported Sentinel image name")
  }
  
  return(sTime)
}
