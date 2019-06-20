#' Mosaics a list of raster images
#'
#' \code{genMosaicList} makes a single mosaic from a list of raster images.
#'
#' This is a helper function used by other functions in this package. It combines
#' a list of raster images with different geolocation. If the images overlap, by
#' default the application selects the maximum value.
#'
#' @param imageList list of raster images.
#' @param fun the function applied to overlapping pixels.
#' @param verbose logical argument. If \code{TRUE} the function prints running stages and warnings.
#'
#' @examples
#' # create simulated rasters
#' img <- matrix(1:16, ncol = 4, byrow = TRUE)
#' r1 <- raster(img)
#' r2 <- r1
#' r3 <- r1
#' extent(r2) <- extent(1, 2, 1, 2)
#' extent(r3) <- extent(1, 2, 0, 1)
#' imageList <- list(r1, r2, r3)
#' # mosaic simulated rasters
#' mr <- genMosaicList(imageList)
#' spplot(mr)
genMosaicList <-function(imageList, fun="max",verbose=TRUE){
  if(length(imageList)>1){
    names(imageList) <- NULL
    imageList$fun<-fun
    imageList$tolerance<-0.000005
    mosaic <- do.call(mosaic, imageList)
    return(mosaic)
  }else{
    if(verbose){
      warning("There is only one raster image in imgList. No need to merge.")
    }
    return(imageList[[1]])
  }
}
