#' Remove the Landsat-7 or Lansat-8 metadata from the environment
#'
#' \code{lsRemoveMetadata} removes Landsat-7 and/or Landsat-8 (\code{.LS7MD}/
#' \code{.LS8MD}) metadata from the environment in `R'.
#'
#' The metadata file is loaded in `R' with \code{\link{ls7Search}}, 
#' \code{\link{ls8Search}} and \code{\link{lsDownload}}. \code{lsRemoveMetadata}
#' removes the metadata and frees up valuable RAM.
#'
#' @examples
#' \dontrun{
#' # creates a MetaData folder and downloads the csv file
#' # in the current working directory
#' ls8LoadMetadata()
#' lsRemoveMetadata()
#' }
lsRemoveMetadata<-function(){
  md <- ls(pos = ".GlobalEnv",all.names = TRUE)
  rm(list = md[grepl(getRGISToolsOpt("LS8META.var"), md)|grepl(getRGISToolsOpt("LS7META.var"), md)], pos = ".GlobalEnv")
  gc()
}
