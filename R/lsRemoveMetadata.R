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
#' src <- paste0(tempdir(),"/Path_for_downloading_folder")
#' print(src)
#' ls8LoadMetadata(AppRoot = src)
#' lsRemoveMetadata()
#' }
lsRemoveMetadata<-function(){
  setRGISToolsOpt("LS7METADATA",NULL)
  setRGISToolsOpt("LS8METADATA",NULL)
  gc()
}
