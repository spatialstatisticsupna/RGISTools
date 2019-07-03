#' Remove from R environment LS7 and LS8 metadata files
#'
#' \code{lsRemoveMetadata} removes Landsat-7 and/or Landsat-8 (\code{.LS7MD}/\code{.LS8MD}) metadata from R enviroment.
#'
#' If the metadata file has been loaded by any Landsat search function or load
#' metadata function, \code{lsRemoveMetadata} removes both dataframes \code{.LS8MD} and/or \code{.LS7MD}.
#'
#' @examples
#' \dontrun{
#' # creates a MetaData folder and downloads the csv on working directory
#' ls8LoadMetadata()
#' lsRemoveMetadata()
#' }
lsRemoveMetadata<-function(){
  md <- ls(pos = ".GlobalEnv",all.names = T)
  rm(list = md[grepl(getRGISToolsOpt("LS8META.var"), md)|grepl(getRGISToolsOpt("LS7META.var"), md)], pos = ".GlobalEnv")
  gc()
}
