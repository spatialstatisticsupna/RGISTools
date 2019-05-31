#' Removes from R environment LS7 and LS8 metadata files
#'
#' \code{lsRemoveMetadata} frees the memory used by metadata data frame loaded in R enviroment.
#'
#' If the metadata file has been loaded by any landsat search function or using load
#' metadata funcion, \code{lsRemoveMetadata} removes anoy of both dataframes \code{.LS8MD} or \code{.LS7MD}.
#'
#' @examples
#' \dontrun{
#' #creates a MetaData folder and downloads the csv on working directory
#' ls8LoadMetadata()
#' lsRemoveMetadata()
#' }
lsRemoveMetadata<-function(){
  md <- ls(pos = ".GlobalEnv",all.names = T)
  rm(list = md[grepl(getRGISToolsOpt("LS8META.var"), md)|grepl(getRGISToolsOpt("LS7META.var"), md)], pos = ".GlobalEnv")
  gc()
}
