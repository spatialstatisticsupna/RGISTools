#' Convert an HDF file into a set of GTiff files
#'
#' \code{modExtractHDF} converts the original format of MODIS images (HDF) into
#' GTiffs (one file for each layer). 
#'
#' HDF files cannot be directly loaded into R, so they must be converted into
#' GTiffs. To acomplish this task, the function \code{modExtractHDF}
#' borrows the \code{gdalwarp} and \code{gdal_translate} functions from the 
#' \code{gdalUtils} package. Further details about these functions can be found
#' in the corresponding package manual. GDAL and \code{gdalUtils} must be properly
#' installed to use \code{modExtractHDF}. GTiffs can be loaded in R using the
#' \code{raster} package.
#'
#' @param filesHDF  the full path where the HDF files are located.
#' @param shp  the shape file of the area of interest.
#' @param bFilter a vector containing the names of the bands to extract.
#' @param rm.band a vector containing the names of the bands excluded from the
#' extraction.
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing
#' images with the same name.
#' @param ... arguments for nested functions:
#' \itemize{
#'   \item \code{AppRoot} the directory where the extracted images are saved.
#' }
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' 
#' src<-"Path_for_downloading_folder"
#' src.mod <- file.path(src, "Modis","MOD11A1")
#' src.mod.hdf <- file.path(src.mod, "hdf")
#' img.list <- modSearch(product = "MOD11A1",
#'                       startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                       endDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                       collection = 6,
#'                       extent = ex.navarre,
#'                       AppRoot = src.mod.hdf)
#'                       
#' # download the images of the list
#' src.mod <- file.path(src, "Modis", "MOD11A1")
#' modDownSearch(searchres = img.list, 
#'               username = "username", 
#'               password = "password",
#'               AppRoot = src.mod.hdf)
#' 
#' src.tif<-file.path(src.mod,"tif")
#' 
#' # extract the first image
#' hdf.files <- list.files(src.mod.hdf, 
#'                         full.names = TRUE, 
#'                         pattern = "\\.hdf$")
#' first.hdf.file <- hdf.files[1]
#' modExtractHDF(filesHDF = first.hdf.file,
#'               AppRoot = src.tif)
#' }
modExtractHDF<-function(filesHDF,overwrite=FALSE,shp=NULL,verbose=FALSE,bFilter=NULL,rm.band=NULL,...){
    arg<-list(...)
    AppRoot<-defineAppRoot(...)
    dir.create(AppRoot,showWarnings = verbose)
    for(fileHDF in filesHDF){
      image.name<-gsub(".hdf","",basename(fileHDF))
        dir.create(paste0(AppRoot,"/",image.name),recursive = TRUE,showWarnings = verbose)
        message(paste0("Extracting bands from hdf file of image ",image.name))
        image.data<-gdalinfo(fileHDF)
        bands.names<-image.data[grepl(".*SUBDATASET_.*_NAME.*", image.data)]
        names<-gsub('.*":','',bands.names)
        names<-gsub(':','_',names)
        if(!is.null(bFilter)){
          exname<-names[Reduce("|", lapply(bFilter,grepl,names,ignore.case = TRUE))]
          bds<-which(names%in%exname)
        }else{
          bds<-1:length(names)
        }
        if(!is.null(rm.band)){
          n.exname<-names[Reduce("|", lapply(rm.band,grepl,names,ignore.case = TRUE))]
          bds.2<-which(names%in%n.exname)
          bds<-bds[!bds%in%bds.2]
        }
        

        for(i in bds){
          if((!file.exists(paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],".tif")))||overwrite){
            message(paste0("Extract band ",i))
            if("s_srs"%in%names(arg)){
              gdal_translate(fileHDF,
                             paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],"_temp.tif"),
                             sd_index=i)
              gdalwarp(srcfile=paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],"_temp.tif"),
                       dstfile= paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],".tif"),
                       s_srs=arg$s_srs)
              file.remove(paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],"_temp.tif"))
            }else{
              gdal_translate(fileHDF,
                             paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],".tif"),
                             sd_index=i,
                             overwrite=arg$overwrite)
            }

          }else{
            if(verbose){
              warning("File exists! not extracting...")
            }
          }
          if(!is.null(shp)){
            gdalwarp(srcfile=paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],".tif"),
                     dstfile=paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],"_cutted.tif"),
                     cutline=shp,
                     crop_to_cutline=TRUE,
                     overwrite=arg$overwrite)
          }
        }
      
    }

}
