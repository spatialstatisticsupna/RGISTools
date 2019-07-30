#' Convert an HDF file into a set of GTiff files
#'
#' \code{modExtractHDF} converts the original Modis image format (HDF) into a file
#' format loadable by R (GTiff). The function extracts all image layers. 
#' This function requires the correct installation of GDAL library.
#'
#' HDF files cannot be directly loaded into R. The function \code{\link{modExtractHDF}}
#' borrows \code{gdalwarp} and \code{gdal_translate} functions from the \code{gdalUtils} package.
#' These functions are used to convert the '.hdf' files into '.tif' files. The '.tif'
#' files can be loaded in R using the \code{raster} package. Go to \code{\link{modSearch}} and \code{\link{modDownload}}
#' for further details about these functions. Further details about the \code{gdalUtils} functions
#' in \code{gdalUtils} package manual.
#'
#' @param filesHDF  the full path where the HDF files are stored.
#' @param shp  the shape file of the area of interest.
#' @param bFilter a vector containing the names of the bands to extract.
#' @param rm.band a vector containing the names of the bands to not extract.
#' @param verbose logical argument. If \code{TRUE}, the function prints running stages and warnings.
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing images with the same name.
#' @param ... argument for function nestering:
#' \itemize{
#'   \item \code{AppRoot} the directory where the extracted images should be located
#' }
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' 
#' src<-"Path_for_downloading_folder"
#' img.list <- modSearch(product = "MOD11A1",
#'                       startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                       endDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                       collection = 6,
#'                       extent = ex.navarre)
#'                       
#' # download first image of image list
#' modDownSearch(searchres = img.list, 
#'               username = "username", 
#'               password = "password",
#'               AppRoot = src)
#' 
#' src.hdf<-file.path(src,"MOD09GA","hdf")
#' src.tif<-file.path(src,"MOD09GA","tif")
#' 
#' # Extract one layer from downloaded image
#' hdf.files <- list.files(src.hdf, 
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
        dir.create(paste0(AppRoot,"/",image.name),recursive = T,showWarnings = verbose)
        print(paste0("Extracting bands from hdf file of image ",image.name))
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
            print(paste0("Extract band ",i))
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
