#' Convert an HDF file into a set of GTiff files
#'
#' \code{modExtractHDF} converts the original format of MODIS images (hierarchical
#' data format or HDF) into GTiffs (one file for each layer). 
#'
#' HDF files cannot be directly loaded into `R', so they must be converted into
#' GTiffs. To acomplish this task, the function \code{modExtractHDF}
#' borrows the \code{gdalwarp} and \code{gdal_translate} functions from the 
#' `\code{gdalUtils}' package. Further details about these functions can be found
#' in the corresponding package manual. `GDAL' and `\code{gdalUtils}' must be 
#' properly installed to use \code{modExtractHDF}. GTiffs can be loaded in `R'
#' using the `\code{raster}' package.
#'
#' @param filesHDF  the full path where the HDF files are located.
#' @param AppRoot the directory where the extracted images are saved.
#' @param region a \code{Spatial*}, projected \code{raster*}, or \code{sf*} class object 
#' defining the area of interest for image masking.
#' @param bFilter a vector containing the names of the bands to extract.
#' @param rm.band a vector containing the names of the bands excluded from the
#' extraction.
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing
#' images with the same name.
#' @param ... arguments for nested functions.
#'\itemize{
#'        \item \code{dates} a vector with the capturing dates being considered
#'   for downloading. 
#' }
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' 
#' wdir <- paste0(tempdir(),"/Path_for_downloading_folder")
#' print(wdir)
#' wdir.mod <- file.path(wdir, "Modis","MOD11A1")
#' wdir.mod.hdf <- file.path(wdir.mod, "hdf")
#' sres <- modSearch(product = "MOD11A1",
#'                       startDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                       endDate = as.Date("01-01-2011", "%d-%m-%Y"),
#'                       collection = 6,
#'                       extent = ex.navarre,
#'                       AppRoot = wdir.mod.hdf)
#'                       
#' # download the images of the list
#' wdir.mod <- file.path(wdir, "Modis", "MOD11A1")
#' modDownload(searchres = sres, 
#'               username = "username", 
#'               password = "password",
#'               AppRoot = wdir.mod.hdf)
#' 
#' wdir.mod.tif<-file.path(wdir.mod,"tif")
#' 
#' # extract the first image
#' files.mod.hdf <- list.files(wdir.mod.hdf, 
#'                         full.names = TRUE, 
#'                         pattern = "\\.hdf$")
#' files.mod.hdf.1 <- files.mod.hdf[1]
#' modExtractHDF(filesHDF = files.mod.hdf.1,
#'               AppRoot = wdir.mod.tif)
#' }
modExtractHDF<-function(filesHDF,AppRoot,overwrite=FALSE,bFilter=NULL,rm.band=NULL,region=NULL,verbose=FALSE,...){
    arg<-list(...)
    if("dates"%in%names(arg)){filesHDF<-filesHDF[modGetDates(filesHDF)%in%arg$dates]}
    filesHDF<-pathWinLx(filesHDF)
    AppRoot<-pathWinLx(AppRoot)
    dir.create(AppRoot,showWarnings = verbose)
    for(fileHDF in filesHDF){
      image.name<-gsub(".hdf","",basename(fileHDF))
        dir.create(paste0(AppRoot,"/",image.name),recursive = TRUE,showWarnings = verbose)
        message(paste0("Extracting bands from hdf file of image ",image.name))
        image.data<-unlist(strsplit(gdal_utils(util = "info", 
                           source =fileHDF,quiet=TRUE),"\n"),
                           recursive =FALSE)
        bands.names<-image.data[grepl(".*SUBDATASET_.*_NAME=", image.data)]
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
              gdal_utils(util = "translate", 
                        source =gsub(".*SUBDATASET_.*_NAME=","",bands.names[i]),
                        destination = gsub(" ","_",paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],"_temp.tif"))
                        )
              gdal_utils(util = "warp", 
                         source =paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],"_temp.tif"),
                         destination = paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],".tif"),
                         options=c("-s_srs",arg$s_srs)
              )
              file.remove(paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],"_temp.tif"))
            }else{
              gdal_utils(util = "translate", 
                         source =gsub(".*SUBDATASET_.*_NAME=","",bands.names[i]),
                         destination = gsub(" ","_",paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],".tif")),
                         quiet=TRUE
              )
            }

          }else{
            if(verbose){
              warning("File exists! not extracting...")
            }
          }
          if(!is.null(region)){
            region<-transform_multiple_proj(region)
            ext=extent(region)
            gdal_utils(util = "warp", 
                       source =paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],".tif"),
                       destination = paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],"_cutted.tif"),
                       options=c("-te",ext@xmin,ext@ymin,ext@xmax,ext@ymax,"-te_srs",st_crs(region)$proj4string)
            )
          }
        }
      
    }

}
