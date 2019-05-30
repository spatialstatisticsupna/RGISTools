#' Converts an .hdf file into a set of .tif files
#'
#' \code{modExtractHDF} converts the original compressed from MODIS (.df) into a file
#' format loadable by R (.tif). The function extracts all image layers and crops the
#' area of interest (if needed)
#'
#' HDF files cannot be directly loaded into R. The function modExtractHDF
#' borrows gdalwarp and gdal_translate functions from the gdalUtils package.
#' These functions are used to convert the .hdf files into .tif files. The .tif
#' files can be loaded in R using the raster package. Go to \code{\link{modSearch}} and \code{\link{modDownload}}
#' for further details about these functions. Further details about the \code{gdalUtils} and \code{gdalUtils} packages
#'
#' @param filesHDF  the full path of the .hdf files to be converted
#' @param shp  the shape file of the area of interest
#' @param overwrite Flag for overwrite existing images
#' @param bFilter a vector containing the names of the bands to extract
#' @param vebose  a boolean flag to print warning messages from external functions
#' @param ... argument to allow function nestering
#' \itemize{
#'   \item \code{AppRoot} the directory where the extracted images should be located
#' }
#' @examples
#' \dontrun{
#' data(ex.navarre)
#' img.list<-modSearch(product="MOD11A1",
#'                     startDate=as.Date("01-01-2011","%d-%m-%Y"),
#'                     endDate=as.Date("01-01-2011","%d-%m-%Y"),
#'                     collection=6,
#'                     extent=ex.navarre)
#' #download first image of image list
#' modDownSearch(img.list,"username","password")
#' #Extract one layer from downloaded image
#' hdf.files <- list.files("./",full.names = T,pattern="\\.hdf$")
#' first.hdf.file <-hdf.files[1]
#' modExtractHDF(first.hdf.file)
#' }
modExtractHDF<-function(filesHDF,overwrite=FALSE,shp=NULL,vebose=F,bFilter=NULL,...){
    arg<-list(...)
    AppRoot<-defineAppRoot(...)
    dir.create(AppRoot,showWarnings = vebose)
    for(fileHDF in filesHDF){
      image.name<-gsub(".hdf","",basename(fileHDF))
      if(!file.exists(paste0(AppRoot,"/",image.name))||overwrite){
        dir.create(paste0(AppRoot,"/",image.name),recursive = T,showWarnings = vebose)
        print(paste0("Extracting bands from hdf file of image ",image.name))
        image.data<-gdalinfo(fileHDF)
        bands.names<-image.data[grepl(".*SUBDATASET_.*_NAME.*", image.data)]
        names<-gsub('.*":','',bands.names)
        names<-gsub(':','_',names)
        if(!is.null(bFilter)){
          exname<-names[Reduce("|", lapply(bFilter,grepl,names))]
          bds<-which(names%in%exname)
        }else{
          bds<-1:length(names)
        }

        for(i in bds){
          if(!file.exists(paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],".tif"))){
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

          }
          if(!is.null(shp)){
            gdalwarp(srcfile=paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],".tif"),
                     dstfile=paste0(AppRoot,"/",image.name,"/",image.name,"_",names[[i]],"_cutted.tif"),
                     cutline=shp,
                     crop_to_cutline=TRUE,
                     overwrite=arg$overwrite)
          }
        }
      }else{
        if(vebose){
          warning("File exists! not extracting...")
        }
      }
    }

}
