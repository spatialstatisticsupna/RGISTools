#' Mosaic a set of MODIS images
#'
#' \code{modMosaic} merges the MODIS imagery that covers a 
#' region of interest on the same dates.
#'
#' The function mosaics the imagery in the \code{src} folder. The folder can
#' hold GTiff images from several tiles, dates and bands. When only a subset
#' of bands or dates has to be mosaicked, the band names or dates should be
#' provided through the argument \code{bFilter} or \code{dates}. Band
#' names are defined by the letter “b” and the two-digit band number (e.g., 
#' ‘b01’). The dates must be provided as a \code{Date} class object. Once
#' mosaicked, the images can be cropped to fit the \code{extent} (optional).
#' The \code{extent} can be defined in any coordinate reference system, since
#' \code{modMosaic} automatically reproject the extent to match the projection
#' of the image. The outputs will be placed in the \code{AppRoot} directory,
#' under the folder named as \code{out.name}. If no name is provided, the 
#' folder is named “outfile”. To use \code{gutils = TRUE}, a proper installation
#' of `GDAL' is required. This method is faster than the native `R' functions.
#'
#'
#' @param src the path of the folder with the MODIS images in GTiff format.
#' @param AppRoot the directory where the mosaicked images are saved.
#' @param out.name  the name of the folder that stores the outputs. By default,
#' “outfile” is assigned.
#' @param region a \code{Spatial*}, projected \code{raster*}, or \code{sf*} class object 
#' defining the area of interest.
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing 
#' images with the same name.
#' @param gutils logical argument. If \code{TRUE}, the function uses `GDAL' 
#' utilities for mosaicking.
#' @param verbose logical argument. If \code{TRUE}, the function prints the 
#' running steps and warnings.
#' @param ... arguments for nested functions:
#'  \itemize{
#'   \item \code{pathrow} a \code{vector} of \code{character} with the path and row numbers
#'   of the tiles concerning the region of interest in 'hXXvYY' format.
#'   \item \code{bFilter} a vector with the bands to be mosaicked. If not
#'   supplied, all bands are mosaicked.
#'   \item \code{dates} a vector with the capturing dates being considered
#'   for mosaicking. If not supplied, all dates are mosaicked.
#' }
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # main output directory
#' wdir <- file.path(tempdir(),"Path_for_downloading_folder")
#' print(wdir)
#' # download MODIS images
#' modDownSearch(product = "MOD09GA",
#'               startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'               endDate = as.Date("03-01-2018", "%d-%m-%Y"),
#'               username = "username",
#'               password = "password",
#'               AppRoot = wdir,
#'               extract.tif = TRUE,
#'               collection = 6,
#'               extent = ex.navarre)
#' # folder with the MODIS images extracted 
#' wdir.mod <- file.path(wdir, "Modis", "MOD09GA")
#' wdir.mod.tif <- file.path(wdir.mod, "tif")
#' # mosaic the MODIS images
#' modMosaic(wdir.mod.tif,
#'           AppRoot = wdir.mod,
#'           out.name = "Navarre",
#'           gutils = TRUE,
#'           overwrite = TRUE,
#'           region = ex.navarre)
#' }
modMosaic<-function(src,
                    AppRoot,
                    region = NULL,
                    out.name = "outfile",
                    verbose = FALSE,
                    gutils = TRUE,
                    overwrite = FALSE,
                    ...){
  arg<-list(...)
  src<-pathWinLx(src)
  AppRoot<-pathWinLx(AppRoot)
  #read all folder names to get all the days
  imgFolders<-list.files(src,full.names = TRUE)
  #remove folders
  #imgFolders<-imgFolders[nchar(basename(imgFolders))==41]

  dates<-unique(modGetDates(imgFolders))
  bpath<-file.path(AppRoot,out.name)

  #filter dates
  if("dates"%in%names(arg)){
    dates<-dates[dates%in%arg$dates]
  }
  if(length(imgFolders)==0)stop(paste0("No images found in ",src," path."))
  for(d in 1:length(dates)){
    #filter the images to one day
    dayImg<-imgFolders[modGetDates(imgFolders)%in%dates[d]]
    if(length(dayImg)<1){
      if(verbose)
        warning(paste0("No tiles for date ",dates[d]))
      next #breaks one iteration only
    }
    #filter the images by pathrow
    if("pathrow"%in%names(arg)){
      # prstr<-c()
      # for(pr in arg$pathrow){
      #   prstr<-c(prstr,paste0("h",sprintf("%02d",pr[1]),"v",sprintf("%02d",pr[2])))
      # }
      dayImg<-dayImg[modGetPathRow(dayImg)%in%arg$pathrow]
      if(length(dayImg)==0){
        warning("There is no tiles after the pathrow filtering.")
        next
      }
    }

    flist<-list.files(dayImg,recursive=TRUE,full.names=TRUE,pattern="\\.tif$")
    #filter the images by data type
    if("bFilter"%in%names(arg)){
      flist<-flist[Reduce("|", lapply(arg$bFilter,grepl,flist))]
    }
    dtype<-flist
    for(x in basename(dayImg)){
      dtype<-gsub(x,"",basename(dtype))
    }
    dtype<-unique(dtype)


    if(gutils){
      message(paste0("Merging and constraining the extent of the image at ",dates[d]," using gdalUtils library"))
    }else{
      message(paste0("Merging and cutting for day ",dates[d]," using raster library"))
    }
    AppRoot<-file.path(bpath,format(dates[d],"%Y%j"))
    dir.create(AppRoot,recursive = TRUE,showWarnings = verbose)
    for(dt in 1:length(dtype)){
      out.file.path<-file.path(AppRoot,paste0(out.name,"_",format(dates[d],"%Y%j"),dtype[dt]))
      if((!file.exists(out.file.path))|overwrite){
        typechunks<-flist[grepl(dtype[dt],flist)]
        if(length(typechunks)==0)next
        if(!gutils){
          #mosaic with native R libraries
          typechunks<-lapply(typechunks,raster)
          tryCatch(
            {
              img<- genMosaicList(typechunks,verbose)
            },
            error=function(cond) {
              if(any(grepl("different CRS",cond))){
                message(paste0("Different CRS when mosaicing tiles!\nProjecting to the same CRS..."))
                typechunks<-lapply(typechunks,projectRaster,to=typechunks[[1]])
                img<- genMosaicList(typechunks,verbose)
              }else if(any(grepl("subscript out of bounds",cond))){
                warning("Tile for ",dt," not found! Check ",d," date image forders for extrating errors.")
              }else{
                stop(cond)
              }
            })
          if(!is.null(region)){
            region <- transform_multiple_proj(region, gdal_crs(typechunks[1])$crs[["proj4string"]])
            #TODO remove as spatial using raster v3 package 
            c_region<-as(region, 'Spatial')
            img<-crop(img,c_region)
            if("cutline"%in%names(arg)){
              img<-mask(img,c_region)
            }
          }
          writeRaster(img,out.file.path,overwrite=overwrite)
        }else{
          if(is.null(region)){
            temp<-gsub(".tif","_temp.vrt",out.file.path,fixed = TRUE)
            genMosaicGdalUtils(typechunks=typechunks,
                               temp=temp,
                               nodata=NULL,
                               out.name=out.file.path)
          }else{
            region <- transform_multiple_proj(region, gdal_crs(typechunks[1])$crs[["proj4string"]])
            ext<-extent(region)
            temp<-gsub(".tif","_temp.vrt",out.file.path,fixed = TRUE)
            out.tmp<-gsub(".vrt",".tif",temp,fixed = TRUE)
            genMosaicGdalUtils(typechunks=typechunks,
                               temp=temp,
                               nodata=NULL,
                               out.name=out.tmp)
            gdal_utils(util = "warp", 
                       source =out.tmp,
                       destination = out.file.path,
                       options=c("-te",ext@xmin,ext@ymin,ext@xmax,ext@ymax,"-te_srs",st_crs(region)$proj4string)
            )
            suppressWarnings(file.remove(out.tmp, showWarnings = FALSE))
          }
        }
      }else{
        if(verbose){
          warning("File exists! not mergin...")
        }
      }
    }
      
  }
  message(paste0("Region saved in HDD.\nFiles in: ",bpath))
}
