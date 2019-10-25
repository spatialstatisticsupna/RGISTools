#' Mosaic a set of Landsat-7 or Landsat-8 images
#'
#' \code{lsMosaic} merges the Landsat-7 or Landsat-8 imagery that covers a 
#' region of interest on the same dates.
#'
#' The function mosaics the imagery in the \code{src} folder. The folder can
#' hold GTiff images from several tiles, dates and bands. When only a subset
#' of bands or dates has to be mosaicked, the band names or dates should be
#' provided through the argument \code{bFilter} or \code{dateFilter}. Band
#' names are defined by the letter “b” and the two-digit band number
#' (e.g., ‘b01’). The dates must be provided as a \code{Date} class object.
#' Once mosaicked, the images can be cropped to fit the extent (optional).
#' The extent can be defined in any coordinate reference system, since
#' \code{lsMosaic} automatically reprojects the extent to match the projection
#' of the image. The outputs are placed in the \code{AppRoot} directory, under
#' the folder named as \code{out.name}. If no name is provided, the folder is
#' named “outfile”. To use `\code{gutils = TRUE}', a proper installation of `GDAL'
#' and the `\code{gdalUtils}' library is required. This method is faster than
#' native `R' functions.
#'
#' @param src the path to the folder with the Landsat-7 or Landsa-8 images in
#'  GTiff format.
#' @param out.name the name of the folder that stores the outputs. By default,
#' “outfile” is assigned.
#' @param AppRoot the directory to save the mosaicked images.
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
#'   \item \code{pathrow} a list of vectors with the path and row numbers of
#'   the tiles concerning the region of interest.
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
#' src <- paste0(tempdir(),"/Path_for_downloading_folder")
#' print(src)
#' # download Landsat-8 images
#' lsDownSearch(satellite = "ls8",
#'              username = "username",
#'              password = "password",
#'              startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'              endDate = as.Date("20-01-2018", "%d-%m-%Y"),
#'              extent = ex.navarre,
#'              untar = TRUE,
#'              AppRoot = src)
#' # folder with the Landsat-8 untared images
#' ls8.src <- file.path(src, "Landsat8")
#' tif.src <- file.path(ls8.src,"untar")
#' # mosaic the Landsat-8 images
#' lsMosaic(src = tif.src,
#'          AppRoot = ls8.src,
#'          out.name = "Navarre")
#'
#' lsMosaic(src = tif.src,
#'          AppRoot = ls8.src,
#'          out.name = "Navarre",
#'          extent = ex.navarre,
#'          gutils = TRUE, # using gdalUtils
#'          overwrite = TRUE) # overwrite
#' }
lsMosaic<-function(src,
                   AppRoot,
                   region=NULL,
                   out.name="outfile",
                   verbose=FALSE,
                   gutils=TRUE,
                   overwrite=FALSE,
                   ...){
  arg<-list(...)
  src<-pathWinLx(src)
  AppRoot<-pathWinLx(AppRoot)
  #read all folder names to get all the days
  imgFolders<-list.files(src,full.names = TRUE)
  #remove folders
  #imgFolders<-imgFolders[nchar(basename(imgFolders))==21]
  if(length(imgFolders)==0)stop(paste0("There is no images in ",src," path."))
  dates<-unique(lsGetDates(imgFolders))
  bpath<-file.path(AppRoot,out.name)

  #filter dates
  if("dates"%in%names(arg)){
    dates<-dates[dates%in%arg$dates]
  }
  #definition of bands names
  if(any(grepl("LE",imgFolders))){
    message("Landsat-7 images detected!")
    dtype<-paste0(getRGISToolsOpt("LS7BANDS"),".tif")
    qcband<-getRGISToolsOpt("LS7BANDS")["quality"]
  }else if(any(grepl("LC",imgFolders))){
    message("Landsat-8 images detected!")
    dtype<-paste0(getRGISToolsOpt("LS8BANDS"),".tif")
    qcband<-getRGISToolsOpt("LS8BANDS")["quality"]
  }else{
    stop("Satellite not supported for Day mosaicing.")
  }
  
  #manage level 2 bands
  if(nchar(basename(imgFolders[1]))!=21){
    message("Level-2 images detected!")
    dtype<-gsub("B","band",dtype)
    dtype<-c(dtype[-which(dtype%in%"bandQA.tif")],"pixel_qa.tif","radsat_qa.tif","sr_aerosol.tif")
    qcband<-c("pixel_qa.tif","radsat_qa.tif","sr_aerosol.tif")
    lvl2=TRUE
  }else{
    lvl2=FALSE
  }
  

  for(d in 1:length(dates)){
    #filter the images to one day
    dayImg<-imgFolders[lsGetDates(imgFolders)%in%dates[d]]
    if(length(dayImg)<1){
      if(verbose)
        warning(paste0("No tiles for date ",dates[d]))
      next #breaks one iteration only
    }
    #filter the images by pathrow
    if("pathrow"%in%names(arg)){
      prstr<-c()
      for(pr in arg$pathrow){
        prstr<-c(prstr,paste0(sprintf("%03d",pr[1]),sprintf("%03d",pr[2])))
      }
      dayImg<-dayImg[lsGetPathRow(dayImg)%in%prstr]
      stopifnot(length(dayImg)>0)
    }

    flist<-list.files(dayImg,recursive=TRUE,
                      full.names=TRUE,
                      pattern="\\.tif$",
                      ignore.case = TRUE)
    #filter the images by data type
    if("bFilter"%in%names(arg)){
      flist<-flist[Reduce("|", lapply(arg$bFilter,grepl,flist))]
      dtype<-dtype[Reduce("|", lapply(arg$bFilter,grepl,dtype))]
    }

    if(gutils){
      message(paste0("Merging and constraining the extent of the image at ",dates[d]," using gdalUtils library"))
    }else{
      message(paste0("Merging and cutting for day ",dates[d]," using raster library"))
    }
    AppRoot<-file.path(bpath,format(dates[d],"%Y%j"))
    dir.create(AppRoot,recursive = TRUE,showWarnings = verbose)
    for(dt in 1:length(dtype)){
      if(lvl2){
        bname<-gsub("band","B",dtype[dt])
      }else{
        bname<-dtype[dt]
      }
      out.file.path<-file.path(AppRoot,paste0(out.name,"_",format(dates[d],"%Y%j"),"_",bname))
      if(verbose){message(paste0("Out file: ",out.file.path))}
      
      if((!file.exists(out.file.path))|overwrite){
        typechunks<-flist[grepl(dtype[dt], flist, ignore.case = TRUE)]
        if(length(typechunks)==0)next
        if(!gutils){
          #mosaic with native R libraries
          typechunks<-lapply(typechunks,raster)
          typechunks<-lapply(typechunks,readAll)
          tryCatch(
            {
              if(verbose){message("Mosaicking images...")}
              img <- genMosaicList(typechunks,verbose)
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
            region<-transform_multiple_proj(region)
            #TODO remove as spatial using raster v3 package
            c_region<-as(region, 'Spatial')
            img<-crop(img,c_region)
            if("cutline"%in%names(arg)){
              img<-mask(img,c_region)
            }
          }
          writeRaster(img,out.file.path,overwrite=overwrite)
        }else{
          #mosaic with gdalutils no supporting cutline
          if(any(grepl(qcband,dtype[dt]))){
            nodata<-1
          }else if(lvl2){
            nodata<--9999
          }else{
           nodata<-0
          }
          
          if(verbose){
            message(paste0("Nodata to ",nodata))
            message(paste0("Chunks ",typechunks))
          }
          
          if(is.null(region)){
            temp<-gsub(".tif","_temp.vrt",out.file.path,fixed = TRUE)
            genMosaicGdalUtils(typechunks=typechunks,
                               temp=temp,
                               nodata=nodata,
                               out.name=out.file.path)
          }else{
            ext<-extent(region)
            temp<-file.path(AppRoot,paste0(out.name,"_",format(dates[d],"%Y%j"),"_",gsub(".tif","",dtype[dt],fixed = TRUE),"_temp.vrt"))
            
            out.tmp<-gsub(".tif","_tmp.tif",out.file.path,fixed = TRUE)
            genMosaicGdalUtils(typechunks=typechunks,
                               temp=temp,
                               nodata=nodata,
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
