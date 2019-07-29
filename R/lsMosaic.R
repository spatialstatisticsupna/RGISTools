#' Mosaic a set of Landsat images
#'
#' \code{lsMosaic} merges the tiles of Landsat images covering a region of interest and returns a single image for each date
#'
#' The function mosaics the tiles of satellite images in the \code{src} folder.
#' The function uses the folder resulting from the \code{lsDownload} function.
#' The folder may contain multiple tiles as tif files, for one or several
#' dates and one or several bands. When only one band has to be mosaicked,
#' the name of the band can be provided through the argument \code{bandFilter}. The
#' name of the band should be defined as a character string beginning with the
#' letter b and a two-digit band number (e.g. ‘b01’). Similarly, when only a
#' subset of dates has to be mosaicked, the date(s) should be provided through
#' the argument \code{dayFilter}. The dates must be provided as date objects. Once the
#' images are mosaicked, they are cropped using the extent defined by
#' \code{extent} (optional). The extent can be defined in any projection
#' format. The function \code{lsMosaic} automatically reprojects the extent to
#' match the projection of the image. The resulting images will be placed
#' in the \code{AppRoot} directory. The output files are named after region of
#' interest provided by the argument \code{out.name}. If no name is provided, by
#' default the output file is named as ‘outfile’.
#'
#' @param src the path of the folder with the Landsat images in tif format.
#' @param out.name the name of the region, otherwise "outfile" is assigned.
#' @param extent \code{Extent}, \code{raster}, \code{RasterStack}, \code{RasterBrick}, 
#' \code{SpatialPolygon} or \code{SpatialPolygonDataFrame} object representing the region of interest.
#' 
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing images with the same name.
#' @param gutils logical argument. If \code{TRUE}, the function uses GDAL utilities for mosaicking. The mosacking 
#' process is faster using \code{gutils} but requires the proper install of \code{gdalUtils} library.
#' @param verbose logical argument. If \code{TRUE}, the function prints running stages and warnings.
#' @param ... argument for function nestering accepts:
#'  \itemize{
#'   \item \code{pathrow} a list with the path and row numbers for the region of interest.
#'   \item \code{bandFilter} a vector with the name of the image bands to be mosaicked.
#' If it is not supplied, the function is applied to all the bands available in \code{src}.
#'   \item \code{dayFilter} a vector containing the days in date format to filter the days wanted.
#'   \item \code{AppRoot} the directory to save the mosaicked images.
#' }
#' @examples
#' \dontrun{
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#' # assign the main output directory
#' src <- "Path_for_downloading_folder"
#' # download Landsat-8 images
#' lsDownload(satellite = "ls8",
#'            username = "username",
#'            password = "password",
#'            startDate = as.Date("01-01-2018", "%d-%m-%Y"),
#'            endDate = as.Date("20-01-2018", "%d-%m-%Y"),
#'            extent = ex.navarre,
#'            untar = TRUE,
#'            AppRoot = src)
#' # assign the folder with the Landsat-8 untared images
#' tif.src <- file.path(src, "untar")
#' # mosaic the Landsat-8 images
#' lsMosaic(tif.src,
#'          AppRoot = src,
#'          out.name = "Navarre")
#'
#' lsMosaic(tif.src,
#'          AppRoot = src,
#'          out.name = "Navarre",
#'          extent = ex.navarre,
#'          gutils = TRUE, # using gdalUtils
#'          overwrite = TRUE) # overwrite
#' }
lsMosaic<-function(src,
                   extent=NULL,
                   out.name="outfile",
                   verbose=FALSE,
                   gutils=FALSE,
                   overwrite=FALSE,
                   ...){
  arg<-list(...)
  AppRoot<-defineAppRoot(...)

  #read all folder names to get all the days
  imgFolders<-list.files(src,full.names = T)
  #remove folders
  #imgFolders<-imgFolders[nchar(basename(imgFolders))==21]

  dates<-unique(lsGetDates(imgFolders))
  bpath<-file.path(AppRoot,out.name)

  #filter dates
  if("dayFilter"%in%names(arg)){
    dates<-dates[dates%in%arg$dayFilter]
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

    flist<-list.files(dayImg,recursive=T,
                      full.names=T,
                      pattern="\\.tif$",
                      ignore.case = T)
    #filter the images by data type
    if("bandFilter"%in%names(arg)){
      arg$bandFilter<-paste0(arg$bandFilter,"\\.tif")
      flist<-flist[Reduce("|", lapply(arg$bandFilter,grepl,flist))]
      dtype<-dtype[Reduce("|", lapply(arg$bandFilter,grepl,dtype))]
    }

    if(gutils){
      print(paste0("Merging and constraining the extent of the image at ",dates[d]," using gdalUtils library"))
    }else{
      print(paste0("Merging and cutting for day ",dates[d]," using raster library"))
    }
    AppRoot<-file.path(bpath,format(dates[d],"%Y%j"))
    dir.create(AppRoot,recursive = T,showWarnings = verbose)
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
          
          if(!is.null(extent)){
            extent<-spTransform(extent,crs(img))
            img<-crop(img,extent)
            if("cutline"%in%names(arg)){
              img<-mask(img,extent)
            }
          }
          writeRaster(img,out.file.path,overwrite=overwrite)
        }else{
          #mosaic with gdalutils no supporting cutline
          if(any(grepl(dtype[dt],qcband))){
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
          
          if(is.null(extent)){
            mosaic_rasters(typechunks,
                           dst_dataset=out.file.path,
                           srcnodata=nodata,
                           vrtnodata=nodata,
                           overwrite=overwrite,
                           verbose = verbose,
                           allow_projection_difference=T)
          }else{
            ext<-extent(extent)
            temp<-file.path(AppRoot,paste0(out.name,"_",format(dates[d],"%Y%j"),"_",gsub(".tif","",dtype[dt]),"_temp.tif"))
            mosaic_rasters(typechunks,
                           dst_dataset=temp,
                           srcnodata=nodata,
                           vrtnodata=nodata,
                           overwrite=TRUE,
                           verbose = verbose,
                           allow_projection_difference=T)
            gdalwarp(srcfile=temp,
                     dstfile=out.file.path,
                     te=c(ext@xmin,ext@ymin,ext@xmax,ext@ymax),
                     te_srs=proj4string(extent),
                     overwrite=overwrite,
                     verbose = verbose)
            file.remove(temp)
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
