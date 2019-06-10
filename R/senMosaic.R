#' Mosaics a set of Sentinel images
#'
#' \code{\link{senMosaic}} merges the tiles of Sentinel images covering a region of interest and
#'  returns a single image for each date
#'
#' The function mosaics the tiles of satellite images in the src folder.
#' The function uses the folder resulting from the \code{\link{senDownSearch}} function.
#' The folder may contain multiple tiles as tif files, for one or several dates
#' and one or several bands. When only one band has to be mosaicked, the name of
#' the band can be provided through the argument \code{bandFilter}. The name of the band
#' should be defined as a character string beginning with the letter b and a
#' two-digit band number (e.g. ‘b01’). Similarly, when only a subset of dates has
#' to be mosaicked, the date(s) should be provided through the argument \code{dayFilter}.
#' The dates must be provided as date objects. Once the images are mosaicked, they are
#' cropped using the extent defined by \code{extent} (optional). The extent can be defined
#' in any projection format. The function \code{\link{senMosaic}} automatically reprojects the
#' extent to match the projection of the image. The resulting images will be placed
#' in the \code{AppRoot} directory. The output files are named after region of interest
#' provided by the argument \code{out.name}. If no name is provided, by default the output
#' file is named as ‘outfile’
#'
#' @param src the path of the folder with the Sentinel images in tif format.
#' @param out.name a character string with the name of the region of interest.
#' @param extent  an \code{Extent} object representing the region of interest.
#' @param overwrite logical argument. If \code{TRUE} overwrites the existing images with the same name.
#' @param gutils logical argument. If \code{TRUE} the function uses GDAL utilities for mosaicking.
#' @param verbose logical argument. If \code{TRUE} the function prints running stages and warnings.
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
#' #load a spatial polygon object of navarre for the example
#' data(ex.navarre)
#' #asign the folder where the example will be run
#' src<-"Path_for_downloading_folder"
#' # download Sentinel images
#' senDownload(startDate=as.Date("2018210","%Y%j"),
#'             endDate=as.Date("2018218","%Y%j"),
#'             platform="Sentinel-2",
#'             intersects=ex.navarre,
#'             product="S2MSI1C",
#'             pathrow=c("R094"),
#'             username="username",
#'             password="password",
#'             AppRoot=src)
#' # asign the folder with the Sentinel images unzipped
#' src.unzip<-file.path(src,"unzip")
#' # mosaic the Sentinel images
#' senMosaic(src.unzip,
#'           AppRoot=src,
#'           gutils=T,
#'           out.name="Navarre")
#' }
senMosaic<-function(src,
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
  #imgFolders<-imgFolders[nchar(basename(imgFolders))==83]

  dates<-unique(senGetDates(imgFolders))
  bpath<-file.path(AppRoot,out.name)

  #filter dates
  if("dayFilter"%in%names(arg)){
    dates<-dates[dates%in%arg$dayFilter]
  }

  for(d in 1:length(dates)){
    #filter the images to one day
    dayImg<-imgFolders[senGetDates(imgFolders)%in%dates[d]]
    if(length(dayImg)<1){
      if(verbose)
        warning(paste0("No tiles for date ",dates[d]))
      next #breaks one iteration only
    }
    #filter the images by pathrow
    if("pathrow"%in%names(arg)){
      prstr<-c()
      for(pr in arg$pathrow){
        prstr<-c(prstr,paste0("h",sprintf("%02d",pr[1]),"v",sprintf("%02d",pr[2])))
      }
      dayImg<-dayImg[modGetPathRow(dayImg)%in%prstr]
      stopifnot(length(dayImg)>0)
    }

    flist<-list.files(dayImg,recursive=T,full.names=T,pattern="\\.jp2$")
    #filter the images by data type
    if("bandFilter"%in%names(arg)){
      flist<-flist[Reduce("|", lapply(arg$bandFilter,grepl,flist))]
    }
    dtype<-unique(gsub(".jp2","",gsub(".*\\s*_(\\d{8}T\\d{6})_", "", basename(flist))))

    if(gutils){
      print(paste0("Merging and constraining the extent of the image at ",dates[d]," using gdalUtils library"))
    }else{
      print(paste0("Merging and cutting for day ",dates[d]," using raster library"))
    }

    AppRoot<-file.path(bpath,format(dates[d],"%Y%j"))
    if(!file.exists(AppRoot)||overwrite){
      dir.create(AppRoot,recursive = T,showWarnings = verbose)
      for(dt in 1:length(dtype)){
        typechunks<-flist[grepl(dtype[dt],flist)]
        if(!gutils){
          #mosaic with native R libraries
          message(paste0("Merging band ",dtype[dt]))
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
          if(!is.null(extent)){
            if(class(extent)!="Extent")
              extent<-spTransform(extent,crs(img))
            img<-crop(img,extent)
          }
          writeRaster(img,file.path(AppRoot,paste0(out.name,"_",format(dates[d],"%Y%j"),"_",dtype[dt],".tif")),overwrite=overwrite)
        }else{
          #mosaic with gdalutils no support cutline
          message(paste0("Merging band ",dtype[dt]))
          if(is.null(extent)){
            mosaic_rasters(typechunks,
                           dst_dataset=file.path(AppRoot,paste0(out.name,"_",format(dates[d],"%Y%j"),"_",dtype[dt],".tif")),
                           srcnodata=0,
                           vrtnodata=0)
          }else{
            ext<-extent(extent)
            temp<-file.path(AppRoot,paste0(out.name,"_",format(dates[d],"%Y%j"),"_",dtype[dt],"_temp.tif"))
            mosaic_rasters(typechunks,
                           dst_dataset=temp,
                           srcnodata=0,
                           vrtnodata=0)
            gdalwarp(srcfile=temp,
                     dstfile=file.path(AppRoot,paste0(out.name,"_",format(dates[d],"%Y%j"),"_",dtype[dt],".tif")),
                     te=c(ext@xmin,ext@ymin,ext@xmax,ext@ymax),
                     te_srs=proj4string(extent),
                     overwrite=overwrite)
            file.remove(temp)
          }
        }
      }
    }else{
      if(verbose){
        warning("File exists! not mergin...")
      }
    }
  }
  message(paste0("Region saved in HDD.\nFiles in: ",bpath))
}
