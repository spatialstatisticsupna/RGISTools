#' Mosaics a set of MODIS images
#'
#' \code{modMosaic} merges the tiles of MODIS covering a region of interest returns a single image for each date
#'
#' The function mosaics the tiles of satellite images in the \code{src} folder.
#' The function uses the folder resulting from the \code{modeExtractHDF} function.
#' The folder may contain multiple tiles as tif files, for one or several dates
#' and one or several bands. When only one band has to be mosaicked, the name of
#' the band can be provided through the argument \code{bandFilter}. The name of the band
#' should be defined as a character string beginning with the letter b and a two-digit
#' band number (e.g. ‘b01’). Similarly, when only a subset of dates has to be mosaicked,
#' the date(s) should be provided through the argument \code{dayFilter}. The dates must be
#' provided as date objects. Once the images are mosaicked, they are cropped using
#' the extent defined by \code{extent} (optional). The extent can be defined in any
#' projection format. The function \code{modMosaic} automatically reprojects the extent
#' to match the projection of the image. The resulting images will be placed in
#' the \code{AppRoot} directory. The output files are named after region of interest
#' provided by the argument \code{out.name}. If no name is provided,
#' by default the output file is named as ‘outfile’
#'
#' @param src the path of the folder with the MODIS images in tif format
#' @param out.name the name of the region, if is not defined "outfile" will be asigned
#' @param extent spatial polygon object representing the region of interest
#' @param gutils a boolean flag to use GDAL utilities for mosaicking
#' @param overwrite a boolean flag to overwrite the existing merged images
#' @param showWarnings  a boolean flag to print warning messages from external functions
#' @param ... Argument for function nestering accepts:
#'  \itemize{
#'   \item \code{pathrow} a list with the path and row numbers for the region of interest
#'   \item \code{bandFilter} a vector with the name of the image bands to be mosaicked.
#' If it is not supplied, the function is applied to all the bands available in \code{src}
#'   \item \code{dayFilter} a vector containing the days in date format to filter the days wanted
#'   \item \code{AppRoot} the directory to save the mosaicked images
#' }
#' @examples
#' \dontrun{
#' #load a spatial polygon object of navarre for the example
#' data(navarre)
#' #asign the folder where the example will be run
#' src<-"Z:/Aplicaciones/Paquetes/TestEnvironment/Modis"
#' #download modis images
#' modDownload(product="MOD09GA",
#'             startDate=as.Date("01-01-2018","%d-%m-%Y"),
#'             endDate=as.Date("03-01-2018","%d-%m-%Y"),
#'             username="rgistools",
#'             password="EspacialUPNA88",
#'             AppRoot=src,
#'             hdfdir="hdf",
#'             tiffdir="tif",
#'             collection=6,
#'             extent=navarre)
#' #asign the folder with the sentinel images untared
#' src<-file.path(src,"MOD09GA")
#' tif.src<-file.path(src,"tif")
#' #mosaic the modis images
#' modMosaic(tif.src,
#'           AppRoot = src,
#'           out.name = "Navarre",
#'           gutils = T,
#'           overwrite = T,
#'           extent=navarre)
#' }
modMosaic<-function(src,
                    extent=NULL,
                    out.name="outfile",
                    showWarnings=T,
                    gutils=F,
                    overwrite=F,
                    ...){
  #src<-"Z:/ImagenesSatelite/MODIS/MOD09GA/tif"
  arg<-list(...)
  AppRoot<-defineAppRoot(...)

  #read all folder names to get all the days
  imgFolders<-list.files(src,full.names = T)
  #remove folders
  imgFolders<-imgFolders[nchar(basename(imgFolders))==41]

  dates<-unique(modGetDates(imgFolders))
  bpath<-file.path(AppRoot,out.name)

  #filter dates
  if("dayFilter"%in%names(arg)){
    dates<-dates[dates%in%arg$dayFilter]
  }

  for(d in 1:length(dates)){
    #filter the images to one day
    dayImg<-imgFolders[modGetDates(imgFolders)%in%dates[d]]
    if(length(dayImg)<1){
      if(showWarnings)
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

    flist<-list.files(dayImg,recursive=T,full.names=T,pattern="\\.tif$")
    #filter the images by data type
    if("bandFilter"%in%names(arg)){
      flist<-flist[Reduce("|", lapply(arg$bandFilter,grepl,flist))]
    }
    dtype<-flist
    for(x in basename(dayImg)){
      dtype<-gsub(x,"",basename(dtype))
    }
    dtype<-unique(dtype)


    if(gutils){
      print(paste0("Merging and constraining the extent of the image at ",dates[d]," using gdalUtils library"))
    }else{
      print(paste0("Merging and cutting for day ",dates[d]," using raster library"))
    }
    AppRoot<-file.path(bpath,format(dates[d],"%Y%j"))
    if(!file.exists(AppRoot)||overwrite){
      dir.create(AppRoot,recursive = T,showWarnings = showWarnings)
      for(dt in 1:length(dtype)){
        typechunks<-flist[grepl(dtype[dt],flist)]
        if(!gutils){
          #mosaic with native R libraries
          typechunks<-lapply(typechunks,raster)
          tryCatch(
            {
              img<- genMosaicList(typechunks)
            },
            error=function(cond) {
              message(paste0(cond,"\nProjecting to the same CRS..."))
              if(cond=="different CRS"){
                typechunks<-lapply(typechunks,projectRaster,to=typechunks[[1]])
                img<- genMosaicList(typechunks)
              }
            })
          if(!is.null(extent)){
            if(class(extent)!="Extent")
              extent<-spTransform(extent,crs(img))
            img<-crop(img,extent)
          }
          writeRaster(img,file.path(AppRoot,paste0(out.name,"_",format(dates[d],"%Y%j"),"_",dtype[dt])),overwrite=overwrite)
        }else{
          #mosaic with gdalutils no support cutline
          if(is.null(extent)){
            mosaic_rasters(typechunks,dst_dataset=file.path(AppRoot,paste0(out.name,"_",format(dates[d],"%Y%j"),dtype[dt])))
          }else{
            ext<-extent(extent)
            temp<-file.path(AppRoot,paste0(out.name,"_",format(dates[d],"%Y%j"),dtype[dt],"_temp.tif"))
            mosaic_rasters(typechunks,
                           dst_dataset=temp)
            gdalwarp(srcfile=temp,
                     dstfile=file.path(AppRoot,paste0(out.name,"_",format(dates[d],"%Y%j"),dtype[dt])),
                     te=c(ext@xmin,ext@ymin,ext@xmax,ext@ymax),
                     te_srs=proj4string(extent),
                     overwrite=overwrite)
            file.remove(temp)
          }
        }
      }
    }else{
      if(showWarnings){
        warning("File exists! not mergin...")
      }
    }
  }
  message(paste0("Region saved in HDD.\nFiles in: ",bpath))
}
