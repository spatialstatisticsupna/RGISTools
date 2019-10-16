genMosaicGdalUtils<-function(typechunks,temp="temp.vrt",nodata,out.name){
  newchunks<-NULL
  
  tryCatch({
    if(is.null(nodata)){
      gdal_utils(util = "buildvrt", 
                 source =typechunks,
                 destination = temp
      )
    }else{
      gdal_utils(util = "buildvrt", 
                 source = typechunks,
                 destination = temp,
                 options=c("-srcnodata",nodata,"-vrtnodata",nodata)
      )
    }
  }, warning = function(warning_condition) {
    if(grepl("gdalbuildvrt does not support heterogeneous projection",warning_condition)){
      #reproject the images
      diffproj=TRUE
      suppressWarnings(file.remove(temp))
      proj<-paste0("EPSG:",gdal_crs(typechunks[1])$crs[[1]])
      newchunks<-c(typechunks[1])
      for(ni in 2:length(typechunks)){
        destemp<-gsub(".TIF","_proj.TIF",typechunks[ni],ignore.case =T,fixed = TRUE)
        destemp<-gsub(".jp2","_warp.tif",typechunks[ni],ignore.case =T,fixed = TRUE)
        gdal_utils(util = "warp", 
                   source =typechunks[ni],
                   destination = destemp,
                   options=c("-t_srs",proj)
        )
        newchunks<-c(newchunks,destemp)
      }
      if(is.null(nodata)){
        gdal_utils(util = "buildvrt", 
                   source =newchunks,
                   destination = temp
        )
      }else{
        gdal_utils(util = "buildvrt", 
                   source =newchunks,
                   destination = temp,
                   options=c("-srcnodata",nodata,"-vrtnodata",nodata)
        )
      }
    }
  })
  gdal_utils(util = "translate", 
             source =temp,
             destination = out.name,
             options=c(paste0("of GTiff"))
  )
  
  suppressWarnings(file.remove(temp,showWarnings = FALSE))
  if(!is.null(newchunks)){
    suppressWarnings(file.remove(newchunks[-1],showWarnings = FALSE))
  }
}

