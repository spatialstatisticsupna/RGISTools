transform_multiple_proj <- function(obj, proj4){
  # Object to be transformed
  if(is(obj, "sf")) {
    new_obj<-obj
  }else if(is(obj, "Spatial")){
    new_obj<-st_as_sf(obj)
  }else if(is(obj, "Raster")){ 
    new_obj <- extent(obj)
    new_obj<-st_as_sf(as(new_obj, 'SpatialPolygons'))
    st_crs(new_obj)<-projection(obj)
  }else{
    stop("Spatial object not supported!")
  }

  if(missing(proj4)){
    return(new_obj)
  }else{
    return(st_transform(new_obj, proj4))
  }
}


