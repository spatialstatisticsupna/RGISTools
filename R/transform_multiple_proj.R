transform_multiple_proj <- function(obj, proj4){
  # Object to be transformed
  if(is(obj, "sf")) {
    return(st_transform(obj, proj4))
  }else if(is(obj, "Spatial")){
    return(st_transform(st_as_sf(obj), proj4))
  }else if(is(obj, "Raster")){ 
    new_obj <- extent(obj)
    new_obj<-st_as_sf(as(new_obj, 'SpatialPolygons') )
    st_crs(new_obj)<-projection(obj)
    return(st_transform(new_obj, proj4))
  }else{
    stop("Spatial object not supported!")
  }
}