#' Plot satellite images with a GIS format
#'
#' This function displays satellite images with the usual format in geographic
#' information systems (GIS), i.e., adding a scale, north arrow, and the border
#' of the region of interest (optional).
#'
#' This is a wrapper function of \code{spplot} and hence displays any 
#' \code{Raster*} object and accepts all of its parameters. The function adds a
#' scale, a north arrow and a polygon in the area of interest. If necessary, the
#' function automatically reprojects the polygon to match the projection of the
#' \code{raster}. The projection of the map can be changed by modifying the 
#' \code{proj} argument. The position in the graph of the arrow and scale bar
#' is measured in relative distances (0-1) to the lower left corner of the
#' graph. Their positions can be modified with the argument \code{compOpt}.
#'
#' @param r a \code{Raster*} class object with the image or image stack to be plotted.
#' @param region a \code{Spatial*}, projected \code{raster*}, or \code{sf*} class object 
#' defining the area of interest.
#' @param cex A numeric multiplier to control character sizes for axis labels.
#' @param ... argument for nested functions:
#' \itemize{
#'   \item \code{compOpt} list to fit the size and the location of the GIS 
#'   components as the arrow and the scales.
#'   \item \code{proj} a \code{CRS} class object defining the coordinate 
#'   reference system of the plot.
#'   \item \code{...} any argument accepted by the \code{spplot} function.
#' }
#'
#' @return \code{tmap} class containing the plot.
#'
#' @examples
#' genPlotGIS(ex.ndvi.navarre,
#'            ex.navarre)
#' 
#' genPlotGIS(r=ex.ndvi.navarre,
#'            region=ex.navarre,
#'            tm.compass.size=1,
#'            tm.compass.type="rose",
#'            tm.scale.bar.text.size=0.4,
#'            tm.polygon.region.lwd=6,
#'            tm.polygon.region.border.col="#000000",
#'            tm.raster.r.palette=rev(terrain.colors(40)),
#'            tm.raster.r.title="NDVI",
#'            tm.layout.legend.stack="horizontal")
#' 
#' tmap_mode("view")
#' genPlotGIS(ex.ndvi.navarre[[1]],
#'            ex.navarre,
#'            tm.raster.r.palette=rev(terrain.colors(40)))
#' 
#' tmap_mode(mode = c("plot"))
#' genPlotGIS(ex.ndvi.navarre,
#'            ex.navarre,
#'            tm.raster.r.palette=rev(terrain.colors(40)))
genPlotGIS<-function(r,region,breaks,labels,zlim,layout,nbreaks=40,nlabels=10,as.grid=FALSE,compass.rm=FALSE,scale.bar.rm=FALSE,...){
  args<-list(...)
  
  
  # layout preconfigured arguments
  tm_layout_args<-args[names(args)%in%names(formals(tm_layout))]
  if(!("legend.bg.color" %in% names(tm_layout_args))){
    tm_layout_args$legend.bg.color="white"
  }
  if(!("panel.show" %in% names(tm_layout_args))){
    tm_layout_args$panel.show=TRUE
  }
  if(!("panel.labels" %in% names(tm_layout_args))){
    tm_layout_args$panel.labels=names(r)
  }
  if(!("legend.outside" %in% names(tm_layout_args))){
    tm_layout_args$legend.outside=TRUE
  }
  if(!("legend.outside.size" %in% names(tm_layout_args))){
    tm_layout_args$legend.outside.size=0.08
  }
  if(!("legend.outside.position" %in% names(tm_layout_args))){
    tm_layout_args$legend.outside.position="right"
  }
  if(!("frame" %in% names(tm_layout_args))){
    tm_layout_args$frame=TRUE
  }
  
  if(!missing(layout)){
    lyt<-tm_facets(ncol=layout[2],nrow = layout[1])
  }else{
    lyt<-NULL
  }
  
  if(as.grid){
    tm_layout_args$between.margin=-.1
    grid<-tm_graticules(lines=FALSE,labels.space.x=.10,labels.space.y=.10)
  }else{
    grid<-tm_graticules(lines=FALSE)
  }
  

  #compass arguments and preconfigured assignation
  if(!compass.rm){
    compass_args<-names(formals(tm_compass))
    names(compass_args)<-paste0("tm.compass.",compass_args)
    tm_compass_args<-args[names(args)%in%names(compass_args)]
    names(tm_compass_args)<-compass_args[names(tm_compass_args)]
    if(!("type" %in% names(tm_compass_args))){
      tm_compass_args$type="arrow"
    }
    if(!("position" %in% names(tm_compass_args))){
      tm_compass_args$position=c("right", "top")
    }
    if(!("size" %in% names(tm_compass_args))){
      tm_compass_args$size=2
    }
    compass<-do.call(tm_compass,tm_compass_args)
  }else{
    compass<-NULL
  }
  
  
  #scale bar arguments and preconfigured assignation
  if(!scale.bar.rm){
    scale_bar_args<-names(formals(tm_scale_bar))
    names(scale_bar_args)<-paste0("tm.scale.bar.",scale_bar_args)
    tm_scale_bar_args<-args[names(args)%in%names(scale_bar_args)]
    names(tm_scale_bar_args)<-scale_bar_args[names(tm_scale_bar_args)]
    if(!("position" %in% names(tm_scale_bar_args))){
      tm_scale_bar_args$position=c("left", "bottom")
    }
    if(!(any(c("text.size","size") %in% names(tm_scale_bar_args)))){
      tm_scale_bar_args$text.size=0.8
    }
    scale.bar<-do.call(tm_scale_bar,tm_scale_bar_args)
  }else{
    scale.bar<-NULL
  }
  
  if(!missing(region)){
    # region default arguments
    shape_region_args<-names(formals(tm_shape))
    shape_region_args<-shape_region_args[!(shape_region_args%in%"...")]
    names(shape_region_args)<-paste0("tm.shape.region.",shape_region_args)
    tm_shape_region_args<-args[names(args)%in%names(shape_region_args)]
    names(tm_shape_region_args)<-shape_region_args[names(tm_shape_region_args)]
    tm_shape_region_args$shp=region
    
    
    polygon_region_args<-c(names(formals(tm_polygons)),names(formals(tm_fill)),names(formals(tm_borders)))
    polygon_region_args<-unique(polygon_region_args[!(polygon_region_args%in%"...")])
    names(polygon_region_args)<-paste0("tm.polygon.region.",polygon_region_args)
    tm_polygon_region_args<-args[names(args)%in%names(polygon_region_args)]
    names(tm_polygon_region_args)<-polygon_region_args[names(tm_polygon_region_args)]
    if(!("alpha" %in% names(tm_polygon_region_args))){
      tm_polygon_region_args$alpha=0
    }
    if(!("lwd" %in% names(tm_polygon_region_args))){
      tm_polygon_region_args$lwd=1
    }

    reg<-do.call(tm_shape,tm_shape_region_args) + do.call(tm_polygons,tm_polygon_region_args)
    
  }else{
    reg<-NULL
  }
  
  # default label and breaks for the raster
  if(missing(zlim)){
    lower<-min(minValue(r))
    upper<-max(maxValue(r))
  }else{
    if((class(zlim)!="numeric")&(length(zlim)!=0))
      stop("zlim must be a vector of length 2 specifying the upper and lower boundaries of the legend.")
    lower<-min(zlim)
    upper<-max(zlim)
  }

  nbreaks=nbreaks-2
  if(missing(breaks))
    breaks<-c(-Inf,seq(from=lower,to=upper,by=((upper-lower)/nbreaks)),Inf)
  if(missing(labels)){
    labels<-c("",as.character(round(breaks[-c(1,length(breaks))],digits = 2)))
    if(length(labels)>nlabels){
      labels<-rep("",length(labels))
      labels[c(seq(1,length(labels),as.integer(length(labels)/nlabels)),length(labels))]<-as.character(round(seq(from=lower,to=upper,by=((upper-lower)/nlabels)),digits = 2))
    }
  }
  
  # raster default arguments
  shape_r_args<-names(formals(tm_shape))
  shape_r_args<-shape_r_args[!(shape_r_args%in%c("..."))]
  names(shape_r_args)<-paste0("tm.shape.r.",shape_r_args)
  tm_shape_r_args<-args[names(args)%in%names(shape_r_args)]
  names(tm_shape_r_args)<-shape_r_args[names(tm_shape_r_args)]
  tm_shape_r_args$shp=r
  
  raster_r_args<-names(formals(tm_raster))
  names(raster_r_args)<-paste0("tm.raster.r.",raster_r_args)
  tm_raster_r_args<-args[names(args)%in%names(raster_r_args)]
  names(tm_raster_r_args)<-raster_r_args[names(tm_raster_r_args)]
  if(!("col" %in% names(tm_raster_r_args))){
    tm_raster_r_args$col=names(r)
  }
  if(!("breaks" %in% names(tm_raster_r_args))){
    tm_raster_r_args$breaks=breaks
  }
  if(!("labels" %in% names(tm_raster_r_args))){
    tm_raster_r_args$labels=labels
  }
  if(!("legend.reverse" %in% names(tm_raster_r_args))){
    tm_raster_r_args$legend.reverse=TRUE
  }
  if(!("title" %in% names(tm_raster_r_args))){
    tm_raster_r_args$title=""
  }
  
  # Base tmap
  do.call(tm_shape,tm_shape_r_args) + do.call(tm_raster,tm_raster_r_args) +# tm_facets(nrow=3,ncol=2)+# raster conf
    do.call(tm_layout,tm_layout_args) +# layout
    compass + #the compass
    reg+ #region
    scale.bar+#scale
    grid+
    lyt
    #tm_facets(inside.original.bbox=F,free.coords=F)
    #tm_graticules(lines=FALSE,labels.inside.frame=F) #coordinates
  
}





