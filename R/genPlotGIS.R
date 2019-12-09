#' Plot satellite images with a GIS format
#'
#' This function displays satellite images with the usual format in geographic
#' information systems (GIS), i.e., adding a scale, north arrow, and the border
#' of the region of interest (optional).
#'
#' This is a wrapper function of \code{tmap} and hence displays any 
#' \code{Raster*} object and accepts all of its parameters. The function adds a
#' scale, a north arrow and a polygon in the area of interest. If necessary, the
#' function automatically reprojects the polygon to match the projection of the
#' \code{raster}. The projection of the map can be changed by modifying the 
#' \code{proj} argument. For futher help on tmap arguements, please go the
#' \code{\link{tmap}} reference manual.

#'
#' @param r a \code{Raster*} class object with an image or stack of images to be plotted. If \code{r} is a \code{list} 
#' of \code{RasterStack}, \code{genPlotGIS} treates the stacks as RGB images.
#' @param region a \code{Spatial*}, projected \code{raster*}, or \code{sf} class object 
#' defining the area of interest.
#' @param proj a \code{character} or 'CRS' class object defining the coordinate 
#' reference system of the plot.
#' @param breaks a \code{numeric} vector defining the color breaks of the legend.
#' @param labels a \code{character} vector defining the labels in the breaks of the legend.
#' @param zlim a \code{numeric} vector defining the maximun and minimun pixel values to be mapped.
#' @param layout a \code{numeric} vector defining rows and columns to divide the plotting area.
#' @param nbreaks a \code{numeric} argument defining the default number of breaks.
#' @param nlabels a \code{numeric} argument defining the default number of labels in the legend.
#' @param as.grid a \code{logical} argument. If \code{TRUE}, removes the space between 
#' plotted layers. 
#' @param compass.rm a \code{logical} argument to remove the compass from the plot. 
#' \code{FALSE} by default.
#' @param scale.bar.rm a \code{logical} argument to remove the scale bat from the plot. 
#' \code{FALSE} by default.
#' @param ... argument for nested functions:
#' \itemize{
#'   \item \code{tm_layout} any argument accepted by the \code{tm_layout} function.
#'   \item \code{tm.graticules} any argument accepted by the \code{tm_graticules} function. 
#'   The arguments are defined as \code{tm.graticules.arg}, where \code{arg} is the 
#'   \code{tm_graticules} argument name. For example, the \code{labels.size} of \code{tm_graticules} 
#'   is defined as \code{tm.graticules.labels.size}.
#'   \item \code{tm.compass} any argument accepted by the \code{tm_compass} function. 
#'   The arguments are defined as \code{tm.compass.arg}, where \code{arg} is the 
#'   \code{tm_compass} argument name. For example, the \code{type} of \code{tm_compass} 
#'   is defined as \code{tm.compass.type}.
#'   \item \code{tm.scale.bar} any argument accepted by the \code{tm_scale_bar} function. 
#'   The arguments are defined as \code{tm.scale.bar.arg}, where \code{arg} is the 
#'   \code{tm_scale_bar} argument name. For example, the \code{text.size} of \code{tm_scale_bar} 
#'   is defined as \code{tm.scale.bar.text.size}.
#'   \item \code{tm.shape} and \code{tm.polygon} refer to the \code{region} argument. Any argument accepted by the 
#'   \code{tm_shape} and \code{tm_polygon} functions. 
#'   The arguments are defined as \code{tm.shape.region.arg} or \code{tm.polygon.region.arg}, where \code{arg} is the 
#'   \code{tm_shape} and \code{tm_polygon} argument name respectively. For example, the \code{lwd} of \code{tm_polygon} 
#'   is defined as \code{tm.polygon.region.lwd}.
#'   \item \code{tm.shape} and \code{tm.raster} refer to the \code{r} argument. Any argument accepted by the 
#'   \code{tm_shape} and \code{tm_raster} functions. 
#'   The arguments are defined as \code{tm.shape.r.arg} or \code{tm.raster.r.arg}, where \code{arg} is the 
#'   \code{tm_shape} and \code{tm_raster} argument name respectively. For example, 
#'   the \code{legend.reverse} of \code{tm_raster} is defined as \code{tm.raster.r.legend.reverse}.
#'   \item \code{tm.tmap.arrange} any argument accepted by the \code{tm_tmap_arrange} function. 
#'   The arguments are defined as \code{tm.tmap.arrange.arg}, where \code{arg} is the 
#'   \code{tm_tmap_arrange} argument name. For example, the \code{asp} of \code{tm_tmap_arrange} 
#'   is defined as \code{tm.tmap.arrange.asp}. This arguments are only accepted when plotting a \code{list}
#'   of stack images to plot as RGB.
#' }
#'
#' @return \code{tmap} class containing the plot.
#'
#' @examples
#' # Simple plot of NDVI in Navarre
#' genPlotGIS(ex.ndvi.navarre,
#'            ex.navarre)
#' 
#' # Using tm arguments        
#'genPlotGIS(ex.ndvi.navarre,
#'           ex.navarre,
#'           tm.compass.size=1,
#'           tm.compass.type="rose",
#'           tm.scale.bar.text.size=0.8,
#'           tm.polygon.region.lwd=6,
#'           tm.polygon.region.border.col="#000000",
#'           tm.raster.r.palette=rev(terrain.colors(40)),
#'           tm.raster.r.title="NDVI",
#'           as.grid = T,
#'           tm.graticules.labels.size=1,
#'           tm.graticules.n.x=3,
#'           tm.graticules.n.y=3)
#' # Using the view mode of tmap for ploting the images in the viewer
#' tmap_mode("view")
#' genPlotGIS(ex.ndvi.navarre,
#'            ex.navarre,
#'            tm.raster.r.palette=rev(terrain.colors(40)))+
#'            tm_facets(as.layers=TRUE)
#'            
#' # path to the cropped and cutted MODIS images for the region of Navarre
#' wdir <- system.file("ExNavarreVar", package = "RGISTools")
#' # list all the tif files
#' files.mod <- list.files(wdir, pattern="\\.tif$", recursive = TRUE, full.names = TRUE)
#' # print the MOD09 bands
#' getRGISToolsOpt("MOD09BANDS")
#' 
#' # select the red, blue and NIR bands
#' img.mod.red <- raster(files.mod[1])
#' img.mod.blue <- raster(files.mod[3])
#' img.mod.green <- raster(files.mod[4])
#' img.mod.rgb<-varRGB(img.mod.red,img.mod.green,img.mod.blue)
#' genPlotGIS(ex.ndvi.navarre,
#'            ex.navarre)+
#'            tm_facets(as.layers = T)+
#' genPlotGIS(list(img.mod.rgb),
#'            ex.navarre)
genPlotGIS<-function(r,region,breaks,labels,zlim,layout,proj,nbreaks=40,nlabels=10,as.grid=TRUE,compass.rm=FALSE,scale.bar.rm=FALSE,...){
  args<-list(...)
  
  # r and region projection management
  if(class(r)=="list"){
    if(class(r[[1]])=="RasterBrick"|class(r[[1]])=="RasterStack"){
      if(!missing(proj)){
          r = lapply(r, projectRaster,crs=proj)
        if(!missing(region)){region=transform_multiple_proj(region,proj4=projection(r[[1]]))}
      }
    }else{
      stop("genPlotGIS only supports RasterBrick or RasterStack, or a list composed by RasterBrick or RasterStack.")
    }
  }else{
    if(class(r)=="RasterBrick"|class(r)=="RasterStack"|class(r)=="RasterLayer"){
      if(!missing(proj)){
        r = projectRaster(r,crs=proj)
        if(!missing(region)){region=transform_multiple_proj(region,proj4=projection(r))}
      }
    }else{
      stop("genPlotGIS only supports RasterBrick or RasterStack, or a list composed by RasterBrick or RasterStack.")
    }
  }
  
  
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
  
  graticules_args<-c(names(formals(tm_graticules)),names(formals(tm_grid)))
  names(graticules_args)<-paste0("tm.graticules.",graticules_args)
  tm_graticules_args<-args[names(args)%in%names(graticules_args)]
  names(tm_graticules_args)<-graticules_args[names(tm_graticules_args)]
  if(!("lines" %in% names(tm_graticules_args))){
    tm_graticules_args$lines=FALSE
  }
  if(as.grid){
    tm_layout_args$between.margin=-.1
    if(!("labels.space.x" %in% names(tm_layout_args))){
      tm_graticules_args$labels.space.x=.10
    }
    if(!("labels.space.y" %in% names(tm_layout_args))){
      tm_graticules_args$labels.space.y=.10
    }
  }
  grid<-do.call(tm_graticules,tm_graticules_args)

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
    if(!("show.labels" %in% names(tm_compass_args))){
      tm_compass_args$show.labels=0
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

  if(class(r)=="list"){
    ####################################################
    # RGB plot
    ####################################################
    maplist<-lapply(r,function(shp,compass,scale.bar,grid,reg){return(tm_shape(shp=shp,frame=T)+tm_rgb()+compass+scale.bar+grid+reg)},compass,scale.bar,grid,reg)
      #tmap_arrange arguments
      tmap_arrange_args<-names(formals(tmap_arrange))
      tmap_arrange_args<-unique(tmap_arrange_args[!(tmap_arrange_args%in%"...")])
      names(tmap_arrange_args)<-paste0("tmap.arrange.",tmap_arrange_args)
      tm_tmap_arrange_args<-args[names(args)%in%names(tmap_arrange_args)]
      names(tm_tmap_arrange_args)<-tmap_arrange_args[names(tm_tmap_arrange_args)]
      
      if(!("asp" %in% tm_tmap_arrange_args)){
        tm_tmap_arrange_args$asp=NA
      }
      
      if(missing(layout)){
        if(length(r)>1){
          tm_tmap_arrange_args$ncol=ceiling(sqrt(length(r)))
        }else{
            return(tm_shape(shp=r[[1]],frame=T)+tm_rgb()+compass+scale.bar+grid+reg)
        }
        
      }else{
        tm_tmap_arrange_args$nrow=layout[1]
        tm_tmap_arrange_args$ncol=layout[2]
      }
      
      return(do.call(tmap_arrange,c(maplist,tm_tmap_arrange_args)))
  }

  ####################################################
  # Stack plot
  ####################################################
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
  return(do.call(tm_shape,tm_shape_r_args) + do.call(tm_raster,tm_raster_r_args) +# tm_facets(nrow=3,ncol=2)+# raster conf
         do.call(tm_layout,tm_layout_args) +# layout
         compass + #the compass
         reg+ #region
         scale.bar+#scale
         grid+
         lyt)
}





