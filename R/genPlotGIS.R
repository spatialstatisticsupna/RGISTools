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
#' @return this function does not return anything.
#'
#' @examples
#' # load the ndvi object of Navarre
#' data(ex.ndvi.navarre)
#' # load a spatial polygon object of Navarre
#' data(ex.navarre)
#'
#' # show a panel of 4 maps, one per date
#' # the region of interest is shown in the last map
#' genPlotGIS(r = ex.ndvi.navarre[[1:4]],
#'            region = ex.navarre,
#'            which = c(4), # show region only in the 4th image
#'            wComp = c(4) # show component only in the 4th image
#' )
#'
#' \dontrun{
#' # plot the land surface temperature of the first date available in 2011 in Navarre
#' # using color palette
#' library('RColorBrewer')
#' my.palette <- rev(brewer.pal(n = 9, name = "YlOrRd"))
#' genPlotGIS(r = ex.ndvi.navarre[[1]],
#'            region = ex.navarre,
#'            proj = st_crs("+init=epsg:4670")$proj4string, # project all components
#'            col.regions = colorRampPalette(my.palette)
#' )
#' }
#'
#' # change scale text relative Y
#' genPlotGIS(r = ex.ndvi.navarre[[1:4]],
#'            region = ex.navarre,
#'            proj = st_crs("+init=epsg:4670")$proj4string,
#'            compOpt=list(
#'            # arrow relatives 0-1
#'            ArrowRelativeX = 0.85,
#'            ArrowRelativeY = 0.1,
#'            ArrowRelativeSize = 0.15,
#'            # scale relatives 0-1
#'            scaleRelativeX = 0.1,
#'            scaleRelativeY = 0.1,
#'            scaleRelativeSize = 0.15,
#'            # scale text relatives 0-1
#'            scaleLabelRelativeX = 0.1,
#'            scaleLabelRelativeY = 0.2,
#'            scaleLabelSize = 0.8
#'         )
#' )
genPlotGIS<-function(r,region=NULL,cex=1,...){
  arg<-list(...)
  if("proj"%in%names(arg)){
    r<-projectRaster(r,crs=arg$proj)
    r<-trim(r)
  }
  
  if(!is.null(region)){
    if("proj"%in%names(arg)){
      region<-transform_multiple_proj(region,arg$proj)
      region<- as(region, 'Spatial')
    }else{
      region<-transform_multiple_proj(region,projection(r))
      region<-as(region, 'Spatial')
    }
  }
  if(!"lwd"%in%names(arg)){lwd=1}else{lwd=arg$lwd}
  args_gis.components <- arg[names(arg) %in% names(formals(.componentPosition))]
  #scale and north arrow
  gis.components=do.call(.componentPosition,c(ext=extent(r),rasProj=projection(r),args_gis.components))

  spplot(r,
         sp.layout=list(list("sp.polygons", region,first = FALSE,lwd=lwd,...),
                        gis.components),
         scales=list(draw=TRUE,cex=cex),
         ...
  )
}


.componentPosition<-function(ext,rasProj,wComp=NULL,compOpt=NULL,first=NULL,...){
  arg<-list(...)
  if(is.null(first)){
    first=F
  }
  #componets relative position an size default
  if(is.null(compOpt)){
    compOpt=list(#arrow relatives
                 ArrowRelativeX=0.85,
                 ArrowRelativeY=0.1,
                 ArrowRelativeSize=0.15,
                 #scale relatives
                 scaleRelativeX=0.13,
                 scaleRelativeY=0.1,
                 scaleRelativeSize=0.18,
                 #scale text relatives
                 scaleLabelRelativeX=0.14,
                 scaleLabelRelativeY=0.16,
                 scaleLabelSize=5000
    )
  }


  rbbox<-matrix(st_bbox(ext),2)
  differences<-rbbox[,2]-rbbox[,1]
  posArrow<-c(differences[1]*compOpt$ArrowRelativeX+rbbox[1,1]
              ,differences[2]*compOpt$ArrowRelativeY+rbbox[2,1])
  if(differences[1]>differences[2]){
    arrowscale<-differences[2]*compOpt$ArrowRelativeSize
  }else{
    arrowscale<-differences[1]*compOpt$ArrowRelativeSize
  }

  scaleSize<-differences[1]*compOpt$scaleRelativeSize
  scaleSize<-round(scaleSize,floor(log10(scaleSize))*-1)

  #scale bar
  ptScale<-c(differences[1]*compOpt$scaleRelativeX+rbbox[1,1],differences[2]*compOpt$scaleRelativeY+rbbox[2,1])

  #labels
  ptlScale<-c(differences[1]*compOpt$scaleLabelRelativeX+rbbox[1,1],differences[2]*compOpt$scaleLabelRelativeY+rbbox[2,1])
  ptl2Scale<-c(differences[1]*(compOpt$scaleLabelRelativeX+0.02)+rbbox[1,1]+scaleSize,differences[2]*compOpt$scaleLabelRelativeY+rbbox[2,1])

  #get position
  pUnits<-unlist(strsplit(rasProj,"+",fixed = TRUE))
  if(any(grepl("longlat",pUnits))){
    pUnits<-rawToChar(as.raw(186))
  }else{
    pUnits<-pUnits[grepl("units",pUnits)]
    pUnits<-gsub(" ","",gsub("units=","",pUnits))
  }

  gis.components<-list(scale = list("SpatialPolygonsRescale",
                                    layout.scale.bar(),
                                    offset = ptScale,
                                    scale = scaleSize,
                                    fill=c("transparent","black"),first=first,which=wComp),
                       l3 = list("sp.text",ptlScale, "0",cex=compOpt$scaleLabelSize,first=first,which=wComp),
                       l4 = list("sp.text", ptl2Scale, paste(scaleSize,pUnits),cex=compOpt$scaleLabelSize,first=first,which=wComp),
                       arrow = list("SpatialPolygonsRescale",
                                    layout.north.arrow(),
                                    offset = posArrow,
                                    scale = arrowscale,
                                    first=first,which=wComp))
  return(gis.components)
}




