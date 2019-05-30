#' Plots satellite images with a proper GIS format
#'
#' This function helps displaying satellite images with usual GIS information, such as scale, the north arrow and the region of interest
#'
#' The function plots any raster type class as, \code{raster}, \code{RasterStack} or \code{RasterBrick}.
#' Accepts all parameter used by the function \code{spplot}.
#' The function genPlotGIS adds a north arrow and a polygon with the area of interest.
#' If necessary, the projection of the polygon is transformed to match the projection of the raster.
#' The projection of the map can be changed by adding the \code{proj} argument. The arrow and scale bar are
#' located by default in relative positions. Their positions can be modified with the \code{compOpt} paragram.
#'
#' @param r \code{raster} class variable with the image or image stack to be plotted
#' @param region a \code{polygon} class variable defining the area of interest
#' @param ... other arguments accepted by the function \code{spplot} or
#' \code{compOpt} as options to fit the size and the location of the
#' GIS components as the arrow and the scales
#'
#' @examples
#' data(ex.ndvi.navarre)
#' data(navarre)
#'
#' # Shows a panel of maps, one per date and the region of interest in shown on the fourth map
#' genPlotGIS(r=ex.ndvi.navarre[[1:4]],
#'            region=navarre,
#'            which = c(4),#show region only y 4th image
#'            wComp=c(4)  #show component only y 4th image
#' )
#'
#' \dontrun{
#' # Plotting the land surface temperature of the first date available in 2011 in Navarre
#' # using color palette
#' library('RColorBrewer')
#' my.palette <- rev(brewer.pal(n = 9, name = "YlOrRd"))
#' genPlotGIS(r=ex.ndvi.navarre[[1]],
#'            region=navarre,
#'            proj=CRS("+init=epsg:4670"), #project all components
#'            col.regions=colorRampPalette(my.palette),
#' )
#' }
#'
#' # change scale text relative Y
#' genPlotGIS(r=ex.ndvi.navarre[[1:4]],
#'          region=navarre,
#'            proj=CRS("+init=epsg:4670"),
#'          compOpt=list(
#'            #arrow relatives 0-1
#'            ArrowRelativeX=0.85,
#'            ArrowRelativeY=0.1,
#'            ArrowRelativeSize=0.15,
#'            #scale relatives 0-1
#'            scaleRelativeX=0.1,
#'            scaleRelativeY=0.1,
#'            scaleRelativeSize=0.15,
#'            #scale text relatives 0-1
#'            scaleLabelRelativeX=0.1,
#'            scaleLabelRelativeY=0.14,
#'            scaleLabelSize=5000
#'         )
#' )
genPlotGIS<-function(r,region=NULL,...){
  arg<-list(...)
  if(!is.null(region)){
    if("proj"%in%names(arg)){
      r<-projectRaster(r,crs=arg$proj)
      region<-spTransform(region,proj4string(r))
      r<-crop(r,region)
    }else{
      region<-spTransform(region,proj4string(r))
    }
  }
  if(!"lwd"%in%names(arg)){lwd=1}else{lwd=arg$lwd}
  args_gis.components <- arg[names(arg) %in% names(formals(.componentPosition))]
  #scale and north arrow
  gis.components=do.call(.componentPosition,c(ext=extent(r),rasProj=proj4string(r),args_gis.components))
  #gis.components=.componentPosition(ext=extent(ras),rasProj=proj4string(ras),args_gis.components)

  spplot(r,
         sp.layout=list(list("sp.polygons", region,first = FALSE,lwd=lwd,...),
                        gis.components),
         scales=list(draw=T),
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
                 scaleRelativeX=0.1,
                 scaleRelativeY=0.1,
                 scaleRelativeSize=0.15,
                 #scale text relatives
                 scaleLabelRelativeX=0.1,
                 scaleLabelRelativeY=0.12,
                 scaleLabelSize=5000
    )
  }


  rbbox<-bbox(ext)
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
                       l3 = list("sp.text",ptlScale, "0",scale=compOpt$scaleLabelSize,first=first,which=wComp),
                       l4 = list("sp.text", ptl2Scale, paste(scaleSize,pUnits),scale=compOpt$scaleLabelSize,first=first,which=wComp),
                       arrow = list("SpatialPolygonsRescale",
                                    layout.north.arrow(),
                                    offset = posArrow,
                                    scale = arrowscale,
                                    first=first,which=wComp))
  return(gis.components)
}




