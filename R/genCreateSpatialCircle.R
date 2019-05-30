genCreateSpatialCircle<-function(x,y,rCircle=0.0001,nCirclePoints=20){
  # center x center y
  pts = seq(0, 2 * pi, length.out = nCirclePoints)
  xy = cbind(x + rCircle * sin(pts), y + rCircle * cos(pts))
  return(xy)
}