# Function for Sentinel-1 images radar correction
#
# \code{snapyRadarCorrection} call snap esas app for project Sentinel-1 radar images
#
# Sentinel-1 images, are releases without projection. Instead of that, in images metadata there are specified
# georeferences points. The projection of the image can be derive from metadata. For this purpose an ESA application can be used, SNAP.
# SNAP provides procedures and function for Sentinel images data interpretation, and accepts python scripting.
# This function works as interface to SNAP calling a python script that adds projection to Sentinel-1 images. For this purpose
# python 3.4 (later versions do not works) installation and SNAP instalation is required
# and both path will be required by \code{snapyRadarCorrection}.
#
# @param snapydir SNAP application
# @param indirname Sentinel-1 Image dir
# @param outdirname Result folder
# @param orchdir Working directory for python
# @param pyPath path to python 3.4 installed folder (default system python)
# @param verbose Debug flag
#
# @examples
# #Add projection to Sentinel-1 image
# senSnapyRC(snapydir='/path2snap/snap-python/',
#            indirname='/path2Image/IMAGE.SAFE',
#            outdirname='/path2Result',
#            pyPath="/path2Python34",
#            verbose = T)
senSnapyRC<-function(snapydir,indirname,outdirname,orchdir=getwd(),pyPath="python",verbose=FALSE){
  if(!file.exists(outdirname))
    dir.create(outdirname,recursive=T)
  pyFile <- system.file("python","RadarCorrection.py",package="RGISTools")
  command <- paste(pyPath, pyFile, snapydir,indirname,outdirname,orchdir)
  if(verbose)
    print(command)
  response <- system(command, intern=T)
  print(response)
}


# snapyRadarCorrection2<-function(snapydir,indirname,outdirname,orchdir=getwd(),pyPath="C:/Python34/python.exe",verbose=FALSE){
#   if(!file.exists(outdirname))
#     dir.create(outdirname,recursive=T)
#
#   pyFile <- system.file("python","runSnap.py",package="RGISTools")
#   pyFile<-"D:/01-Packages/RGISTools/inst/python/runSnap.py"
#   command <- paste(pyPath, pyFile, snapydir,indirname,outdirname,orchdir)
#   if(verbose)
#     print(command)
#   response <- system(command, intern=T)
#   print(response)
# }
