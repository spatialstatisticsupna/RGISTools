# RGISTools
Tools for Downloading, Processing, and Smoothing Time Series of Satellite Images From Landsat, Modis, and Sentinel.

## Table of contents

- [Introduction](#introduction)
- [Installation](#installation)
- [Credentials for downloading satellite images](#credentials-for-downloading-satellite-images)
- [Copyright and license](#copyright-and-license)


# The package
This package aims for downloading, processing, and smoothing time series of satellite images from 
Landsat, Modis and Sentinel satellite programs in a uniform and standardized way. The functions of 
RGISTools automatically convert the original formats into .tif files, which can be loaded into R. 
The processing functions of RGISTools include tile mosaicking, cropping, and extracting the variables 
of interest. When multiple tile match the region of interest, RGISTools combines them to generate a 
single image and crops the area of interest to save memory and computing time. All images available 
in a range of dates are then stacked to produce time series of satellite images for a particular area. 
Images may have clouds or measurement errors that limit their use in subsequent analyses. Hence, the 
package includes a set of functions for removing clouds, gap filling and smoothing. Due to the wide 
variety of procedures and sources of information being handled in RGISTools, the functions are divided 
into 7 categories, which are identified by the first 3 characters of the function names; 

1. ```mod``` for Modis Terra and Aqua satellite images.
2. ```sen``` for Sentinel images.
3. ```ls7``` for Landsat 7 images.
4. ```ls8``` for Landsat 8 images.
5. ```ls``` for both Landsat 7 and 8 images.
6. ```gen``` for any of the three platforms.
7. ```var``` for any of the three platforms.

Below is a list of the most important functions grouped by running order for creating a time series of
images for each satellite considered by the package.
Each satellite group of functions considers all the procedure including searching, previewing, 
downloading, mosaicking, extracting variables, composing, and filling-smoothing.

## I. Landsat time series functions
Landsat mission releases images comming from two satellites, Landsat-7 and Landsat-8. Each satellite needs its own 
download and processing workflow. The download of any Landsat images requires a USGS login account. 
[Get your credentials](https://ers.cr.usgs.gov/register/).

### Landsat-7

* ```ls7LoadMetadata``` To load Landsat-7 meta data file for image search .
* ```ls7Search``` To search Landsat-7 time-series images list .
* ```lsPreview``` To preview in R Landsat satellite images .
* ```lsDownSearch``` To download a time series of satellite images from Landsat.
* ```lsMosaic``` To mosaic Landsat time series of images .
* ```ls7FolderToVar``` To compute derived variables from Landsat-7 multispectral bands .
* ```genSaveTSRData``` To import into R the processed time series of images .

### Landsat-8
* ```ls8LoadMetadata``` To load Landsat-8 meta data file for image search .
* ```ls8Search``` To search Landsat-8 time-series images list .
* ```lsPreview``` To preview in R Landsat satellite images .
* ```lsDownSearch``` To download a time series of satellite images from Landsat.
* ```lsMosaic```  To mosaic Landsat time series of images.
* ```ls8FolderToVar``` To compute derived variables from Landsat-8 multispectral bands.
* ```genSaveTSRData``` To import into R the processed time series of images .

## II. Modis time series functions
The download of any Modis product requires the credentianls from EarthData to access the NASA’s web data service. 
[Get your credentials](https://urs.earthdata.nasa.gov/users/new).
* ```modSearch``` To search Modis time-series images list .
* ```modPreview``` To preview in R Modis satellite images.
* ```modDownSearch``` To download a Modis time series of satellite images.
* ```modMosaic``` To mosaic Modis time series of images .
* ```modFolderToVar``` To compute derived variables from Modis multispectral bands.
* ```genSaveTSRData``` To import into R the processed time series of images .

## III. Sentinel time series functions
The download of any Sentinel product requires the credentianls from ESA’s SciHub data service.
[Get your credentials](https://scihub.copernicus.eu/dhus/#/self-registration).
* ```senSearch``` To search Sentinel time-series images list.
* ```senPreview``` To preview in R Sentinel satellite images.
* ```senDownSearch``` To download a Sentinel time series of satellite images.
* ```senMosaic``` To mosaic Sentinel time series of images .
* ```senFolderToVar``` To compute derived variables from Sentinel-2 multispectral bands.
* ```genSaveTSRData``` To import into R the processed time series of images.

## IV. Important general functions
* ```genCompositions``` To create image compositions from a time series of satellite images.
* ```genSmoothingIMA``` To fill the gaps in a time series of satellite images.
* ```genSmoothingCovIMA``` To smooth outliers in a time series of satellite images using covariates.
* ```genPlotGIS``` To plot satellite images with a proper GIS format.
* ```genGetDates``` To get a date from the name of a raster layer.


## V. Variable functions
* ```varEVI``` To calculate the enhanced vegetation index (EVI) from multispectral bands.
* ```varMSAVI2``` To calculate the modified soil-adjusted vegetation index (MSAVI2) from multispectral bands.
* ```varNBR``` To calculate the normalized burn ratio (NBR) from multispectral bands.
* ```varNBR2``` To calculate the normalized burn ratio 2 (NBR2) from multispectral bands.
* ```varNDMI``` To calculate the normalized difference moisture index (NDMI) from multispectral bands.
* ```varNDVI``` To calculate the normalized difference vegetation index (NDVI) from multispectral bands.
* ```varNDWI```  To calculate the normalized difference water index (NDWI) from multispectral bands.
* ```varRGB```  To calculate an RGB image from 3 spectral bands from multispectral bands.
* ```varSAVI```  To calculate the soil-adjusted vegetation index (SAVI) from multispectral bands.


# Installation

## Install from GitHub
```
# Install devtools package from cran repository
install.packages("devtools")

# load devtools library
library(devtools)

# Install RGISTools from GitHub repositoy
install_github("spatialstatisticsupna/RGISTools")
```

# Credentials for downloading satellite images

### Modis
Credentials [EarthData](https://ers.cr.usgs.gov/register/) 

### Landsat
Credentials [EarthData](https://ers.cr.usgs.gov/register/) 

### Sentinel
Credentials [SciHub](https://scihub.copernicus.eu/dhus/#/self-registration) 

## Copyright and license
Licensed under the GPL-3 License. [Full license here](/blob/master/LICENSE.md).