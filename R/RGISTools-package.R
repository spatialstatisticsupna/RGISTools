#' RGISTools: Tools for Downloading, Smoothing, and Processing Time-Series of Satellite Images
#'
#' This package aims for downloading, processing, and smoothing time series of satellite images from 
#' Landsat, Modis, and Sentinel satellite programs in a uniform and standardized way. The functions of 
#' RGISTools automatically convert the original formats into GTiff files, which can be loaded into R. 
#' The processing functions of RGISTools includes tile mosaicking, cropping, cloud masking, and extracting the variables 
#' of interest. When multiple tiles matches the region of interest, RGISTools combines them to generate a 
#' single image and crops the area of interest to save memory, and computing time. All images available 
#' in a range of dates are then stacked to produce time series of satellite images for a particular area. 
#' Images may have clouds or measurement errors that limit their use in subsequent analyses. Hence, the 
#' package includes a set of functions for removing clouds, gap filling, and smoothing. Due to the wide 
#' variety of procedures and sources of information being handled in RGISTools, the functions are divided 
#' into 7 categories, which are identified by the first 3 characters of the function names; 
#' \enumerate{
#'   \item \code{mod} identifies Modis Terra and Aqua satellite functions.
#'   \item \code{sen} identifies Sentinel functions.
#'   \item \code{ls7} identifies Landsat 7 functions.
#'   \item \code{ls8} identifies Landsat 8 functions.
#'   \item \code{ls} identifies both Landsat 7 and 8 functions.
#'   \item \code{gen} identifies function to use with any of the three platforms.
#'   \item \code{var} identifies function to calculate variables with any of the three platforms.
#' }
#' 
#' Below is a list of the most important functions grouped by running order for creating a time series of
#' images for each satellite considered in the package.
#' Each satellite group of functions considers all the procedure including searching, previewing, 
#' downloading, mosaicking, extracting variables, composing, and filling-smoothing.
#' @section I. Landsat time series functions:
#' Landsat mission releases images comming from two satellites, Landsat-7 and Landsat-8. Each satellite needs its own 
#' download and processing workflow. The download of any Landsat images requires a USGS login account. 
#' \href{https://ers.cr.usgs.gov/register/}{Get your credentials}.
#' 
#' \subsection{Landsat-7}{
#' \tabular{ll}{
#'   \code{ \link{ls7LoadMetadata}}\tab Load Landsat-7 meta data file for image search \cr
#'   \code{\link{ls7Search}} \tab Search Landsat-7 time-series images list \cr
#'   \code{\link{lsPreview}} \tab Preview in R Landsat satellite images \cr
#'   \code{\link{lsDownSearch}} \tab Download a time series of satellite images from Landsat\cr
#'   \code{\link{lsCloudMask}} \tab Create clouds layers for Landsat images \cr
#'   \code{\link{lsMosaic}} \tab Mosaic Landsat time series of images \cr
#'   \code{\link{ls7FolderToVar}} \tab Compute derived variables from Landsat-7 multispectral bands \cr
#'   \code{\link{lsCloudMask}} \tab Create clouds layers for Landsat images \cr
#'   \code{\link{genSaveTSRData}} \tab Import into R the processed time series of images \cr
#'   ----------------------\tab -------------------------------------------------------------------------------------------- \cr  
#'   }
#' }
#' \subsection{Landsat-8}{
#' \tabular{ll}{
#'   \code{ \link{ls8LoadMetadata}}\tab Load Landsat-8 meta data file for image search \cr
#'   \code{\link{ls8Search}} \tab Search Landsat-8 time-series images list \cr
#'   \code{\link{lsPreview}} \tab Preview in R Landsat satellite images \cr
#'   \code{\link{lsDownSearch}} \tab Download a time series of satellite images from Landsat\cr
#'   \code{\link{lsCloudMask}} \tab Create clouds layers for Landsat images \cr
#'   \code{\link{lsMosaic}} \tab  Mosaic Landsat time series of images\cr
#'   \code{\link{ls8FolderToVar}} \tab Compute derived variables from Landsat-8 multispectral bands\cr
#'   \code{\link{genSaveTSRData}} \tab Import into R the processed time series of images \cr
#'   ----------------------\tab -------------------------------------------------------------------------------------------- \cr  
#'   }
#' }
#' 
#' @section II. Modis time series functions:
#' Terra and Aqua satellite have onboard more than one sensor. These functions allows to download all land products from Terra
#' and Aqua satellites but the processing is focused on the multispectral images.
#' The download of any Modis product requires the credentianls from EarthData to access the NASA’s web data service. 
#' \href{https://urs.earthdata.nasa.gov/users/new}{Get your credentials}.
#' \tabular{ll}{
#'   \code{ \link{modSearch}} \tab Search Modis time-series images list \cr
#'   \code{\link{modPreview}} \tab Preview in R Modis satellite images\cr
#'   \code{\link{modDownSearch}} \tab Download a Modis time series of satellite images\cr
#'   \code{\link{modMosaic}} \tab Mosaic Modis time series of images \cr
#'   \code{\link{modFolderToVar}} \tab Compute derived variables from Modis multispectral bands\cr
#'   \code{\link{modCloudMask}} \tab Create clouds layers for Modis images \cr
#'   \code{\link{genSaveTSRData}} \tab Import into R the processed time series of images \cr
#'   ----------------------\tab -------------------------------------------------------------------------------------------- \cr  
#'   }
#' 
#' @section III. Sentinel time series functions:
#' Sentinel satellite program includes 5 different satellite types. These functions allows to download all the products 
#' from SciHub platform, ut the processing is focused on the Sentinel-2 multispectral images. 
#' The download of any Sentinel product requires the credentianls from ESA’s SciHub data service.
#' \href{https://scihub.copernicus.eu/dhus/#/self-registration}{Get your credentials}.
#' \tabular{ll}{
#'   \code{ \link{senSearch}} \tab Search Sentinel time-series images list\cr
#'   \code{\link{senPreview}} \tab Preview in R Sentinel satellite images\cr
#'   \code{\link{senDownSearch}} \tab Download a Sentinel time series of satellite images\cr
#'   \code{\link{senMosaic}} \tab Mosaic Sentinel time series of images \cr
#'   \code{\link{senCloudMask}} \tab Create clouds layers for Sentinel images \cr
#'   \code{\link{senFolderToVar}} \tab Compute derived variables from Sentinel-2 multispectral bands\cr
#'   \code{\link{genSaveTSRData}} \tab Import into R the processed time series of images\cr
#'   ----------------------\tab -------------------------------------------------------------------------------------------- \cr  
#'   }
#' 
#' @section IV. Important general functions:
#' In addition to satellite downloading and processing functions, the package provides some general functions to easy the data
#' manage.
#' \tabular{ll}{
#'   \code{ \link{genCompositions}} \tab Create image compositions from a time series of satellite images\cr
#'   \code{\link{genSmoothingIMA}} \tab Fill the gaps in a time series of satellite images\cr
#'   \code{\link{genSmoothingCovIMA}} \tab Smooth outliers in a time series of satellite images using covariates\cr
#'   \code{\link{genPlotGIS}} \tab Plot satellite images with a proper GIS format\cr
#'   \code{\link{genGetDates}} \tab Get a date from the name of a raster layer\cr
#'   ----------------------\tab -------------------------------------------------------------------------------------------- \cr  
#'   }
#' 
#' @section V. Variable functions:
#' Many variables can be derived from multispectral images, the most common variables are preprogramed with the 'var' prefix.
#' \tabular{ll}{
#'   \code{ \link{varEVI}}\tab Calculate the enhanced vegetation index (EVI) from multispectral bands\cr
#'   \code{\link{varMSAVI2}} \tab Calculate the modified soil-adjusted vegetation index (MSAVI2) from multispectral bands\cr
#'   \code{\link{varNBR}} \tab Calculate the normalized burn ratio (NBR) from multispectral bands\cr
#'   \code{\link{varNBR2}} \tab Calculate the normalized burn ratio 2 (NBR2) from multispectral bands\cr
#'   \code{\link{varNDMI}} \tab Calculate the normalized difference moisture index (NDMI) from multispectral bands\cr
#'   \code{\link{varNDVI}} \tab Calculate the normalized difference vegetation index (NDVI) from multispectral bands\cr
#'   \code{\link{varNDWI}} \tab  Calculate the normalized difference water index (NDWI) from multispectral bands\cr
#'   \code{\link{varRGB}} \tab  Calculate an RGB image from 3 spectral bands from multispectral bands\cr
#'   \code{\link{varSAVI}} \tab  Calculate the soil-adjusted vegetation index (SAVI) from multispectral bands\cr
#'   ----------------------\tab -------------------------------------------------------------------------------------------- \cr  
#' }
#' 
#' @docType package
#' @name RGISTools-package
NULL