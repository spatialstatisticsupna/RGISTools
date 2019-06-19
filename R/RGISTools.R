#' RGISTools: Tools for Downloading and Processing Time-Series of Satellite Images
#'
#' This package aims for downloading, processing, and smoothing time series of satellite images from 
#' Landsat, MODIS and Sentinel satellite programs in a uniform and standardized way. The functions of 
#' RGISTools automatically convert the original formats into .tif files, which can be loaded into R. 
#' The processing functions of RGISTools include tile mosaicking, cropping, and extracting the variables 
#' of interest. When multiple tile match the region of interest, RGISTools combines them to generate a 
#' single image and crops the area of interest to save memory and computing time. All images available 
#' in a range of dates are then stacked to produce time series of satellite images for a particular area. 
#' Images may have clouds or measurement errors that limit their use in subsequent analyses. Hence, the 
#' package includes a set of functions for removing clouds, gap filling and smoothing. Due to the wide 
#' variety of procedures and sources of information being handled in RGISTools, the functions are divided 
#' into 6 categories, which are identified by the first 3 characters of the function names; 
#' \enumerate{
#'   \item \code{mod} for MODIS Terra and Aqua satellite images
#'   \item \code{sen} for Sentinel images
#'   \item \code{ls7} for Landsat 7 images
#'   \item \code{ls8} for Landsat 8 images
#'   \item \code{ls} for both Landsat 7 and 8 images
#'   \item \code{gen} for any of the three platforms.
#' }
#' 
#' @section Modis functions
#' @section Sentinel functions
#' @section Landsat functions
#' @section General functions
#'
#' @docType package
#' @name RGISTools
NULL