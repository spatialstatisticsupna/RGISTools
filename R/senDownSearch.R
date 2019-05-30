#' Downloads Sentinel images from search function response
#'
#' \code{senDownSearch} downloads the list of urls generated y the function senSearch by using the ESA’s SciHub API
#'
#' \code{senDownSearch} downloads the images from Sentinel products using the search results provided by \code{\link{senSearch}}.
#'  The raw images are downloaded into the Aproot directory. In case the download is interrupted,
#'  the image file could be corrupted. The function detects the corrupted files to restart the process.
#'  To prevent the computer from crashing, the nattempts  flag limits the number of attempts to download the image.
#'  The default number of attempts is set to 3.
#' senDownload requires the credentials to access the ESA’s SciHub data service. Please,
#' sign up at: \url{https://scihub.copernicus.eu/dhus/#/self-registration}
#'
#' @param searchres response from \code{senSearch}
#' @param username login credentials to access the ESA’s SciHub web service
#' @param password login credentials to access the ESA’s SciHub web service
#' @param unzip flag for unzipping the images
#' @param overwrite flag for overwriting the resulting the images
#' @param nattempts the number of attempts that the function has to carry out to download an image in case the file becomes corrupted.
#' @param error.log error log file name
#' @param ... argument to allow function nestering
#' \itemize{
#'   \item \code{AppRoot} the directory to save the resulting time series
#' }
#'
#' @examples
#' \dontrun{
#' # Download S2MSI1C products sensed by Sentinel - 2 satellite in July-August 2018
#' data(ex.navarre)
#' searchres<-senSearch(startDate=as.Date("2018-07-29","%Y-%m-%d"),
#'                      endDate=as.Date("2018-08-06","%Y-%m-%d"),
#'                      platform="Sentinel-2",
#'                      extent=ex.navarre,
#'                      product="S2MSI1C",
#'                      username="username",
#'                      password="password")
#'
#' #filtering the path R094 where Navarre is located
#' length(searchres)
#' searchres<-searchres[grepl("R094",names(searchres))]
#' length(searchres)
#'
#' #sentinel download function
#' senDownSearch(searchres=searchres,
#'               username="username",
#'               password="password",
#'               AppRoot="Path_for_downloading_folder",
#'               unzip=T)
#' }
senDownSearch<-function(searchres,
                        username,
                        password,
                        error.log = "download_error.log",
                        nattempts = NULL,
                        unzip=F,
                        overwrite=F,
                        ...){
  arg<-list(...)
  AppRoot<-defineAppRoot(...)
  downFolder<-file.path(AppRoot,"/raw")
  dir.create(downFolder,recursive=T,showWarnings = F)
  if(unzip){
    unzipFolder<-file.path(AppRoot,"/unzip")
    dir.create(unzipFolder,recursive=T,showWarnings = F)
  }

  for(i in 1:length(searchres)){
    url<-searchres[i]
    file.name<-names(url)
    tryCatch({
      c.handle = new_handle()
      handle_setopt(c.handle,
                    referer=getRGISToolsOpt("SCIHUBHUSURL"),
                    useragent = getRGISToolsOpt("USERAGENT"),
                    followlocation = TRUE ,
                    autoreferer = TRUE ,
                    username=username,
                    password=password)
      image.url<-URLencode(url)
      downPath<-file.path(downFolder,paste0(file.name,".zip"))
      curl_download(image.url, destfile=downPath,handle = c.handle)

      #md5 check
      md5.url<-paste0(gsub("$value","",url,fixed = T),"Checksum/Value/$value")
      print(md5.url)
      repeat{
        response<-curl(md5.url,handle =c.handle)
        md5.text<-readLines(response)
        if(!grepl("Error",md5.text)){
          print(paste0("Get md5: ",md5.text))
          break
        }else{
          message("md5 not found! trying again.")
          Sys.sleep(10)
        }
      }
      if(!genCheckMD5(downPath,oficial.md5=md5.text)){
        cat(paste0("Error cheking ",file.name," file md5: ",md5.text),file=error.log,sep="\n",append = T)
        file.remove(downPath)
        senDownSearch(username,password,url,file.path,file.name,error.log,AppRoot=AppRoot,nattempts +1)
      }else{
        print(paste0("OK: cheking ",file.name," file md5."))
        if(unzip){
          message("Unzipping ", basename(downPath)," file.")
          unzip(zipfile=downPath,
                exdir = unzipFolder,
                overwrite=overwrite)
        }
      }
    }, error = function(e) {
      print(paste0("ERROR:",e))
      close(file)
      cat(file.name,file=error.log,sep="\n",append = T)
      file.remove(downPath)
      senDownSearch(username,password,url,file.path,error.log,AppRoot=AppRoot,nattempts +1)
    }, finally = {
    })
  }

  if(unzip){
    message(paste0("The images have been unzipped in: ",unzipFolder))
  }else{
    message(paste0("The images have been downloaded and saved on HHD. \nFile path: ",downFolder))
  }
}
