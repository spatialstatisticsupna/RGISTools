lsEspaCreateConnection<-function(username,password){
  c.handle = new_handle()
  handle_setopt(c.handle,
                referer=getRGISToolsOpt("LS.ESPA.API"),
                useragent = getRGISToolsOpt("USERAGENT"),
                followlocation = TRUE,
                autoreferer = TRUE,
                username=username,
                password=password)
  return(c.handle)
}