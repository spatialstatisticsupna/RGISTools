startUSGSsession<-function(username,
                           password,
                           cookies.file="cookies.txt",
                           verbose=F){
  c.handle = new_handle()
  handle_setopt(c.handle,
                referer=getRGISToolsOpt("USGS.url"),
                cookiejar = cookies.file,
                useragent = getRGISToolsOpt("USERAGENT"),
                followlocation = TRUE ,
                autoreferer = TRUE )

  req <- curl(getRGISToolsOpt("USGS.login"), handle = c.handle)
  html<-readLines(req)
  html<-paste(html,collapse = "\n ")
  html<-read_html(html)
  csrf<-html %>% html_nodes(xpath = '//*[@name="csrf_token"]') %>% xml_attr("value")
  if(grepl("ncforminfo",html)){
    nc<-html %>% html_nodes(xpath = '//*[@name="__ncforminfo"]') %>% xml_attr("value")
    handle_setform(c.handle,
                   'username' = username,
                   'password' = password,
                   "csrf_token"=csrf,
                   "__ncforminfo"=nc
    )
  }else{
    handle_setform(c.handle,
                   'username' = username,
                   'password' = password,
                   "csrf_token"=csrf)
  }
  req <- curl_fetch_memory(getRGISToolsOpt("USGS.login"), handle = c.handle)
  if(verbose){
    message(paste(parse_headers(req$headers),collapse="\n"))
  }
  return(c.handle)
}
