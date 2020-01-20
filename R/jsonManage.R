toEspaJSON<-function(json_list,is.array=c("products","inputs")){
  nam<-names(json_list)
  resjson<-'{'
  for(n in 1:length(nam)){
    resjson<-paste0(resjson,'"',nam[n],'":')
    nlist<-json_list[[n]]
    if(class(nlist)=='list'){
      resjson<-paste0(resjson,toEspaJSON(nlist))
    }else if(length(nlist)>1|nam[n]%in%is.array){
      resjson<-paste0(resjson,'[')
      for(x in nlist){
        if(is.na(x)){
          resjson<-paste0(resjson,'null,')
        }else{
          resjson<-paste0(resjson,'"',x,'",')
        }
      }
      resjson<-paste0(resjson,']')
    }else if(length(nlist)==1){
      if(is.na(nlist)){
        resjson<-paste0(resjson,'null')
      }else{
        resjson<-paste0(resjson,'"',nlist,'"')
      }
    }
    resjson<-paste0(resjson,",")
  }
  resjson<-paste0(resjson,'}')
  resjson<-gsub(",]","]",resjson)
  resjson<-gsub(",}","}",resjson)
  return(resjson)
}

