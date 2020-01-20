genAskForYN<-function(question,cont=0,n.attempts=5,omit.question=FALSE){
  str = tolower(readline(question))
  if(omit.question){return(TRUE)}
  if(cont<n.attempts){
    switch(str,
           y={
             return(TRUE)
           },
           yes={
             return(TRUE)
           },
           n={
             return(FALSE)
           },
           no={
             return(FALSE)
           },
           {
             return(genAskForYN(question,cont+1))
           }
    )
  }else{
    message("No answering question, stoping process...")
  }
}




