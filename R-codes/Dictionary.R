Dictionary<-function(documents){
  k=""
  for(i in 1:nrow(documents)){
    d=tolower( gsub("[^A-Za-z]"," ",documents[i,1][[1]]))
    d=gsub(" +"," ",d)
    t1=table(strsplit(d, " "))
  a1 <- as.data.frame(t1)
  colnames(a1) <- c('word','freq')
  if(i==1)
    {
  k <- as.data.frame(t1)
  colnames(k) <- c('word','freq')
  }
  if(i>1)
    {
    k=merge(k,a1,by = "word",all = TRUE)
    k[which(is.na( k[,2])),2]=0
    k[which(is.na( k[,3])),3]=0
    k[,2]=k[,2]+k[,3]
    k=k[,-3]
  }
  }
  return( k)
}

#documents are web_scraping data sets