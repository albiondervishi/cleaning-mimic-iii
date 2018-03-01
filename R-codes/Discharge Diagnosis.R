library(stringr)
z=which(NOTEEVENTS.csv[,7]=="Discharge summary")
q=NOTEEVENTS.csv[z,2:3]
data=matrix(0,nrow(q),12)
for(i in 1:nrow(q)){
  for(j in 1:10){
    data[i,j]=NOTEEVENTS.csv[z[i],j]
  }
  x=strsplit(NOTEEVENTS.csv[z[i],11],":")
  r=which(str_detect(tolower(x[[1]]),tolower("Discharge Diagnosis")))
  if(length(r)>0){
    if(str_detect(tolower(x[[1]][r[length(r)]+1]),tolower("Primary")))
    {
      r=r[length(r)]+2
      data[i,11]=x[[1]][r]
      if(str_detect(tolower(x[[1]][r[length(r)]]),tolower("Secondary"))){
        r=r[length(r)]+1
        data[i,11]=x[[1]][r]
      }
    }else
      {
        r=r[length(r)]+1
    data[i,11]=x[[1]][r]
    }
    }
  v=which(DIAGNOSES_ICD.csv[,2]==q[i,1] & DIAGNOSES_ICD.csv[,3]==q[i,2])
  if(length(v)>0){
    data[i,12]=paste(DIAGNOSES_ICD.csv[v,5], collapse = " ")
  }
}

d=data[-which(data[,11]==0),]
d=d[-which(d[,11]==""),]
for (i in 1:nrow(d)){
  a=strsplit(d[i,11],"\n\n")
  d[i,11]=paste(a[[1]][1:length(a[[1]])-1], collapse = " ")
}


for (i in 1:nrow(d)){
d[i,11]=gsub("[^A-Za-z]"," ",d[i,11])
d[i,11]=gsub(' +',' ',d[i,11])
}

d=d[,11:12]
## choose 1000 top of data
len=rep(0, nrow(d))
for (i in 1:nrow(d)){
  len[i]= length(str_match_all( d[i,1], "\\S+" )[[1]] )
}
z=order(len,decreasing=TRUE)
colnames(d)<-c("text","label")
write.csv(d[z[1:1000],],'data5.csv')
