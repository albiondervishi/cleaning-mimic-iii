NOTEEVENTS[,11]=gsub(","," ",NOTEEVENTS[,11])
NOTEEVENTS[,11]=gsub("\n"," ",NOTEEVENTS[,11])
NOTEEVENTS=NOTEEVENTS[,-c(5,6)]
write.csv(NOTEEVENTS, "D:\\NOTEEVENTS.csv", row.names=FALSE)
t=table(DIAGNOSES_ICD.csv[,5])
hx=unique(DIAGNOSES_ICD.csv[,5])
x=as.numeric(t[hx])
hx=hx[order(x)]
x=x[order(x)]
x=x[1:length(x)-1]
hx=hx[1:length(hx)-1]
plot(x, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of ICD-9 Distributions")
labels=hx
dignoses_ICD=DIAGNOSES_ICD.csv[ which(DIAGNOSES_ICD.csv[,5]%in%labels),]
sid=unique(dignoses_ICD [,2])
hid=unique(dignoses_ICD[,3])


t=table(PROCEDURES_ICD.csv[,5])
hx=unique(PROCEDURES_ICD.csv[,5])
x=unname(t)
hx=hx[order(x)]
x=x[order(x)]
x=x[1:length(x)-1]
hx=hx[1:length(hx)-1]
plot(x, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of ICD-9 Distributions")
labels=hx
procedures_ICD=PROCEDURES_ICD.csv[ which(PROCEDURES_ICD.csv[,5]%in%labels),]
sid=unique(procedures_ICD [,2])
hid=unique(PROCEDURES_ICD.csv[,3])
# all category text with icd-9-diagnoses with 126 lables
index=1
data=matrix("",length(hid),2)
for(i in 1:length(hid)){
  s=which(dignoses_ICD[,3]==hid[i])
  if(length(s)>0){
    sid=dignoses_ICD[which(dignoses_ICD[,3]==hid[i]),2]
    ssid=unique(sid)
    for(j in 1:length(ssid)){
      txt=NOTEEVENTS[which(NOTEEVENTS[,2]==ssid[j] & NOTEEVENTS[,3]==hid[i]),9]
      icd_dia=dignoses_ICD[which(dignoses_ICD[,2]==ssid[j] & dignoses_ICD[,3]==hid[i]),5]
      d=paste(txt,collapse=" ")
      data[index,1]=tolower( gsub("[^A-Za-z0-9]"," ",d))
      data[index,2]=paste(icd_dia,collapse=" ")
      index=index+1
    }
  }
}
colnames(data)<-c("text","label")
write.csv(data[1:1000,],"mimic_sample1000.csv",row.names = FALSE)
write.csv(data,"mimic_icd_9_diagnoses.csv",row.names = FALSE)


# discharge text with icd-9-diagnoses
index=index
data_discharge=matrix("",length(hid),2)
for(i in i:length(hid)){
  s=which(dignoses_ICD[,3]==hid[i])
  if(length(s)>0){
    sid=dignoses_ICD[which(dignoses_ICD[,3]==hid[i]),2]
    ssid=unique(sid)
    for(j in 1:length(ssid)){
      txt=NOTEEVENTS[which(NOTEEVENTS[,2]==ssid[j] & NOTEEVENTS[,3]==hid[i] & NOTEEVENTS[,5]=="Discharge summary"),9]
      icd_dia=dignoses_ICD[which(dignoses_ICD[,2]==ssid[j] & dignoses_ICD[,3]==hid[i]),5]
      d=paste(txt,collapse=" ")
      data_discharge[index,1]=tolower( gsub("[^A-Za-z0-9]"," ",d))
      data_discharge[index,2]=paste(icd_dia,collapse=" ")
      index=index+1
    }
  }
}
colnames(data_discharge)<-c("text","label")
write.csv(data_discharge[1:1000,],"mimic_discharge1000.csv",row.names = FALSE)
write.csv(data_discharge,"mimic_icd_9_diagnoses_discharge.csv",row.names = FALSE)


# Physician with icd-9-diagnoses
Physician_i=1
Nursing_i=1
Nutrition_i=1
Consult_i=1
data_Consult=matrix("",length(hid),2)
data_Nutrition=matrix("",length(hid),2)
data_Physician=matrix("",length(hid),2)
data_Nursing=matrix("",length(hid),2)
for(i in 1:length(hid)){
  s=which(dignoses_ICD[,3]==hid[i])
  if(length(s)>0){
    sid=dignoses_ICD[which(dignoses_ICD[,3]==hid[i]),2]
    ssid=unique(sid)
    for(j in 1:length(ssid)){
      txt=NOTEEVENTS[which(NOTEEVENTS[,2]==ssid[j] & NOTEEVENTS[,3]==hid[i] & NOTEEVENTS[,5]=="Physician "),9]
      icd_dia=dignoses_ICD[which(dignoses_ICD[,2]==ssid[j] & dignoses_ICD[,3]==hid[i]),5]
      d=paste(txt,collapse=" ")
      if(d!="")
        {
        data_Physician[Physician_i,1]=tolower( gsub("[^A-Za-z0-9]"," ",d))
        data_Physician[Physician_i,2]=paste(icd_dia,collapse=" ")
        Physician_i=Physician_i+1
      }
      txt=NOTEEVENTS[which(NOTEEVENTS[,2]==ssid[j] & NOTEEVENTS[,3]==hid[i] & NOTEEVENTS[,5]=="Nursing"),9]
      icd_dia=dignoses_ICD[which(dignoses_ICD[,2]==ssid[j] & dignoses_ICD[,3]==hid[i]),5]
      d=paste(txt,collapse=" ")
      if(d!="")
      {
        data_Nursing[Nursing_i,1]=tolower( gsub("[^A-Za-z0-9]"," ",d))
        data_Nursing[Nursing_i,2]=paste(icd_dia,collapse=" ")
        Nursing_i=Nursing_i+1
      }
      txt=NOTEEVENTS[which(NOTEEVENTS[,2]==ssid[j] & NOTEEVENTS[,3]==hid[i] & NOTEEVENTS[,5]=="Nutrition"),9]
      icd_dia=dignoses_ICD[which(dignoses_ICD[,2]==ssid[j] & dignoses_ICD[,3]==hid[i]),5]
      d=paste(txt,collapse=" ")
      if(d!="")
      {
        data_Nutrition[Nutrition_i,1]=tolower( gsub("[^A-Za-z0-9]"," ",d))
        data_Nutrition[Nutrition_i,2]=paste(icd_dia,collapse=" ")
        Nutrition_i=Nutrition_i+1
      }
      txt=NOTEEVENTS[which(NOTEEVENTS[,2]==ssid[j] & NOTEEVENTS[,3]==hid[i] & NOTEEVENTS[,5]=="Consult"),9]
      icd_dia=dignoses_ICD[which(dignoses_ICD[,2]==ssid[j] & dignoses_ICD[,3]==hid[i]),5]
      d=paste(txt,collapse=" ")
      if(d!="")
      {
        data_Consult[Consult_i,1]=tolower( gsub("[^A-Za-z0-9]"," ",d))
        data_Consult[Consult_i,2]=paste(icd_dia,collapse=" ")
        Consult_i=Consult_i+1
      }
    }
  }
}
colnames(data_Physician)<-c("text","label")
write.csv(data_Physician[1:1000,],"mimic_Physician1000.csv",row.names = FALSE)
write.csv(data_Physician[1:10000,],"mimic_Physician10000.csv",row.names = FALSE)
write.csv(data_Physician,"mimic_icd_9_diagnoses_Physician.csv",row.names = FALSE)

colnames(data_discharge)<-c("text","label")
write.csv(data_discharge[1:1000,],"mimic_Physician1000.csv",row.names = FALSE)
write.csv(data_discharge[1:10000,],"mimic_Physician10000.csv",row.names = FALSE)
write.csv(data_discharge,"mimic_icd_9_diagnoses_Physician.csv",row.names = FALSE)

colnames(data_discharge)<-c("text","label")
write.csv(data_discharge[1:1000,],"mimic_Physician1000.csv",row.names = FALSE)
write.csv(data_discharge[1:10000,],"mimic_Physician10000.csv",row.names = FALSE)
write.csv(data_discharge,"mimic_icd_9_diagnoses_Physician.csv",row.names = FALSE)

colnames(data_discharge)<-c("text","label")
write.csv(data_discharge[1:1000,],"mimic_Physician1000.csv",row.names = FALSE)
write.csv(data_discharge[1:10000,],"mimic_Physician10000.csv",row.names = FALSE)
write.csv(data_discharge,"mimic_icd_9_diagnoses_Physician.csv",row.names = FALSE)
