library(stringr)
x="001"
number_of_patients_text=nrow(train)
discharge_icd9_data = matrix(0, number_of_patients_text, 2)
for (ii in 1:number_of_patients_text) {
  icd=strsplit(train[ii,2][[1]],"\n")[[1]]
  k=which(substring(icd,1,1)==".")
  if(length(k)>0){
    icd[k]=substring(icd[k],2,4)}
  icd9_for_patient_i = substring(icd,1,3)
  icd9_for_patient_i = icd9_for_patient_i[-which(icd9_for_patient_i=="")]
  if(!is.na(icd9_for_patient_i))
  {
    for(j in 1:length(icd9_for_patient_i))
    {
      if(str_detect(icd9_for_patient_i[j], "E"))
      {
        icd9_for_patient_i[j] = icd9_cathegiries[20,1]
      }
      else if(str_detect(icd9_for_patient_i[j], "V")){
        icd9_for_patient_i[j] = icd9_cathegiries[19,1]
      }
      else
        icd9_for_patient_i[j] = icd9_cathegiries[which(as.numeric(icd9_for_patient_i[j])<= as.numeric(icd9_cathegiries[1:18,2]) &  as.numeric(icd9_for_patient_i[j]) >= as.numeric(icd9_cathegiries[1:18,1])),1]
    }
  }
  x=unique(c(x,icd9_for_patient_i))
  discharge_icd9_data[ii,2] = paste(icd9_for_patient_i,  collapse = " ")
  discharge_icd9_data[ii,1] = train[ii,1][[1]]
}
colnames(discharge_icd9_data)<-c("text","label")
write.csv(
  discharge_icd9_data,
  "train.csv",
  row.names = FALSE
)
