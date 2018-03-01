discharge_indexs = which(NOTEEVENTS_csv[, 7] == "Discharge summary")
patients_id_with_discharge_text = unique(NOTEEVENTS_csv[discharge_indexs, 2:3])
number_of_patients_text = nrow(patients_id_with_discharge_text)
discharge_icd9_data = matrix(0, number_of_patients_text, 2)
for (i in 1:number_of_patients_text) {
  inexs_of_discharge_text_for_patient_i = which(
    NOTEEVENTS_csv[discharge_indexs, 2] == patients_id_with_discharge_text[i, 1] &
      NOTEEVENTS_csv[discharge_indexs, 3] == patients_id_with_discharge_text[i, 2]
  )
  inexs_of_icd9_for_patient_i = which(
    DIAGNOSES_ICD_csv[, 2] == patients_id_with_discharge_text[i, 1] &
      DIAGNOSES_ICD_csv[, 3] == patients_id_with_discharge_text[i, 2]
  )
  discharge_icd9_data[i,1] = paste(NOTEEVENTS_csv[inexs_of_discharge_text_for_patient_i,11] , collapse = " ")
  discharge_icd9_data[i,1] = gsub("\n"," ",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub("\\[([^]]+)\\]"," ",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub("[A-Za-z]+ [A-Za-z]+:"," ",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub("[A-Za-z]+:"," ",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub("[0-9]+"," ",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub("[#:]"," ",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub(" M ","male",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub(" F ","female",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub(" y ","year old",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub(" yo ","year old",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub(" y/o ","year old",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub("-year-old","year old",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub("year-old","year old",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub("y.o.","year old",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub("man","male",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub("woman","female",discharge_icd9_data[i,1])
  discharge_icd9_data[i,1] = gsub(" +"," ",discharge_icd9_data[i,1])
  icd9_for_patient_i = substring(DIAGNOSES_ICD_csv[inexs_of_icd9_for_patient_i,5],1,3)
  discharge_icd9_data[i,2] = paste(icd9_for_patient_i,  collapse = " ")
}
colnames(discharge_icd9_data)<-c("text","label")
write.csv(
  discharge_icd9_data,
  "/home/sepideh/Documents/data/all_discharge_with_preprocessing_first_three_codes.csv",
  row.names = FALSE
)
