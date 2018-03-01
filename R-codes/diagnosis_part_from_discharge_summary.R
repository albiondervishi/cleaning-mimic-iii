library(stringr)
discharge_indexs = which(NOTEEVENTS_csv[, 7][[1]] == "Discharge summary")
patients_id_with_discharge_text = unique(NOTEEVENTS_csv[discharge_indexs, 2:3])
number_of_patients_text = nrow(patients_id_with_discharge_text)
discharge_icd9_data = matrix('', number_of_patients_text, 2)
for (i in 1:number_of_patients_text) {
  data_compose = ""
  inexs_of_discharge_text_for_patient_i = which(
    NOTEEVENTS_csv[discharge_indexs, 2][[1]] == patients_id_with_discharge_text[i, 1][[1]] &
      NOTEEVENTS_csv[discharge_indexs, 3][[1]] == patients_id_with_discharge_text[i, 2][[1]]
  )
  inexs_of_icd9_for_patient_i = which(
    DIAGNOSES_ICD_csv[, 2][[1]] == patients_id_with_discharge_text[i, 1][[1]] &
      DIAGNOSES_ICD_csv[, 3][[1]] == patients_id_with_discharge_text[i, 2][[1]]
  )
  
  
  data = paste(NOTEEVENTS_csv[inexs_of_discharge_text_for_patient_i, 11][[1]] , collapse = " ")
  new_data = gsub("primary:", " ", str_to_lower(data[[1]]))
  new_data = gsub("secondary:", " ", new_data)
  split_text = strsplit(new_data, ":")
  index_of_diagnosis_in_text = which(str_detect(split_text[[1]], "diagnosis"))
  if (length(index_of_diagnosis_in_text) > 0)
  { data_all = ""
    for(j in 1:length(index_of_diagnosis_in_text))
    {
    data = split_text[[1]][index_of_diagnosis_in_text[j] + 1]
    clean_data = strsplit(data, "\n")
    clean_data[[1]] = clean_data[[1]][-length(clean_data[[1]])]
    data_all = c(data_all, clean_data[[1]])
    }
  data_compose = paste(data_all, collapse = "\n")
  }
  index_of_history_of_present_illness_in_text = which(str_detect(split_text[[1]], "history of present illness"))
  if (length(index_of_history_of_present_illness_in_text) > 0)
  {
    data_all = ""
    for(j in 1:length(index_of_history_of_present_illness_in_text))
    {
      data = split_text[[1]][index_of_history_of_present_illness_in_text[j] + 1]
      clean_data = strsplit(data, "\n")
      clean_data[[1]] = clean_data[[1]][-length(clean_data[[1]])]
      data_all = c(data_all, clean_data[[1]])
    }
    new_data_compose = paste(data_all, collapse = "\n")
  }
  discharge_icd9_data [i, 1] = paste(data_compose, new_data_compose, collapse = " ")
  discharge_icd9_data[i, 1] = gsub("\n", " ", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub("\\[([^]]+)\\]", "UNKN", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub("[#:]", " ", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub(" +", " ", discharge_icd9_data[i, 1])
  icd9_for_patient_i = substring(DIAGNOSES_ICD_csv[inexs_of_icd9_for_patient_i, 5][[1]], 1, 3)
  if (length(icd9_for_patient_i) > 1)
  {
    for (j in 1:length(icd9_for_patient_i))
    {
      if (str_detect(icd9_for_patient_i[j], "E"))
      {
        icd9_for_patient_i[j] = icd9_cathegiries[19, 1]
      }
      else if (str_detect(icd9_for_patient_i[j], "V")) {
        icd9_for_patient_i[j] = icd9_cathegiries[18, 1]
      }
      else{
        index = which(
          as.numeric(icd9_for_patient_i[j]) <= as.numeric(icd9_cathegiries[1:17, 2]) &
            as.numeric(icd9_for_patient_i[j]) >= as.numeric(icd9_cathegiries[1:17, 1])
        )
        if(length(index)>0){
          icd9_for_patient_i[j] = icd9_cathegiries[index, 1]
        }
      }
    }
    discharge_icd9_data[i, 2] = paste(unique(icd9_for_patient_i),  collapse = " ")
    discharge_icd9_data[i, 2] = trimws(discharge_icd9_data[i, 2])
  }
}
data = discharge_icd9_data
for(i in 1:nrow(data))
{
 if(length(strsplit( data[i,1], ' ')[[1]])< 20)
   data = data[-i,]
}
data = data[-which(data[,2]==''),]
colnames(data) <- c("text", "label")
write.csv(data, "data.csv", row.names = FALSE)
