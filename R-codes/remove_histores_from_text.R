library(stringr)
discharge_indexs = which(NOTEEVENTS_csv[, 7][[1]] == "Discharge summary")
patients_id_with_discharge_text = unique(NOTEEVENTS_csv[discharge_indexs, 2:3])
number_of_patients_text = nrow(patients_id_with_discharge_text)
discharge_icd9_data = matrix(0, number_of_patients_text, 2)
for (i in 1:number_of_patients_text) {
  inexs_of_discharge_text_for_patient_i = which(
    NOTEEVENTS_csv[discharge_indexs, 2][[1]] == patients_id_with_discharge_text[i, 1][[1]] &
      NOTEEVENTS_csv[discharge_indexs, 3][[1]] == patients_id_with_discharge_text[i, 2][[1]]
  )
  inexs_of_icd9_for_patient_i = which(
    DIAGNOSES_ICD_csv[, 2][[1]] == patients_id_with_discharge_text[i, 1][[1]] &
      DIAGNOSES_ICD_csv[, 3][[1]] == patients_id_with_discharge_text[i, 2][[1]]
  )
  discharge_icd9_data[i, 1] = paste(NOTEEVENTS_csv[inexs_of_discharge_text_for_patient_i, 11][[1]] , collapse = " ")
  if (str_detect(discharge_icd9_data[i, 1], "HISTORY"))
  {
    split_text = strsplit(discharge_icd9_data[i, 1], ":")
    index_of_history_in_text = which(str_detect(split_text[[1]], "HISTORY"))
    indesis_without_history = index_of_history_in_text + 1
    data = split_text[[1]][-indesis_without_history]
    clean_data = strsplit(data, "\n")
    discharge_icd9_data[i, 1] = ""
    for (j in 1:length(clean_data))
    {
      clean_data[[j]] = clean_data[[j]][-length(clean_data[[j]])]
      data_compose = paste(clean_data[[j]], collapse = "\n")
      discharge_icd9_data[i, 1] = paste(discharge_icd9_data[i, 1] , data_compose , collapse = "\n")
    }
  }
  discharge_icd9_data[i, 1] = gsub("\n", " ", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub("\\[([^]]+)\\]", " ", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub("[#:]", " ", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub(" M ", "male", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub(" F ", "female", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub(" y ", "year old", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub(" yo ", "year old", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub(" y/o ", "year old", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub("-year-old", "year old", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub("year-old", "year old", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub("y.o.", "year old", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub("man", "male", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub("woman", "female", discharge_icd9_data[i, 1])
  discharge_icd9_data[i, 1] = gsub(" +", " ", discharge_icd9_data[i, 1])
  icd9_for_patient_i = substring(DIAGNOSES_ICD_csv[inexs_of_icd9_for_patient_i, 5][[1]], 1, 3)
  if (length(icd9_for_patient_i) > 1)
  {
    for (j in 1:length(icd9_for_patient_i))
    {
      if (str_detect(icd9_for_patient_i[j], "E"))
      {
        icd9_for_patient_i[j] = icd9_cathegiries[20, 1]
      }
      else if (str_detect(icd9_for_patient_i[j], "V")) {
        icd9_for_patient_i[j] = icd9_cathegiries[19, 1]
      }
      else
        icd9_for_patient_i[j] = icd9_cathegiries[which(
          as.numeric(icd9_for_patient_i[j]) <= as.numeric(icd9_cathegiries[1:18, 2]) &
            as.numeric(icd9_for_patient_i[j]) >= as.numeric(icd9_cathegiries[1:18, 1])
        ), 1]
    }
  }
  discharge_icd9_data[i, 2] = paste(icd9_for_patient_i,  collapse = " ")
}
colnames(discharge_icd9_data) <- c("text", "label")
write.csv(
  discharge_icd9_data,"all_discharge_with_preprocessing_without_history.csv",
  row.names = FALSE
)
