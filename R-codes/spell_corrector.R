

make_dictionary_mimic_icd9_discription_frequency <-
  function(d_icd_diagnoses,
           noteevents)
  {
    library(stringr)
    i=1
    num=length(which(str_detect(d_icd_diagnoses[i,2],discharge_notes_with_icd[,2])))
    words=strsplit( gsub(" +", " ", gsub("[^A-Za-z]", " ", d_icd_diagnoses[i,4]))," ")[[1]]
    dictionary=data.frame(words,rep(num,length(words)))
    for(i in 2:nrow(d_icd_diagnoses))
  }


make_dictionary_medical_corpus_frequency <- function(docs)
{
  
}


combine_dictionaries <- function()
{
  
}


find_incorrect_words <- function()
{
  
}


mapping_incorrect_word_with_dictionary <- function()
{
  
}


mapping_incorrect_word_with_MetaMap <- function()
{
  
}


mapping_incorrect_word_with_hunspell_suggestion <- function()
{
  
}


replace_incorrect_with_correct_in_texts <- function()
{
  
}
 

discharge_notes_with_icd = make_discharge_notes_with_icd(NOTEEVENTS.csv, DIAGNOSES_ICD.csv)
gc()
dictionary_mimic_icd9_discription_frequency <-
  make_dictionary_mimic_icd9_discription_frequency(D_ICD_DIAGNOSES_csv, NOTEEVENTS.csv)
