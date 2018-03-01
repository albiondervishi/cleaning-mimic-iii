make_documents_for_w2v_model <-
  function(d_icd_diagnoses, icd_top_level)
  {
    #/home/sepideh/workshop/hamster/code/Data/mimic
    num_row = nrow(icd_top_level) - 2
    top_codes = substr(D_ICD_DIAGNOSES_csv[, 2][[1]], 1, 3)
    for (i in 1:num_row)
    {
      documenti = paste(unique(d_icd_diagnoses[!is.na(which(
        as.numeric(top_codes) >= as.numeric(icd_top_level[i, 1]) &
          as.numeric(top_codes) < as.numeric(icd_top_level[i, 2])
      )), 4]), collapse = " ")
      documenti=gsub(' +', ' ', documenti)
      documenti=gsub("[^A-Za-z]", ' ', documenti)
      write.table(
        documenti,
        paste(
          "/home/sepideh/workshop/hamster/code/Data/mimic/data",
          i,
          ".txt",sep=""
        )
        ,
        sep = ""
      )
    }
    documenti = paste(unique(d_icd_diagnoses[!is.na(which(str_detect(d_icd_diagnoses[, 2], "V"))), 4]), collapse = " ")
    documenti=gsub(' +', ' ', documenti)
    documenti=gsub("[^A-Za-z]", ' ', documenti)
    write.table(
      documenti,
      paste(
        "/home/sepideh/workshop/hamster/code/Data/mimic/data",
        i + 1,
        ".txt",sep=""
      )
      ,
      sep = ""
    )
    
    
    documenti = paste(unique(d_icd_diagnoses[!is.na(which(str_detect(d_icd_diagnoses[, 2], "E"))), 4]), collapse = " ")
    documenti=gsub(' +', ' ', documenti)
    documenti=gsub("[^A-Za-z]", ' ', documenti)
    write.table(
      documenti,
      paste(
        "/home/sepideh/workshop/hamster/code/Data/mimic/data",
        i + 2,
        ".txt",sep=""
      )
      ,
      sep = ""
    )
  }


make_icd_top_level <- function()
{
  icd_top_level = matrix(0, 20, 2)
  icd_top_level[, 1] = c(
    '001',
    '140',
    '240',
    '280',
    '290',
    '320',
    '360',
    '390',
    '460',
    '520',
    '580',
    '630',
    '680',
    '710',
    '740',
    '760',
    '780',
    '800',
    'e800',
    'v01'
  )
  icd_top_level[, 2] = c(
    '139',
    '239',
    '279',
    '289',
    '319',
    '359',
    '389',
    '459',
    '519',
    '579',
    '629',
    '679',
    '709',
    '739',
    '759',
    '779',
    '799',
    '999',
    'e999',
    'v91'
  )
  return(icd_top_level)
}


icd_top_level = make_icd_top_level()
documents_for_w2v_model = make_documents_for_w2v_model(D_ICD_DIAGNOSES_csv, icd_top_level)
