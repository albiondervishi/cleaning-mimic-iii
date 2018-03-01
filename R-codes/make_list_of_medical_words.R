der2_Refset_ICD10CM_US_20170901_1_ <-
  read_delim(
    "/media/sepideh/e1cc3a3a-aab5-4e10-8820-63c2942c104b/der2_Refset_ICD10CM_US_20170901 (1).zip",
    +"\t",
    escape_double = FALSE,
    trim_ws = TRUE
  )
list_texts_of_culomn9 = der2_Refset_ICD10CM_US_20170901_1_[, 9][[1]]
list_texts_of_culomn10 = der2_Refset_ICD10CM_US_20170901_1_[, 10][[1]]

list_of_medical_words = NULL

for (i in 1:nrow(der2_Refset_ICD10CM_US_20170901_1_))
{
  list_texts_of_culomn9[i] = tolower(gsub('[[:punct:] ]+', ' ', list_texts_of_culomn9[i]))
  list_texts_of_culomn10[i] = tolower(gsub('[[:punct:] ]+', ' ', list_texts_of_culomn10[i]))
  list_texts_of_culomn9[i] = tolower(trimws(gsub(
    "\\w*[0-9]+\\w*\\s*", "", list_texts_of_culomn9[i]
  )))
  list_texts_of_culomn10[i] = tolower(trimws(gsub(
    "\\w*[0-9]+\\w*\\s*", "", list_texts_of_culomn10[i]
  )))
  words9 = strsplit(list_texts_of_culomn9[i], " ")[[1]]
  words10 = strsplit(list_texts_of_culomn10[i], " ")[[1]]
  index_of_words9 = which(!hunspell_check(words9))
  index_of_words10 = which(!hunspell_check(words10))
  list_of_medical_words = unique(c(list_of_medical_words,
                                   words9[index_of_words9],
                                   words10[index_of_words10]))
}

write.csv(
  list_of_medical_words,
  "words_for_search_in_wikipedia.txt",
  row.names = FALSE
)
