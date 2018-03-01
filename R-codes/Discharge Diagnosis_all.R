library(stringr)
library(varhandle)

DIAGNOSES_ICD=as.matrix(DIAGNOSES_ICD)
NOTEEVENTS_csv=as.matrix(NOTEEVENTS_csv)
colnames(NOTEEVENTS_csv)<-NULL
colnames(DIAGNOSES_ICD)<-NULL
z = which(NOTEEVENTS_csv[, 7] == "Discharge summary")
q = NOTEEVENTS_csv[z, 2:3]
q = unique(q)
data = matrix(0, nrow(q), 2)
for (i in 1:nrow(q)) {
    v = which(DIAGNOSES_ICD[, 2] == q[i, 1] &
                DIAGNOSES_ICD[, 3] == q[i, 2])
    if (length(v) > 0) {
      data[i, 2] = unique(paste(DIAGNOSES_ICD[v, 6], collapse = " "))
    }
    w = which(NOTEEVENTS_csv[, 2] == q[i, 1] &
                NOTEEVENTS_csv[, 3] == q[i, 2])
    if (length(w) > 0) {
      data[i, 1] = gsub("[^A-Za-z]", " ", NOTEEVENTS_csv[w[1], 11 ])
      data[i, 1] = gsub(' +', ' ', data[i, 1])
    }
}
  d = data[-which(data[, 11] == 0), ]
  d = d[-which(d[, 11] == ""), ]
  for (i in 1:nrow(d)) {
    a = strsplit(d[i, 11], "\n\n")
    d[i, 11] = paste(a[[1]][1:length(a[[1]]) - 1], collapse = " ")
  }
  for (i in 1:nrow(d)) {
    d[i, 11] = gsub("[^A-Za-z]", " ", d[i, 11])
    d[i, 11] = gsub(' +', ' ', d[i, 11]) 
  }
  dd = d[, 11:12]
  colnames(data6) <- c("text", "label")
  write.csv(data6, "data6.csv", row.names = FALSE)
  