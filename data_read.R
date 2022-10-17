# Reading BCAD Data

#Step 1: read field delim data

library(readxl)
install.packages("stringi")
library(data.table)


f3_fields<-read_xlsx("../WingDownload/2022 Appraisal Data Export as of 2022-09-01 s239/Appraisal Export Layout - 8.0.25.xlsx",range = "A55:F486",col_names = T)

types<-gsub(pattern = "\\s*\\([^\\)]+\\)",replacement = "",x = f3_fields$Datatype)

DT <- fread("../WingDownload/2022 Appraisal Data Export as of 2022-09-01 s239/2022 Appraisal Data Export as of 2022-09-01 s239/2022-09-07_006063_APPRAISAL_INFO.TXT", header = FALSE, sep = "\n")
DT2 <-DT[ , lapply(seq_len(length(f3_fields$Start)), function(ii) {
  stringi::stri_sub(V1, f3_fields$Start[ii], f3_fields$End[ii])
})]

DT2[,.N,by=prop_val_yr]# only one year of data available 2022

DT2[,sum(grepl(pattern = "LLC",x = py_owner_name))] # total number of records with LLC in their name

DT2[,.N,by=ht_exempt]# total number of records that have homestead exception claimed.  

