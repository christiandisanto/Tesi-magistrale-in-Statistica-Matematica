library("readxl")
#carichiamo il dataset 
datimiei<-as.data.frame(read_excel("dati_2021_data_spesa_sede.xlsx"))

# Calcolo delle frequenze assolute
table(datimiei$Data)
mydates <- as.Date(c("2021-01-01", "2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01","2021-08-01","2021-09-01","2021-10-01","2021-11-01","2021-12-01"))
table(cut(datimiei$Data,breaks=mydates))

