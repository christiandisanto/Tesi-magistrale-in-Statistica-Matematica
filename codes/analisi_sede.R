library("readxl")
#carichiamo il dataset 
datimiei<-as.data.frame(read_excel("../datasets/year/dati_2021_data_spesa_sede.xlsx"))

# Calcolo delle frequenze assolute
table(datimiei$Sede)

#calcolo delle frequenze relative
table(datimiei$Sede)/length(datimiei$Sede)*100

#calcolo delle frequenze assolute cumulate
cumsum(table(datimiei$Sede))

#calcolo delle frequenze relative cumulate
cumsum(table(datimiei$Sede)/length(datimiei$Sede))

#  DIAGRAMMA A TORTA
pie(table(datimiei$Sede))

#diagramma a torta con l'indicazione delle percentuali
pie(table(datimiei$Sede),labels=paste(round(proportions(table(datimiei$Sede))*100),"%",sep=""),main = "Sedi di acquisto")

#disegno del diagramma delle frequenze assolute della variabile esami
plot(table(datimiei$Sede))


#disegno del diagramma delle frequenze relative della variabile
plot(table(datimiei$Sede)/length(datimiei$Sede),col=1:25,ylab="fr. rel. acquisti in sede")

#moda
#in R non esiste una funzione predefinita, si può usare una funzione opportuna
getmode<-function(v){ 
  uniqv<-unique(v) #crea un vettore senza duplicati
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
moda<-getmode(datimiei$Sede)
moda










