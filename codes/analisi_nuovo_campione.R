#funzione moda
#in R non esiste una funzione predefinita, si può usare una funzione opportuna
getmode<-function(v){ 
  uniqv<-unique(v) #crea un vettore senza duplicati
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

library("readxl")
#carichiamo il dataset
datimiei<-as.data.frame(read_excel("../datasets/new_sample.xlsx"))

#calcoliamo la lunghezza del campione
n<-nrow(datimiei)
n

#media
media<-round(mean(datimiei$numero_acquisti),3)
media

#moda
moda<-getmode(datimiei$numero_acquisti)
moda

#mediana
mediana<-round(median(datimiei$numero_acquisti),3)
mediana

#quantili
quantili<-quantile(datimiei$numero_acquisti)
quantili

summary(datimiei$numero_acquisti)

#INDICI DI DISPERSIONE
#varianza
varianza<-round(var(datimiei$numero_acquisti),3)
varianza

#deviazione standard
deviazionestandard<-round(sd(datimiei$numero_acquisti),3)
deviazionestandard

#coefficiente di variazione
coefdivariazione<-round(deviazionestandard/media,3)
coefdivariazione

#calcolo dell'ampiezza interquartile
diff(quantile(datimiei$numero_acquisti,probs = c(0.25,0.75)))

#INDICI DI FORMA
library(moments)

#calcolo dell'asimmetria o skewness
skw<-round(skewness(datimiei$numero_acquisti),3)
skw

#calcolo della curtosi
curt<-round(kurtosis(datimiei$numero_acquisti),3)
curt