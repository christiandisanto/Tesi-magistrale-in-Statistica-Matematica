library("readxl")
#carichiamo il dataset 
datimiei<-as.data.frame(read_excel("dati_2021_data_spesa_sede.xlsx"))
#calcoliamo la lunghezza del campione
n<-nrow(datimiei)
n

#PRINCIPALI INDICI DI POSIZIONE
#mediana
media<-mean(datimiei$Prezzo)
media

#moda
#in R non esiste una funzione predefinita, si può usare una funzione opportuna
getmode<-function(v){ 
uniqv<-unique(v) #crea un vettore senza duplicati
uniqv[which.max(tabulate(match(v,uniqv)))]
}
 
moda<-getmode(datimiei$Prezzo)
moda

#mediana
mediana<-median(datimiei$Prezzo)
mediana

#quantili
quantili<-quantile(datimiei$Prezzo)
quantili

summary(datimiei$Prezzo)

#INDICI DI DISPERSIONE
#varianza
varianza<-var(datimiei$Prezzo)
varianza

#deviazione standard
deviazionestandard<- sd(datimiei$Prezzo)
deviazionestandard

#coefficiente di variazione
coefdivariazione<-deviazionestandard/media
coefdivariazione

#calcolo dell'ampiezza interquartile
diff(quantile(datimiei$Prezzo,probs = c(0.25,0.75)))

#INDICI DI FORMA
library(moments)

#calcolo dell'asimmetria o skewness
skw<-skewness(datimiei$Prezzo)
skw

#calcolo della curtosi
curt<-kurtosis(datimiei$Prezzo)
curt

#coefficiente di Pearson
sigma<-sqrt((n-1)*var(datimiei$Prezzo)/n)
asimpearson<-3*(media-mediana)/sigma
asimpearson


#frequenze assolute
table(cut(datimiei$Prezzo,breaks=c(0,50,150,350,10600)))

#frequenze relative
round(table(cut(datimiei$Prezzo,breaks=c(0,50,150,350,10600)))/length(datimiei$Prezzo),digits = 2)

#istogramma
hist(datimiei$Prezzo,breaks=c(0,50,150,350,10600),main="Istogramma")

#CALCOLO DEL BOX PLOT

#calcolo dei cardini 
cardine1<-quantile(datimiei$Prezzo)[[2]]-1.5*(quantile(datimiei$Prezzo)[[4]]-quantile(datimiei$Prezzo)[[2]])
cardine1
cardine2<-quantile(datimiei$Prezzo)[[4]]+1.5*(quantile(datimiei$Prezzo)[[4]]-quantile(datimiei$Prezzo)[[2]])
cardine2

#disegno del box plot
boxplot(datimiei$Prezzo,xlab="prezzi",col="red",ylim=c(0,300))
#le estremità della scatola sono in corrispondenza di Q1,Q2 e Q3, mentre la linea centrale è Q2.
#Il primo baffo ha la lunghezza data dalla differenza tra Q1 ed il minimo (del campione privo di outliers)
#Il secondo baffo ha lunghezza data dalla differenza tra il massimo (del campione privo di outliers) e Q3.


