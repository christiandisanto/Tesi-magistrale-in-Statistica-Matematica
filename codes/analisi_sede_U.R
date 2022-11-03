#funzione moda
#in R non esiste una funzione predefinita, si può usare una funzione opportuna
getmode<-function(v){ 
  uniqv<-unique(v) #crea un vettore senza duplicati
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
library("readxl")

#carichiamo il dataset 
datimiei<-as.data.frame(read_excel("../datasets/locals/sede_U.xlsx"))
#calcoliamo la lunghezza del campione
n<-nrow(datimiei)
n

#ANALISI DEL PREZZO
#PRINCIPALI INDICI DI POSIZIONE
#media
media<-round(mean(datimiei$Prezzo),3)
media

#moda
moda<-round(getmode(datimiei$Prezzo),3)
moda

#mediana
mediana<-round(median(datimiei$Prezzo),3)
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
FREQ<-table(cut(datimiei$Prezzo,breaks=c(0,50,150,350,10600)))
FREQ
#frequenze relative 
round(table(cut(datimiei$Prezzo,breaks=c(0,50,150,350,10600)))/length(datimiei$Prezzo),digits = 2)

par(mfrow=c(1,2))
#DISEGNO ISTOGRAMMA E BOX PLOT
#hist(datimiei$Prezzo,breaks=c(0,10,30,50,70,90,110,200,300,400,500,600,1000,2000,3000,4000,5000,10600),main="Istogramma",xlab="prezzo",ylab ="intensità")
hist(datimiei$Prezzo,breaks=c(0,50,150,350,10600),main="Istogramma",xlab="prezzo",ylab ="intensità")
boxplot(datimiei$Prezzo,main="Box plot",ylab="prezzo",col="red",ylim=c(0,300),horizontal = TRUE)
par(mfrow=c(1,1))

#Per quanto riguarda il box plot è bene osservare anche i valori dei cardini 
#calcolo dei cardini 
cardine1<-quantile(datimiei$Prezzo)[[2]]-1.5*(quantile(datimiei$Prezzo)[[4]]-quantile(datimiei$Prezzo)[[2]])
cardine1
cardine2<-quantile(datimiei$Prezzo)[[4]]+1.5*(quantile(datimiei$Prezzo)[[4]]-quantile(datimiei$Prezzo)[[2]])
cardine2


