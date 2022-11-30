#funzione moda
#in R non esiste una funzione predefinita, si può usare una funzione opportuna
getmode<-function(v){ 
  uniqv<-unique(v) #crea un vettore senza duplicati
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

library("readxl")
#carichiamo il dataset 
datimiei<-as.data.frame(read_excel("../datasets/months/12_dicembre.xlsx"))
#calcoliamo la lunghezza del campione
n<-nrow(datimiei)
n

#ANALISI DEL PREZZO
#PRINCIPALI INDICI DI POSIZIONE
#media
media<-mean(datimiei$Prezzo)
media

#moda
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
table(cut(datimiei$Prezzo,breaks=c(-1,2,5,10,20,30,40,50,100,200,300,400,500,2000,4000,10600)))

#frequenze relative 
round(table(cut(datimiei$Prezzo,breaks=c(-1,2,5,10,20,30,40,50,100,200,300,400,500,2000,4000,10600)))/length(datimiei$Prezzo),digits = 2)


#DISEGNO ISTOGRAMMA E BOX PLOT
hist(datimiei$Prezzo,breaks=c(-1,2,5,10,20,30,40,50,100,200,300,400,500,2000,4000,10600),main="Istogramma",xlab="prezzo",ylab ="intensità")
boxplot(datimiei$Prezzo,main="Box plot",ylab="prezzo",col="red",ylim=c(0,300),horizontal = TRUE)


#Per quanto riguarda il box plot è bene osservare anche i valori dei cardini 
#calcolo dei cardini 
cardine1<-quantile(datimiei$Prezzo)[[2]]-1.5*(quantile(datimiei$Prezzo)[[4]]-quantile(datimiei$Prezzo)[[2]])
cardine1
cardine2<-quantile(datimiei$Prezzo)[[4]]+1.5*(quantile(datimiei$Prezzo)[[4]]-quantile(datimiei$Prezzo)[[2]])
cardine2

#ANALISI SEDI
# Calcolo delle frequenze assolute
table(datimiei$Sede)

#calcolo delle frequenze relative percentuali
frequenze_relative_perc<-table(datimiei$Sede)/length(datimiei$Sede)*100
frequenze_relative_perc

#disegno del diagramma delle frequenze assolute e relative
par(mfrow=c(1,2))
plot(table(datimiei$Sede),ylab ="fr. assolute acquisti in sede")
plot(frequenze_relative_perc,ylab="fr. rel. percentuali acquisti in sede")
par(mfrow=c(1,1))

#moda
moda<-getmode(datimiei$Sede)
moda

#TABELLA DI CONTINGENZA 
classiprezzo<-c(cut(datimiei$Prezzo,breaks = c(0,500,2000,4000,10000)))
fclassiprezzo<-factor(classiprezzo)
levels(fclassiprezzo)<-c("(0-500]","(500-2000]","(2000-4000]","(4000-10000]")
contingenza<-table(fclassiprezzo,datimiei$Sede)
addmargins(contingenza)