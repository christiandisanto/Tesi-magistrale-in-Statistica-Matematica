#In questo script vogliamo fare un'analisi sull'intero dataset relativo alle spese effettuate nel 2021
#in particolare per ogni feature cercheremo di trarre delle informazioni utili 
#########################

#funzione moda
#in R non esiste una funzione predefinita, si può usare una funzione opportuna
getmode<-function(v){ 
  uniqv<-unique(v) #crea un vettore senza duplicati
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

library("readxl")

#carichiamo il dataset 
datimiei<-as.data.frame(read_excel("../datasets/year/dati_2021_data_spesa_sede.xlsx"))
#calcoliamo la lunghezza del campione
n<-nrow(datimiei)
n

#ANALISI TEMPORALE
mese<-c("gennaio","febbraio","marzo","aprile","maggio","giugno","luglio",
        "agosto","settembre","ottobre","novembre","dicembre")
freqass<-c(784,904,841,743,664,952,960,684,1061,1041,802,1062)
freqrelative<-prop.table(freqass)
periodo<-data.frame(Mesi=mese,Frequenze_assolute=freqass,Frequenze_relative=round(freqrelative,3),
           Percentuali=round((freqrelative*100),1))
periodo
barplot(periodo$Frequenze_assolute, xlab = "mesi", ylab = "numero di acquisti",col = "blue")

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


#DISEGNO ISTOGRAMMA E BOX PLOT
hist(datimiei$Prezzo,breaks=c(0,2,5,10,20,30,40,50,100,200,300,400,500,2000,4000,10600),main="Istogramma",xlab="prezzo",ylab ="intensità")
boxplot(datimiei$Prezzo,main="Box plot",xlab="prezzo",col="red",ylim=c(0,300),horizontal = TRUE)


#Per quanto riguarda il box plot è bene osservare anche i valori dei cardini 
#calcolo dei cardini 
cardine1<-quantile(datimiei$Prezzo)[[2]]-1.5*(quantile(datimiei$Prezzo)[[4]]-quantile(datimiei$Prezzo)[[2]])
cardine1
cardine2<-quantile(datimiei$Prezzo)[[4]]+1.5*(quantile(datimiei$Prezzo)[[4]]-quantile(datimiei$Prezzo)[[2]])
cardine2

#ANALISI SEDI
# Calcolo delle frequenze assolute
freq_assolute<-table(datimiei$Sede)
freq_assolute

#calcolo delle frequenze relative percentuali
frequenze_relative_perc<-round(table(datimiei$Sede)/length(datimiei$Sede)*100,digits = 2)
frequenze_relative_perc

#disegno del diagramma delle frequenze assolute e relative
par(mfrow=c(1,2))
plot(table(datimiei$Sede),ylab ="fr. assolute acquisti in sede")
plot(frequenze_relative_perc,ylab="Percentuali acquisti in sede")
par(mfrow=c(1,1))


#moda
moda<-getmode(datimiei$Sede)
moda




