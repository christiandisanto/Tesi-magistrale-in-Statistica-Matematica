# Calcolo del box plot
library("readxl")
#carichiamo il dataset 
datimiei<-as.data.frame(read_excel("dati_2021_data_spesa_sede.xlsx"))

#calcolo dei cardini 
cardine1<-quantile(datimiei$Prezzo)[[2]]-1.5*(quantile(datimiei$Prezzo)[[4]]-quantile(datimiei$Prezzo)[[2]])
cardine1
cardine2<-quantile(datimiei$Prezzo)[[4]]+1.5*(quantile(datimiei$Prezzo)[[4]]-quantile(datimiei$Prezzo)[[2]])
cardine2


#disegno del box plot
boxplot(datimiei$Prezzo,xlab="prezzi",col="red",ylim=c(0,200))
#le estremità della scatola sono in corrispondenza di Q1,Q2 e Q3, mentre la linea centrale è Q2.
#Il primo baffo ha la lunghezza data dalla differenza tra Q1 ed il minimo (del campione privo di outliers)
#Il secondo baffo ha lunghezza data dalla differenza tra il massimo (del campione privo di outliers) e Q3.

plot(ecdf(datimiei$Prezzo),main="Funzione di distribuzione empirica", col="green", ylim=c(0,1))
par(mfrow=c(1,2))

#costruiamo il nostro qqplot
prova1<-sort(datimiei$Prezzo)
prova2<-0

for (i in 1:10498){
  prova2[i]<-qnorm((i-0.5)/10498)
}

plot(prova2,prova1)
qqnorm(datimiei$Prezzo)

#standardizzare i dati del campione genera nuovi dati linearmente legati 
#al campione prima della standardizzazione. La forma del plot rimane la stessa, cambia solo la scala

qqnorm(datimiei$Prezzo)

z<-(datimiei$Prezzo-mean(datimiei$Prezzo))/sd(datimiei$Prezzo)
qqnorm(z)

par(mfrow=c(1,1))






