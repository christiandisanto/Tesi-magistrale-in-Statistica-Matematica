#carichiamo il dataset 
datimiei<-as.data.frame(read_excel("../datasets/year/dati_2021_data_spesa_sede.xlsx"))

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






