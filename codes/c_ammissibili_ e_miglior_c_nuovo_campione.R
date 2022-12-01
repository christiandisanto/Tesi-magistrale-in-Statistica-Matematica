library("readxl")
#carichiamo il dataset
datimiei<-as.data.frame(read_excel("../datasets/new_sample.xlsx"))

#calcoliamo la lunghezza del campione
n<-nrow(datimiei)
n

###################################################################################
#FUNZIONE PER INDIVIDUARE I VALORI AMMISSIBILI DI C
esito_test<-function(costante,campione,alfa){
  dati<-trunc(costante*log(campione+1))
  massimo<-max(dati)
  freqosservate<-table(cut(dati,breaks = c(0:(massimo+1)),right=FALSE))
  if(length(freqosservate>=3)){
  lambda<-mean(dati)
  distrpois<-c(dpois(0:(massimo-1),lambda,log = FALSE),1-sum(dpois(0:(massimo-1),lambda,log = FALSE)))
  freqattese<-round(n*distrpois,digits = 2)
  chisquare<-round(sum((freqosservate-freqattese)^2/freqattese),digits = 2)
  gradilib<-length(freqosservate)-1-1
  valorecritico<-qchisq(1-alfa,df=gradilib)
  frequenze<-as.data.frame(freqosservate)
  a<-ifelse(frequenze[2]>=5,1,0)
  b<-ifelse(freqattese>=5,1,0)
  if(sum(a)<length(freqosservate)){return(0)}#ci devono essere almeno 5 osservazioni in ogni classe
  if(sum(b)<length(freqattese)){return(0)}#tutte le frequenze attese delle classi devono essere >=5
  if(chisquare>=valorecritico){return(0)}else{return(1)}}#con zero indichiamo il rifiuto mentre con uno l'accettazione
else{return(0)}
  }
########################################################################################
#FUNZIONE CHE RITORNA I CHISQUARE
trovamiglior_c<-function(costante,campione,alfa){
  dati<-trunc(costante*log(campione+1))
  massimo<-max(dati)
  freqosservate<-table(cut(dati,breaks = c(0:(massimo+1)),right=FALSE))
  lambda<-mean(dati)
  distrpois<-c(dpois(0:(massimo-1),lambda,log = FALSE),1-sum(dpois(0:(massimo-1),lambda,log = FALSE)))
  freqattese<-round(n*distrpois,digits = 2)
  chisquare<-sum((freqosservate-freqattese)^2/freqattese)
  return(chisquare)}
#####################################################################################
# INDIVIDUIAMO I VALORI DI C PER ALFA PARI A 0.10
c<-0.300
esiti<-0
cc<-0
j<-0.000
for (i in 1:10000){
  esiti[i]<-esito_test((c+j),datimiei$numero_acquisti,0.10)
  cc[i]<-c+j
  j<-j+0.001
}
#cbind(cc,esiti)
valc1<-cc[which(esiti>0)] #valori di c per cui il test chi quadrato funziona
valc1

#INDIVIDUIAMO IL VALORE DI C PER CUI IL CHISQUARE è MINIMO
chisquares<-0
for (i in 1:length(valc1))
{chisquares[i]<-trovamiglior_c(valc1[i],datimiei$numero_acquisti,0.10)}
chisquares
miglior<-valc1[which.min(chisquares)]
miglior

######################################################################################

# INDIVIDUIAMO I VALORI DI C PER ALFA PARI A 0.05
c<-0.300
esiti<-0
cc<-0
j<-0.000
for (i in 1:10000){
  esiti[i]<-esito_test((c+j),datimiei$numero_acquisti,0.05)
  cc[i]<-c+j
  j<-j+0.001
}
#cbind(cc,esiti)
valc2<-cc[which(esiti>0)] #valori di c per cui il test chi quadrato funziona
valc2

#INDIVIDUIAMO IL VALORE DI C PER CUI IL CHISQUARE è MINIMO
chisquares<-0
for (i in 1:length(valc2))
{chisquares[i]<-trovamiglior_c(valc2[i],datimiei$numero_acquisti,0.05)}
round(chisquares,digits=4)
miglior<-valc2[which.min(chisquares)]
miglior
##########################################################################################

# INDIVIDUIAMO I VALORI DI C PER ALFA PARI A 0.01
c<-0.300
esiti<-0
cc<-0
j<-0.000
for (i in 1:10000){
  esiti[i]<-esito_test((c+j),datimiei$numero_acquisti,0.01)
  cc[i]<-c+j
  j<-j+0.001
}
#cbind(cc,esiti)
valc3<-cc[which(esiti>0)] #valori di c per cui il test chi quadrato funziona
valc3

#INDIVIDUIAMO IL VALORE DI C PER CUI IL CHISQUARE è MINIMO
chisquares<-0
for (i in 1:length(valc3))
{chisquares[i]<-trovamiglior_c(valc3[i],datimiei$numero_acquisti,0.01)}
chisquares
miglior<-valc3[which.min(chisquares)]
miglior





##############################################################################################
#
y1<-rep(0.10,length(valc1))
y2<-rep(0.05,length(valc2))
y3<-rep(0.01,length(valc3))
ascisse1<-c(valc1,valc2,valc3)
ordinate1<-c(y1,y2,y3)
grafico1<-data.frame(ascisse1,ordinate1)

#GRAFICO 1
plot(grafico1$ascisse1,grafico1$ordinate1,xlim = c(min(valc3)-0.01,max(valc3)+0.01),ylim = c(0.000,0.200),xlab = "valori di c",ylab = "valori di alfa")
#axis(1,at=seq(min(valc3)-0.001,max(valc3)+0.001,by=0.1),las=0)
points(valc1,y1,col="red")
points(valc2,y2,col="blue")
points(valc3,y3,col="black")
legend(locator(1),c("alfa=0.01","alfa=0.05","alfa=0.10"),pch=c(1,16),col=c("black","blue","red"))
############################################################################################
ascisse2<-c(y1,y2,y3)
ordinate2<-c(valc1,valc2,valc3)
grafico2<-data.frame(ascisse2,ordinate2)

#GRAFICO 2 (con ascisse e ordinate invertite)
range(min(valc3)-0.01,max(valc3)+0.01)
plot(grafico2$ascisse2,grafico2$ordinate2,xlim = c(0.000,0.200),ylim =c(min(valc3)-0.01,max(valc3)+0.01) ,xlab = "valori di alfa",ylab = "valori di c")
#axis(2,at=seq(min(valc3)-0.001,max(valc3)+0.001,by=0.1),las=0)
points(y1,valc1,col="red")
points(y2,valc2,col="blue")
points(y3,valc3,col="black")
legend(locator(1),c("alfa=0.01","alfa=0.05","alfa=0.10"),pch=c(1,16),col=c("black","blue","red"))
