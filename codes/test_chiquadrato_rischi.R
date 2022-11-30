library("readxl")
#carichiamo il dataset 
datimiei<-as.data.frame(read_excel("dataset_completo.xlsx",sheet = 4))

#calcoliamo la lunghezza del campione
n<-nrow(datimiei)
n

#calcoliamo gli indici di posizione e dispersione
summary(datimiei$`num rischi occorsi per settimana`)
var(datimiei$`num rischi occorsi per settimana`)
sd(datimiei$`num rischi occorsi per settimana`)

#grafico frequenze assolute per ogni settimana
barplot(datimiei$`num rischi occorsi per settimana`,xlab = "settimane",ylab = "numero di cause di guasti",ylim = c(0,150))

#EFFETTUO UNA TRASFORMAZIONE LOGARITMICA
c<-5.503
trasformazione_dati<-trunc(c*log(datimiei$`num rischi occorsi per settimana`+1))
datimiei2<-trasformazione_dati-min(trasformazione_dati)
datimiei2

#calcolo le frequenze assolute del numero di guasti
frequenze<-table(datimiei2)
frequenze


massimo<-max(datimiei2)
freqosservate<-table(cut(datimiei2,breaks = c(0:(massimo+1)),right=FALSE))
freqosservate

#stima del parametro lambda che coincide con la media campionaria
lambda<-mean(datimiei2)
lambda

# TEST DI ADATTAMENTO - DISTRIBUZIONE DI POISSON
#calcolo distribuzione teorica

distrpois<-c(dpois(0:(massimo-1),lambda,log = FALSE),1-sum(dpois(0:(massimo-1),lambda,log = FALSE)))
round(distrpois,digits = 4)
sum(round(distrpois,digits = 4))

#calcolo frequenze attese
freqattese<-round(n*distrpois,digits = 2)
freqattese
#nota che la condizione n*pigreco >=5 è soddisfatta


#calcolo valore della statistica chi quadrato
chisquare<-round(sum((freqosservate-freqattese)^2/freqattese),digits = 2)
chisquare

alfa<-0.05
gradilib<-length(freqosservate)-1-1
valorecritico<-qchisq(1-alfa,df=gradilib)
valorecritico

#decisione del test
if(chisquare>=valorecritico){print("Rifiuto H0")}else{print("Accetto H0")}

#istogramma
hist(datimiei2,right = FALSE,breaks = seq(0,4,1),xlab = "numero di cause di guasti",las=1,main = "Istogramma",ylab="Frequenze",yaxt="n",ylim = c(0,30))
axis(2,at=seq(0,30,by=5),las=2)
lines(freqattese~c(0:3+0.5),lwd=2,col="red")


