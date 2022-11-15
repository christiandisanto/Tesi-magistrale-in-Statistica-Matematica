library("readxl")
#carichiamo il dataset 
datimiei<-as.data.frame(read_excel("../datasets/new_sample.xlsx"))
#calcoliamo la lunghezza del campione
n<-nrow(datimiei)
n
summary(datimiei)

#Questo dataset non è adeguato per un test di adattamento per la distribuzione di Poisson
#è necessario manipolarlo, ossia trasformare ogni realizzazione x nella parte intera inferiore di c*ln(x+1)
#dove c viene scelto opportunamente:
c<-0.942
datimiei2<-trunc(c*log(datimiei$numero_acquisti+1))
datimiei2

# TEST DI ADATTAMENTO - DISTRIBUZIONE DI POISSON

# H0: i dati sono distribuiti secondo una distribuzione di Poisson
# H1: i dati non seguono una distribuzione di Poisson

#calcolo le frequenze assolute del numero di acquisti/riparazioni
frequenze<-table(datimiei2)
frequenze

#Testare con una distribuzione di Poisson significa confrontare con una variabile che ha infiniti valori.
#Calcoliamo le frequenze osservate per le classi
#"0", "1", "2", "3", "4", "da 5 in poi"
#Ricordiamo che per intervalli chiusi a sinistra ed aperti a destra occorre specificare in cut l'opzione right=FALSE
massimo<-max(datimiei2)
freqosservate<-table(cut(datimiei2,breaks = c(0,1,2,3,4,5,massimo+0.01),right = FALSE))
freqosservate

#calcolo le frequenze attese per una distribuzione di Poisson
#la funzione dpois(x,lambda,log=FALSE) calcola la densità di probabilità di Poisson
#log è un valore logico e se TRUE calcola la probabilità come log(p)

#stima del parametro lambda che coincide con la media per Poisson
lambda<-mean(datimiei2)
lambda

#calcolo distribuzione teorica
distrpois<-c(dpois(0:4,lambda,log = FALSE),1-sum(dpois(0:4,lambda,log = FALSE)))
distrpois

#calcolo frequenze attese
freqattese<-round(n*distrpois,digits = 2)
freqattese
#nota che la condizione n*pigreco >=5 è soddisfatta

#diagramma delle frequenze osservate
plot(table(datimiei2),yaxt="n",ylim =c(0,100),xlab = "numero acquisti", ylab = "frequenze assolute",main="diagramma delle frequenze")
#nuovi ticks: axis(side,at=,las=)
axis(2,at=seq(0,100,by=15),las=2)
#side: un intero che dove posizionare le lineette: 
#1=in basso, 2 a sinistra, 3 in alto, 4 a destra
#at:intervallo e passo 
#las: 0 le lineette sono parallele e 2 perpendicolare all'asse delle ascisse
lines(freqattese~c(0:5),lwd=2,col="red")

#istogramma
hist(datimiei2,right = FALSE,breaks = seq(0,6,1),xlab = "numero di acquisti",las=1,main = "istogramma",yaxt="n",ylim = c(0,100))
axis(2,at=seq(0,100,by=15),las=2)
lines(freqattese~c(0:5+0.5),lwd=2,col="red")


#calcolo valore della statistica chi quadrato
chisquare<-round(sum((freqosservate-freqattese)^2/freqattese),digits = 2)
chisquare

#calcolo valore critico
alfa<-0.05
gradilib<-(6-1-1)
valorecritico<-qchisq(1-alfa,df=gradilib)
valorecritico

#decisione del test
if(chisquare>=valorecritico){print("Rifiuto H0")}else{print("Accetto H0")}




