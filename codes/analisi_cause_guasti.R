library("readxl")
#carichiamo il dataset 
datimiei<-as.data.frame(read_excel("../datasets/dataset_completo.xlsx",sheet = 1))

#moda
#in R non esiste una funzione predefinita, si può usare una funzione opportuna
getmode<-function(v){ 
  uniqv<-unique(v) #crea un vettore senza duplicati
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

#calcoliamo la lunghezza del campione
n<-nrow(datimiei)
n

# Calcolo delle frequenze assolute
table(datimiei$`causa guasti`)

#disegno del diagramma delle frequenze assolute della variabile cause di guasti
plot(table(datimiei$`causa guasti`))

moda<-getmode(datimiei$`causa guasti`)
moda