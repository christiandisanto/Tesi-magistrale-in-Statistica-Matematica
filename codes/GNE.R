#tabelle di contingenza
library("readxl")
datimiei<-as.data.frame(read_excel("dati_2021_data_spesa_sedeLET.xlsx"))
classiprezzo<-c(cut(datimiei$Prezzo,breaks = c(0,500,2000,4000,10000)))
fclassiprezzo<-factor(classiprezzo)
levels(fclassiprezzo)<-c("(0-500]","(500-2000]","(2000-4000]","(4000-10000]")

#GENNAIO
chisquare<-chisq.test(fclassiprezzo[1:784],datimiei$Sede[1:784])
chisquareoss<-chisquare$observed
dati<-addmargins(chisquareoss)
dati
write.table(dati,"clipboard",sep = "\t",col.names = NA)

#FEBBRAIO
chisquare<-chisq.test(fclassiprezzo[785:1688],datimiei$Sede[785:1688])
chisquareoss<-chisquare$observed
dati<-addmargins(chisquareoss)
dati
write.table(dati,"clipboard",sep = "\t",col.names = NA)

#MARZO
chisquare<-chisq.test(fclassiprezzo[1689:2529],datimiei$Sede[1689:2529])
chisquareoss<-chisquare$observed
dati<-addmargins(chisquareoss)
dati
write.table(dati,"clipboard",sep = "\t",col.names = NA)

#APRILE


#MAGGIO


#GIUGNO


#LUGLIO


#AGOSTO


#SETTEMBRE


#OTTOBRE


#NOVEMBRE


#DICEMBRE





contingenza2<-table(datimiei$Sede[1:784],fclassiprezzo[1:784])
addmargins(contingenza2)

#FEBBRAIO
contingenza<-table(fclassiprezzo[785:1688],datimiei$Sede[785:1688])
#contingenza
addmargins(contingenza)
dati<-addmargins(contingenza)
dati
write.table(dati,"clipboard",sep = "\t",col.names = NA)

#MARZO
classiprezzo<-c(cut(datimiei$Prezzo,breaks = c(0,500,2000,4000,10000)))
fclassiprezzo<-factor(classiprezzo)
levels(fclassiprezzo)<-c("(0-500]","(500-2000]","(2000-4000]","(4000-10000]")
#fclassiprezzo
contingenza<-table(fclassiprezzo[1689:2529],datimiei$Sede[1689:2529])
#contingenza
addmargins(contingenza)

#APRILE
classiprezzo<-c(cut(datimiei$Prezzo,breaks = c(0,500,2000,4000,10000)))
fclassiprezzo<-factor(classiprezzo)
levels(fclassiprezzo)<-c("(0-500]","(500-2000]","(2000-4000]","(4000-10000]")
#fclassiprezzo
contingenza<-table(fclassiprezzo[2530:3272],datimiei$Sede[2530:3272])
#contingenza
addmargins(contingenza)

#MAGGIO
classiprezzo<-c(cut(datimiei$Prezzo,breaks = c(0,500,2000,4000,10000)))
fclassiprezzo<-factor(classiprezzo)
levels(fclassiprezzo)<-c("(0-500]","(500-2000]","(2000-4000]","(4000-10000]")
#fclassiprezzo
contingenza<-table(fclassiprezzo[3273:3936],datimiei$Sede[3273:3936])
#contingenza
addmargins(contingenza)

#GIUGNO
classiprezzo<-c(cut(datimiei$Prezzo,breaks = c(0,500,2000,4000,10000)))
fclassiprezzo<-factor(classiprezzo)
levels(fclassiprezzo)<-c("(0-500]","(500-2000]","(2000-4000]","(4000-10000]")
#fclassiprezzo
contingenza<-table(fclassiprezzo[3937:4888],datimiei$Sede[3937:4888])
#contingenza
addmargins(contingenza)

#LUGLIO
classiprezzo<-c(cut(datimiei$Prezzo,breaks = c(0,500,2000,4000,10000)))
fclassiprezzo<-factor(classiprezzo)
levels(fclassiprezzo)<-c("(0-500]","(500-2000]","(2000-4000]","(4000-10000]")
#fclassiprezzo
contingenza<-table(fclassiprezzo[4889:5848],datimiei$Sede[4889:5848])
#contingenza
addmargins(contingenza)

#AGOSTO
classiprezzo<-c(cut(datimiei$Prezzo,breaks = c(0,500,2000,4000,10000)))
fclassiprezzo<-factor(classiprezzo)
levels(fclassiprezzo)<-c("(0-500]","(500-2000]","(2000-4000]","(4000-10000]")
#fclassiprezzo
contingenza<-table(fclassiprezzo[5849:6532],datimiei$Sede[5849:6532])
#contingenza
addmargins(contingenza)

#SETTEMBRE
classiprezzo<-c(cut(datimiei$Prezzo,breaks = c(0,500,2000,4000,10000)))
fclassiprezzo<-factor(classiprezzo)
levels(fclassiprezzo)<-c("(0-500]","(500-2000]","(2000-4000]","(4000-10000]")
#fclassiprezzo
contingenza<-table(fclassiprezzo[6533:7593],datimiei$Sede[6533:7593])
#contingenza
addmargins(contingenza)

#OTTOBRE
classiprezzo<-c(cut(datimiei$Prezzo,breaks = c(0,500,2000,4000,10000)))
fclassiprezzo<-factor(classiprezzo)
levels(fclassiprezzo)<-c("(0-500]","(500-2000]","(2000-4000]","(4000-10000]")
#fclassiprezzo
contingenza<-table(fclassiprezzo[7594:8634],datimiei$Sede[7594:8634])
#contingenza
addmargins(contingenza)

#NOVEMBRE
classiprezzo<-c(cut(datimiei$Prezzo,breaks = c(0,500,2000,4000,10000)))
fclassiprezzo<-factor(classiprezzo)
levels(fclassiprezzo)<-c("(0-500]","(500-2000]","(2000-4000]","(4000-10000]")
#fclassiprezzo
contingenza<-table(fclassiprezzo[8635:9436],datimiei$Sede[8635:9436])
#contingenza
addmargins(contingenza)

#DICEMBRE
classiprezzo<-c(cut(datimiei$Prezzo,breaks = c(0,500,2000,4000,10000)))
fclassiprezzo<-factor(classiprezzo)
levels(fclassiprezzo)<-c("(0-500]","(500-2000]","(2000-4000]","(4000-10000]")
#fclassiprezzo
contingenza<-table(fclassiprezzo[9437:10498],datimiei$Sede[6533:10498])
#contingenza
addmargins(contingenza)
#chisquare<-chisq.test(fclassiprezzo[1:784],datimiei$Sede[1:784])
#chisquareoss<-chisquare$observed
#chisquareoss
#addmargins(chisquareoss)
#classisede<-c("S1","S2","S3","S4","S5","S6","S7","S8","S9","S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20","S21","S22","S23","S24")