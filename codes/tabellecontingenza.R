#tabelle di contingenza
library("readxl")
datimiei<-as.data.frame(read_excel("dati_2021_data_spesa_sede.xlsx"))
n<-nrow(datimiei)
n
classiprezzo<-c(cut(datimiei$Prezzo,breaks = c(0,500,2000,4000,10000)))
fclassiprezzo<-factor(classiprezzo)
levels(fclassiprezzo)<-c("(0-500]","(500-2000]","(2000-4000]","(4000-10000]")

#Gennaio
contingenza<-table(fclassiprezzo[1:784],datimiei$Sede[1:784])
addmargins(contingenza)
#oppure:
chisquare<-chisq.test(fclassiprezzo[1:784],datimiei$Sede[1:784])
#frequenze osservate
chisquareoss<-chisquare$observed
dati<-addmargins(chisquareoss)
dati
write.table(dati,"clipboard",sep = "\t",col.names = NA)
#frequenze attese
chisquareattese<-chisquare$expected
#chisquareattese
addmargins(chisquareattese)

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
contingenza<-table(fclassiprezzo[1689:2529],datimiei$Sede[1689:2529])
addmargins(contingenza)
dati<-addmargins(contingenza)
dati
write.table(dati,"clipboard",sep = "\t",col.names = NA)

#APRILE
contingenza<-table(fclassiprezzo[2530:3272],datimiei$Sede[2530:3272])
dati<-addmargins(contingenza)
dati
write.table(dati,"clipboard",sep = "\t",col.names = NA)

#MAGGIO
contingenza<-table(fclassiprezzo[3273:3936],datimiei$Sede[3273:3936])
dati<-addmargins(contingenza)
dati
write.table(dati,"clipboard",sep = "\t",col.names = NA)

#GIUGNO
contingenza<-table(fclassiprezzo[3937:4888],datimiei$Sede[3937:4888])
dati<-addmargins(contingenza)
dati
write.table(dati,"clipboard",sep = "\t",col.names = NA)

#LUGLIO
contingenza<-table(fclassiprezzo[4889:5848],datimiei$Sede[4889:5848])
dati<-addmargins(contingenza)
dati
write.table(dati,"clipboard",sep = "\t",col.names = NA)

#AGOSTO
contingenza<-table(fclassiprezzo[5849:6532],datimiei$Sede[5849:6532])
dati<-addmargins(contingenza)
dati
write.table(dati,"clipboard",sep = "\t",col.names = NA)

#SETTEMBRE
contingenza<-table(fclassiprezzo[6533:7593],datimiei$Sede[6533:7593])
dati<-addmargins(contingenza)
dati
write.table(dati,"clipboard",sep = "\t",col.names = NA)

#OTTOBRE
contingenza<-table(fclassiprezzo[7594:8634],datimiei$Sede[7594:8634])
dati<-addmargins(contingenza)
dati
write.table(dati,"clipboard",sep = "\t",col.names = NA)

#NOVEMBRE
contingenza<-table(fclassiprezzo[8635:9436],datimiei$Sede[8635:9436])
dati<-addmargins(contingenza)
dati
write.table(dati,"clipboard",sep = "\t",col.names = NA)

#DICEMBRE
contingenza<-table(fclassiprezzo[9437:10498],datimiei$Sede[9437:10498])
dati<-addmargins(contingenza)
dati
write.table(dati,"clipboard",sep = "\t",col.names = NA)
