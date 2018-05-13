Conso=read.csv("Conso.csv", sep = ";", header = TRUE, dec = ",")
Conso=Conso[,-1]
Conso$Date=as.Date(Conso$Date)

Temp=read.csv("Temp.csv", sep = ";", header = TRUE, dec = ",")
Temp=Temp[,-1]
Temp$Date=as.Date(Temp$Date)


str(Conso)

colnames(Temp)[which(colnames(Temp)=="DE")]<-"tmoy"

Temp %>%
  plyr::mutate(tmoy1=lag(tmoy, 1))%>%
  plyr::mutate(tmoy2=lag(tmoy, 2))->Temp

Temp.nona<-na.omit(Temp)
