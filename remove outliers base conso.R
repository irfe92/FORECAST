# ==========================================================================
# Remove outliers: boxplot procedure 
# ==========================================================================

#
# wide.Conso is the data with outliers==>they will be removed; Conso=clean data set;
#

# > head(wide.Conso)
#         Date NL     UK ES FR DE BE
# 1 2005-01-01 NA 224.52 NA NA NA NA
# 2 2005-01-02 NA 260.43 NA NA NA NA
# 3 2005-01-03 NA 250.19 NA NA NA NA
# 4 2005-01-04 NA 257.63 NA NA NA NA
# 5 2005-01-05 NA 276.01 NA NA NA NA
# 6 2005-01-06 NA 258.90 NA NA NA NA

Sys.setlocale(category = "LC_TIME", locale="")                                   # base should not be in french!!!!
Sys.setlocale("LC_TIME", "C")

noms<-colnames(wide.Conso)[-1]
base<-wide.Conso
outlier_<-out_<-idout_<-NULL

for(i in noms)
{
for (m in c(1:12))
{
var_name<-base[as.numeric(format(base$Date, "%m"))==m,i]
var_name_dated<-base[as.numeric(format(base$Date, "%m"))==m,c("Date",i)]

 tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  outlierRow = which(var_name %in% c (outlier))
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title(paste("Outlier Check", ":", i,  month.abb[m], sep=" "), outer=TRUE)
  na2 <- sum(is.na(var_name))
  message(paste("Outlier Information", ":", i,  month.abb[m], sep=" "))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)

if (mo=="NaN"){print(message('No outliers'))}else{
print(info<-data.frame(Dates=var_name_dated[c(outlierRow),"Date"], IdConso=i, IdRegion=i, OutValue=outlier))}
base[as.numeric(format(base$Date, "%m"))==m,i]<-var_name
}}
baseBoxPlot<-base


# ==========================================================================
# Clean Conso
# ==========================================================================
Conso<-base

library(reshape2)
long.Conso <- melt(Conso, id.vars = c("Date"))
colnames(long.Conso)<-c("Date", "id", "conso") 
Temp<-wide.Meteo

# creation d'une base regroupant les deux fichiers
base<-merge(long.Conso,long.Meteo,by=c("Date","id"), all=TRUE)
base<-base[order(base$id),]
summary(base$id)

#creation d'une variable jour avec les jours de la semaine, classe factor
#install.packages(c("lubridate", "magrittr"))
library("lubridate")
library("magrittr")
library(dplyr)
base%>%
  plyr::mutate(mois=month(base$Date,label = TRUE))%>%
  plyr::mutate(jour=wday(base$Date,label = TRUE))->base

# conversion du mois en factor simple car il était en facteur ordonné
base$mois=factor(as.character(base$mois))
base$jour=factor(as.character(base$jour))
class(base$mois)
class(base$jour)

str(base) # on a bien format date et format factor pour id

#jours fériés et week ends

#creation  temperatures retardees
library(dplyr)
base %>%
  plyr::mutate(tmoy1=lag(base$meteo, 1))%>%
  plyr::mutate(tmoy2=lag(base$meteo, 2))%>%
  plyr::mutate(tmoy3=lag(base$meteo, 3))%>%
  plyr::mutate(tmoy4=lag(base$meteo, 4))%>%
  plyr::mutate(tmoy5=lag(base$meteo, 5))%>%
  plyr::mutate(tmoy6=lag(base$meteo, 6))%>%
  plyr::mutate(tmoy7=lag(base$meteo, 7))->base

# creation de sous base par pays, 
base_NL<-subset(base,id=="NL", select = Date:tmoy7)
base_UK<-subset(base,id=="UK", select = Date:tmoy7)
base_ES<-subset(base,id=="ES", select = Date:tmoy7)
base_FR<-subset(base,id=="FR", select = Date:tmoy7)
base_DE<-subset(base,id=="DE", select = Date:tmoy7)
base_BE<-subset(base,id=="BE", select = Date:tmoy7)

#en enlevant les NA
base_NL<-na.omit(base_NL)
base_UK<-na.omit(base_UK)
base_ES<-na.omit(base_ES)
base_FR<-na.omit(base_FR)
base_DE<-na.omit(base_DE)
base_BE<-na.omit(base_BE)

# on enlève la variable id qui réfère au pays
base_NL<-base_NL[,-2]
base_UK<-base_UK[,-2]
base_ES<-base_ES[,-2]
base_FR<-base_FR[,-2]
base_DE<-base_DE[,-2]
base_BE<-base_BE[,-2]

# pays<-c("NL","UK","ES","FR","DE","BE") 
# for (p in pays)  { base_p <-subset(base, id=="p", select = Date:meteo) }




#------------------------------
# Plot (there are NA)
#------------------------------
library(ggplot2)
ggplot()+
geom_line(data=long.Conso, aes(x=Date, y=conso))+
facet_wrap(~ id,scales = "free") +
scale_fill_brewer()+
geom_vline(xintercept = as.numeric(as.Date("2015-01-01")), linetype=4, color='red')+
ylab('Unit')+
theme_bw() 




