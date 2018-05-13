# # ==========================================================================
# # Remove outliers: boxplot procedure 
# # ==========================================================================

# #
# # wide.Conso is the data with outliers==>they will be removed; Conso=clean data set;
# #

# # > head(wide.Conso)
# #         Date NL     UK ES FR DE BE
# # 1 2005-01-01 NA 224.52 NA NA NA NA
# # 2 2005-01-02 NA 260.43 NA NA NA NA
# # 3 2005-01-03 NA 250.19 NA NA NA NA
# # 4 2005-01-04 NA 257.63 NA NA NA NA
# # 5 2005-01-05 NA 276.01 NA NA NA NA
# # 6 2005-01-06 NA 258.90 NA NA NA NA



# noms<-colnames(wide.Conso)[-1]
# base<-wide.Conso
# outlier_<-out_<-idout_<-NULL

# for(i in noms)
# {
# for (m in c(1:12))
# {
# var_name<-base[as.numeric(format(base$Date, "%m"))==m,i]
# var_name_dated<-base[as.numeric(format(base$Date, "%m"))==m,c("Date",i)]

#  tot <- sum(!is.na(var_name))
#   na1 <- sum(is.na(var_name))
#   m1 <- mean(var_name, na.rm = T)
#   par(mfrow=c(2, 2), oma=c(0,0,3,0))
#   boxplot(var_name, main="With outliers")
#   hist(var_name, main="With outliers", xlab=NA, ylab=NA)
#   outlier <- boxplot.stats(var_name)$out
#   outlierRow = which(var_name %in% c (outlier))
#   mo <- mean(outlier)
#   var_name <- ifelse(var_name %in% outlier, NA, var_name)
#   boxplot(var_name, main="Without outliers")
#   hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
#   title(paste("Outlier Check", ":", i,  month.abb[m], sep=" "), outer=TRUE)
#   na2 <- sum(is.na(var_name))
#   message(paste("Outlier Information", ":", i,  month.abb[m], sep=" "))
#   message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
#   message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
#   message("Mean of the outliers: ", mo)
#   m2 <- mean(var_name, na.rm = T)
#   message("Mean without removing outliers: ", m1)
#   message("Mean if we remove outliers: ", m2)

# if (mo=="NaN"){print(message('No outliers'))}else{
# print(info<-data.frame(Dates=var_name_dated[c(outlierRow),"Date"], IdConso=i, IdRegion=i, OutValue=outlier))}
# base[as.numeric(format(base$Date, "%m"))==m,i]<-var_name
# }}
# baseBoxPlot<-base


# # ==========================================================================
# # Clean Conso
# # ==========================================================================
# Conso<-base

# long.Conso <- melt(Conso, id.vars = c("Date"))
# colnames(long.Conso)<-c("Date", "id", "conso") 
# #------------------------------
# # Plot (there are NA)
# #------------------------------
# ggplot()+
# geom_line(data=long.Conso, aes(x=Date, y=conso))+
# facet_wrap(~ id,scales = "free") +
# scale_fill_brewer()+
# geom_vline(xintercept = as.numeric(as.Date("2015-01-01")), linetype=4, color='red')+
# ylab('Unit')+
# theme_bw() 


# ==========================================================================
# Remove outliers from formats
# ==========================================================================
formats<-get(load("C:\\Users\\MF1177\\Desktop\\MyGit\\FORECAST\\final.formats.RData"))


Pays<-unique(formats$Pays)

#
#With outliers 
#

ggplot(formats, aes(x=Date,y=Conso, color=Pays)) +
    geom_line() +
    facet_wrap( ~ Pays, scales="free") +
    ggtitle("Consumption with outliers") +
    theme(legend.position="none")   



# ==========================================================================
# Remove outliers from base "formats"
# ==========================================================================

# > head(formats)
#          Date Pays    Conso    tmoy   month year       day weekend   cosinus      sinus wday quarter      season doy holidays     day_length jc lagholidays
# 1: 2014-01-03   FR 1640.478  9.5766 January 2014    Friday       0 0.9986668 0.05161967    6       1 peak_season   3        0 12.00020 hours  0           0
# 2: 2014-01-04   FR 1625.095  9.1782 January 2014  Saturday       1 0.9976303 0.06880243    7       1 peak_season   4        0 12.00068 hours  1           0
# 3: 2014-01-05   FR 1712.825  7.3562 January 2014    Sunday       1 0.9962982 0.08596480    1       1 peak_season   5        0 12.00119 hours  1           0
# 4: 2014-01-06   FR 1706.572 11.0275 January 2014    Monday       0 0.9946708 0.10310170    2       1 peak_season   6        0 12.00173 hours  0           0
# 5: 2014-01-07   FR 1627.610 11.1275 January 2014   Tuesday       0 0.9927487 0.12020804    3       1 peak_season   7        0 12.00231 hours  0           0
# 6: 2014-01-08   FR 1584.281 11.3100 January 2014 Wednesday       0 0.9905325 0.13727877    4       1 peak_season   8        0 12.00292 hours  0           0
#    leadholidays   tmoy1   tmoy2      teff seuil T00 Wind_Speed Cloud_covering Solar_radiation Raining_Level Pressure
# 1:            0  9.2144  7.9234  9.250514  17.2   0         NA             NA              NA            NA       NA
# 2:            0  9.5766  9.2144  9.251720  17.2   0         NA             NA              NA            NA       NA
# 3:            0  9.1782  9.5766  8.021204  17.2   0         NA             NA              NA            NA       NA
# 4:            0  7.3562  9.1782 10.107491  17.2   0         NA             NA              NA            NA       NA
# 5:            0 11.0275  7.3562 10.507092  17.2   0         NA             NA              NA            NA       NA


outlier_<-out_<-idout_<-NULL

for(i in Pays)
{

base<-subset(formats, Pays==i)
base<-base[,c("Date", "Conso")]
colnames(base)[2]<-i
base<-data.frame(base)

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
}

 formats[which(formats$Pays==i),]$Conso<-base[,2]


}    


# ==========================================================================
# Final Data to Use
# ==========================================================================
ggplot(formats, aes(x=Date,y=Conso, color=Pays)) +
    geom_line() +
    facet_wrap( ~ Pays, scales="free") +
    ggtitle("Consumption with outliers") +
    theme(legend.position="none")   