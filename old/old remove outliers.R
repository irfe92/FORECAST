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

long.Conso <- melt(Conso, id.vars = c("Date"))
colnames(long.Conso)<-c("Date", "id", "conso") 
#------------------------------
# Plot (there are NA)
#------------------------------
ggplot()+
geom_line(data=long.Conso, aes(x=Date, y=conso))+
facet_wrap(~ id,scales = "free") +
scale_fill_brewer()+
geom_vline(xintercept = as.numeric(as.Date("2015-01-01")), linetype=4, color='red')+
ylab('Unit')+
theme_bw() 

