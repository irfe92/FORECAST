
# importation des bases et créations variables----
library(dplyr)
Conso=read.csv("Conso.csv", sep = ";", header = TRUE, dec = ",")
Conso=Conso[,-1]
Conso$Date=as.Date(Conso$Date)
str(Conso)
head(Conso)

Temp=read.csv("Temp.csv", sep = ";", header = TRUE, dec = ",")
Temp=Temp[,-1]
Temp$Date=as.Date(Temp$Date)
colnames(Temp)[which(colnames(Temp)=="DE")]<-"tmoy"

#creation variables temperatures retardees
Temp %>%
  plyr::mutate(tmoy1=lag(tmoy, 1))%>%
  plyr::mutate(tmoy2=lag(tmoy, 2))%>%
  plyr::mutate(tmoy3=lag(tmoy, 3))%>%
  plyr::mutate(tmoy4=lag(tmoy, 4))%>%
  plyr::mutate(tmoy5=lag(tmoy, 5))%>%
  plyr::mutate(tmoy6=lag(tmoy, 6))%>%
  plyr::mutate(tmoy7=lag(tmoy, 7))->Temp
Temp.nona<-na.omit(Temp)
head(Temp.nona)

# creation d'une base regroupant les deux fichiers
base<-merge(Conso,Temp,by="Date", all.x=TRUE)
base.nona<-na.omit(base)
head(base.nona)

#base sans les villes et sans la date
base.sv<-base.nona[,-c(1,5,6,7,8,9,10)]
head(base.sv)

#creation d'une variable jour avec les jours de la semaine, classe factor
install.packages(c("lubridate", "magrittr"))
library("lubridate")
library("magrittr")
library(dplyr)
base.nona%>%
  plyr::mutate(mois=month(base.nona$Date,label = TRUE))%>%
  plyr::mutate(jour=wday(base.nona$Date,label = TRUE))->base.nona


#creation base uniquement de conso et temperatures retardées, sans les villes, ni jour, ni mois
base.temp<-base.nona[,-c(1,2,3,5,6,7,8,9,10,19,20)]
head(base.temp)


plot(base.nona$TOTAL,base.nona$tmoy)
boxplot(base.nona$TOTAL~base.nona$jour)
boxplot(base.nona$tmoy~base.nona$jour)
boxplot(base.nona$TOTAL~base.nona$mois)
boxplot(base.nona$tmoy~base.nona$mois)
boxplot(base.nona$tmoy)
boxplot(base.nona$TOTAL)
summary(base.nona$TOTAL)
summary(base.nona$tmoy)

#relation linéaire entre la température et la conso totale

# modèles linéaires avec jours, mois et les températures retardées----

mod.J<-lm(TOTAL~jour,data=base.nona)
mod.J.sum<-summary(mod.J)
mod.J.sum
plot(mod.J)


mod.M<-lm(TOTAL~mois,data=base.nona)
mod.M.sum<-summary(mod.M)
mod.M.sum
plot(mod.M)

mod.L0<-lm(TOTAL~tmoy,data=base.temp)
mod.L0.sum<-summary(mod.L0)
mod.L0.sum
plot(mod.L0)
# les résidus vs Fitted ont une structure

mod.L1<-lm(TOTAL~tmoy+tmoy1,data=base.temp)
mod.L1.sum<-summary(mod.L1)
mod.L1.sum
plot(mod.L1)

mod.L2<-lm(TOTAL~tmoy+tmoy1+tmoy2,data=base.temp)
mod.L2.sum<-summary(mod.L2)
mod.L2.sum
plot(mod.L2)

mod.L3<-lm(TOTAL~tmoy+tmoy1+tmoy2+tmoy3,data=base.temp)
mod.L3.sum<-summary(mod.L3)
mod.L3.sum
plot(mod.L3)

mod.L4<-lm(TOTAL~tmoy+tmoy1+tmoy2+tmoy3+tmoy4,data=base.temp)
mod.L4.sum<-summary(mod.L4)
mod.L4.sum
plot(mod.L4)

mod.L5<-lm(TOTAL~tmoy+tmoy1+tmoy2+tmoy3+tmoy4+tmoy5,data=base.temp)
mod.L5.sum<-summary(mod.L5)
mod.L5.sum
plot(mod.L5)

mod.L6<-lm(TOTAL~tmoy+tmoy1+tmoy2+tmoy3+tmoy4+tmoy5+tmoy6,data=base.temp)
mod.L6.sum<-summary(mod.L6)
mod.L6.sum
plot(mod.L6)

mod.L7<-lm(TOTAL~.,data=base.temp)
mod.L7.sum<-summary(mod.L7)
mod.L7.sum
plot(mod.L7)

mod.best.fw<-lm(TOTAL~tmoy+tmoy2+tmoy4,data=base.temp)
mod.best.fw.sum<-summary(mod.best.fw)
mod.best.fw.sum

#selection variables regsubset sur temperatures retardees, methode exhaustive----
#install.packages('leaps')
#install.packages('ISLR')
library(ISLR)
library(leaps)

# 2) Création de l'échantillon test, 2/3 individus nrow(base.temp)
set.seed(1)
dim<-nrow(base.temp)
train=sample(dim,2*dim/3,replace=FALSE)

# 3.a) On tente de modéliser la conso TOTAL par les températures retardées jusqu'à 7 JOURS
#    Pour chaque complexité de 1 à 8, on sélectionne le meilleur modèle
#    grâce à une forward selection (sur le training set)
#    On va donc obtenir 8 modèles comprenant entre une et 7 variables
best_models8=regsubsets(TOTAL~.,data=base.temp[train,],nvmax=8,method='exhaustive')


# 4) On a 8 modèles, il faut choisir le meilleur. Pour ce faire, on va appliquer
#    chacun des modèles sur le test set et calculer la MSE
mse=rep(NA,8)
test=model.matrix(TOTAL~.,data=base.temp[-train,])
for(i in 1:8){
  coefi=coef(best_models8,id=i)
  pred=test[,names(coefi)]%*%coefi
  mse[i]=mean((base.temp$TOTAL[-train]-pred)^2)
}

# 5) Ici on plot les RMSE des 8 modèles sur le training et sur le test set
#    On choisit le modèle qui a la RMSE la plus petite sur le test set: c'est celui avec 2 variables
plot(sqrt(mse),ylab='Root MSE des 8 modèles',pch=19,type='b')


# 6) Pour accéder aux coefficient du modèle 2, on appelle la fonction coeff
#      Pour accéder aux RSS des modèles, on lance la fonction summary
coef(best_models8,2)
summary(best_models8)$rss


#selection variables regsubset sur temperatures retardees, methode FORWARD----
#install.packages('leaps')
#install.packages('ISLR')
library(ISLR)
library(leaps)

# 2) Création de l'échantillon test, 2/3 individus nrow(base.temp)
set.seed(1)
dim<-nrow(base.temp)
train=sample(dim,2*dim/3,replace=FALSE)

# 3.a) On tente de modéliser la conso TOTAL par les températures retardées jusqu'à 7 JOURS
#    Pour chaque complexité de 1 à 8, on sélectionne le meilleur modèle
#    grâce à une forward selection (sur le training set)
#    On va donc obtenir 8 modèles comprenant entre une et 7 variables
best_models8_forward=regsubsets(TOTAL~.,data=base.temp[train,],nvmax=8,method='forward')


# 4) On a 8 modèles, il faut choisir le meilleur. Pour ce faire, on va appliquer
#    chacun des modèles sur le test set et calculer la MSE
mse.fw=rep(NA,8)
test=model.matrix(TOTAL~.,data=base.temp[-train,])
for(i in 1:8){
  coefi=coef(best_models8_forward,id=i)
  pred=test[,names(coefi)]%*%coefi
  mse.fw[i]=mean((base.temp$TOTAL[-train]-pred)^2)
}


# 5) Ici on plot les RMSE des 8 modèles sur le training et sur le test set
#    On choisit le modèle qui a la RMSE la plus petite sur le test set: c'est celui avec 2 variables
plot(sqrt(mse.fw),ylab='Root MSE des 8 modèles FW',pch=19,type='b')


# 6) Pour accéder aux coefficient du modèle 2, on appelle la fonction coeff
#      Pour accéder aux RSS des modèles, on lance la fonction summary
coef(best_models8_forward,2)
coef(best_models8,2)

summary(best_models8)
summary(best_models8_forward)

summary(best_models8_forward)$rss

#quand la base de données ne comporte que les températures c'est le modèle avec 2 VARIABLES qui a la plus petite MSE
#quand la base de données comporte aussi les mois, c'est le modèle à 6 VARIABLES qui a la plus petite MSE


#selection variables regsubset sur temperatures retardees, methode BACKWARD PB----
#install.packages('leaps')
#install.packages('ISLR')
library(ISLR)
library(leaps)

# 2) Création de l'échantillon test, 2/3 individus nrow(base.temp)
set.seed(1)
dim<-nrow(base.temp)
train=sample(dim,2*dim/3,replace=FALSE)

# 3.a) On tente de modéliser la conso TOTAL par les températures retardées jusqu'à 7 JOURS
#    Pour chaque complexité de 1 à 8, on sélectionne le meilleur modèle
#    grâce à une forward selection (sur le training set)
#    On va donc obtenir 8 modèles comprenant entre une et 7 variables
best_models8.BW=regsubsets(TOTAL~.,data=base.temp[train,],nvmax=8,method='backward')


# 4) On a 8 modèles, il faut choisir le meilleur. Pour ce faire, on va appliquer
#    chacun des modèles sur le test set et calculer la MSE
mse.BW=rep(NA,8)
test=model.matrix(TOTAL~.,data=base.temp[-train,])
for(i in 1:8){
  coefi=coef(best_models8.BW,id=i)
  pred=test[,names(coefi)]%*%coefi
  mse[i]=mean((base.temp$TOTAL[-train]-pred)^2)
}

# 5) Ici on plot les RMSE des 8 modèles sur le training et sur le test set
#    On choisit le modèle qui a la RMSE la plus petite sur le test set: c'est celui avec 2 variables
plot(sqrt(mse.BW),ylab='Root MSE des 8 modèles',pch=19,type='b')


# 6) Pour accéder aux coefficient du modèle 2, on appelle la fonction coeff
#      Pour accéder aux RSS des modèles, on lance la fonction summary
coef(best_models8_forward,2)
summary(best_models8_forward)$rss


#quand la base de données ne comporte que les températures c'est le modèle avec 2 VARIABLES qui a la plus petite MSE
#quand la base de données comporte aussi les mois, c'est le modèle à 6 VARIABLES qui a la plus petite MSE




# random forest sur base totale----

library(randomForest)

set.seed(1)
fit.all <- randomForest(TOTAL~ ., data = base.sv, na.action = na.roughfix)
# par defaut mtry=sqr(nb variables) soit 4 ie nb de variables testées à chaque division 
print(fit.all)
#mean square residuals à 9557, % var explained à 98.86%

set.seed(1)
fit.all.2000 <- randomForest(TOTAL~ ., data = base.sv, na.action = na.roughfix,ntree=2000)
print(fit.all.2000)
#mean square residuals à 9430, % var explained à 98.87%

# random forest sur base temperatures----

library(randomForest)

set.seed(1)
fit.temp <- randomForest(TOTAL~ ., data = base.temp, na.action = na.roughfix)
# par defaut mtry=sqr(nb variables) soit 4 ie nb de variables testées à chaque division 
print(fit.temp)
#mean square residuals à 50492, % var explained plus faible à 93.96%

fit.temp.2000 <- randomForest(TOTAL~ ., data = base.temp, na.action = na.roughfix,ntree=2000, mtry=2)
print(fit.temp.2000)
#mean square residuals à 50135, % var explained à 94.01%