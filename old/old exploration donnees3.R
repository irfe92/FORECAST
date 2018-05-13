
# analyse descriptives
plot(base$conso,base$meteo)

#etude par pays


# library(dplyr)
# base.nona<-na.omit(base)
# head(base.nona)

#creation  temperatures retardees
Temp %>%
  plyr::mutate(tmoy1=lag(meteo, 1))%>%
  plyr::mutate(tmoy2=lag(tmoy, 2))%>%
  plyr::mutate(tmoy3=lag(tmoy, 3))%>%
  plyr::mutate(tmoy4=lag(tmoy, 4))%>%
  plyr::mutate(tmoy5=lag(tmoy, 5))%>%
  plyr::mutate(tmoy6=lag(tmoy, 6))%>%
  plyr::mutate(tmoy7=lag(tmoy, 7))->Temp



#relation lin?aire entre la temp?rature et la conso totale

# mod?les lin?aires avec jours, mois et les temp?ratures retard?es----

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
# les r?sidus vs Fitted ont une structure

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

# 2) Cr?ation de l'?chantillon test, 2/3 individus nrow(base.temp)
set.seed(1)
dim<-nrow(base.temp)
train=sample(dim,2*dim/3,replace=FALSE)

# 3.a) On tente de mod?liser la conso TOTAL par les temp?ratures retard?es jusqu'? 7 JOURS
#    Pour chaque complexit? de 1 ? 8, on s?lectionne le meilleur mod?le
#    gr?ce ? une forward selection (sur le training set)
#    On va donc obtenir 8 mod?les comprenant entre une et 7 variables
best_models8=regsubsets(TOTAL~.,data=base.temp[train,],nvmax=8,method='exhaustive')


# 4) On a 8 mod?les, il faut choisir le meilleur. Pour ce faire, on va appliquer
#    chacun des mod?les sur le test set et calculer la MSE
mse=rep(NA,8)
test=model.matrix(TOTAL~.,data=base.temp[-train,])
for(i in 1:8){
  coefi=coef(best_models8,id=i)
  pred=test[,names(coefi)]%*%coefi
  mse[i]=mean((base.temp$TOTAL[-train]-pred)^2)
}

# 5) Ici on plot les RMSE des 8 mod?les sur le training et sur le test set
#    On choisit le mod?le qui a la RMSE la plus petite sur le test set: c'est celui avec 2 variables
plot(sqrt(mse),ylab='Root MSE des 8 mod?les',pch=19,type='b')


# 6) Pour acc?der aux coefficient du mod?le 2, on appelle la fonction coeff
#      Pour acc?der aux RSS des mod?les, on lance la fonction summary
coef(best_models8,2)
summary(best_models8)$rss


#selection variables regsubset sur temperatures retardees, methode FORWARD----
#install.packages('leaps')
#install.packages('ISLR')
library(ISLR)
library(leaps)

# 2) Cr?ation de l'?chantillon test, 2/3 individus nrow(base.temp)
set.seed(1)
dim<-nrow(base.temp)
train=sample(dim,2*dim/3,replace=FALSE)

# 3.a) On tente de mod?liser la conso TOTAL par les temp?ratures retard?es jusqu'? 7 JOURS
#    Pour chaque complexit? de 1 ? 8, on s?lectionne le meilleur mod?le
#    gr?ce ? une forward selection (sur le training set)
#    On va donc obtenir 8 mod?les comprenant entre une et 7 variables
best_models8_forward=regsubsets(TOTAL~.,data=base.temp[train,],nvmax=8,method='forward')


# 4) On a 8 mod?les, il faut choisir le meilleur. Pour ce faire, on va appliquer
#    chacun des mod?les sur le test set et calculer la MSE
mse.fw=rep(NA,8)
test=model.matrix(TOTAL~.,data=base.temp[-train,])
for(i in 1:8){
  coefi=coef(best_models8_forward,id=i)
  pred=test[,names(coefi)]%*%coefi
  mse.fw[i]=mean((base.temp$TOTAL[-train]-pred)^2)
}


# 5) Ici on plot les RMSE des 8 mod?les sur le training et sur le test set
#    On choisit le mod?le qui a la RMSE la plus petite sur le test set: c'est celui avec 2 variables
plot(sqrt(mse.fw),ylab='Root MSE des 8 mod?les FW',pch=19,type='b')


# 6) Pour acc?der aux coefficient du mod?le 2, on appelle la fonction coeff
#      Pour acc?der aux RSS des mod?les, on lance la fonction summary
coef(best_models8_forward,2)
coef(best_models8,2)

summary(best_models8)
summary(best_models8_forward)

summary(best_models8_forward)$rss

#quand la base de donn?es ne comporte que les temp?ratures c'est le mod?le avec 2 VARIABLES qui a la plus petite MSE
#quand la base de donn?es comporte aussi les mois, c'est le mod?le ? 6 VARIABLES qui a la plus petite MSE


#selection variables regsubset sur temperatures retardees, methode BACKWARD PB----
#install.packages('leaps')
#install.packages('ISLR')
library(ISLR)
library(leaps)

# 2) Cr?ation de l'?chantillon test, 2/3 individus nrow(base.temp)
set.seed(1)
dim<-nrow(base.temp)
train=sample(dim,2*dim/3,replace=FALSE)

# 3.a) On tente de mod?liser la conso TOTAL par les temp?ratures retard?es jusqu'? 7 JOURS
#    Pour chaque complexit? de 1 ? 8, on s?lectionne le meilleur mod?le
#    gr?ce ? une forward selection (sur le training set)
#    On va donc obtenir 8 mod?les comprenant entre une et 7 variables
best_models8.BW=regsubsets(TOTAL~.,data=base.temp[train,],nvmax=8,method='backward')


# 4) On a 8 mod?les, il faut choisir le meilleur. Pour ce faire, on va appliquer
#    chacun des mod?les sur le test set et calculer la MSE
mse.BW=rep(NA,8)
test=model.matrix(TOTAL~.,data=base.temp[-train,])
for(i in 1:8){
  coefi=coef(best_models8.BW,id=i)
  pred=test[,names(coefi)]%*%coefi
  mse[i]=mean((base.temp$TOTAL[-train]-pred)^2)
}

# 5) Ici on plot les RMSE des 8 mod?les sur le training et sur le test set
#    On choisit le mod?le qui a la RMSE la plus petite sur le test set: c'est celui avec 2 variables
plot(sqrt(mse.BW),ylab='Root MSE des 8 mod?les',pch=19,type='b')


# 6) Pour acc?der aux coefficient du mod?le 2, on appelle la fonction coeff
#      Pour acc?der aux RSS des mod?les, on lance la fonction summary
coef(best_models8_forward,2)
summary(best_models8_forward)$rss


#quand la base de donn?es ne comporte que les temp?ratures c'est le mod?le avec 2 VARIABLES qui a la plus petite MSE
#quand la base de donn?es comporte aussi les mois, c'est le mod?le ? 6 VARIABLES qui a la plus petite MSE




# random forest sur base totale----

library(randomForest)

set.seed(1)
fit.all <- randomForest(TOTAL~ ., data = base.sv, na.action = na.roughfix)
# par defaut mtry=sqr(nb variables) soit 4 ie nb de variables test?es ? chaque division 
print(fit.all)
#mean square residuals ? 9557, % var explained ? 98.86%

set.seed(1)
fit.all.2000 <- randomForest(TOTAL~ ., data = base.sv, na.action = na.roughfix,ntree=2000)
print(fit.all.2000)
#mean square residuals ? 9430, % var explained ? 98.87%

# random forest sur base temperatures----

library(randomForest)

set.seed(1)
fit.temp <- randomForest(TOTAL~ ., data = base.temp, na.action = na.roughfix)
# par defaut mtry=sqr(nb variables) soit 4 ie nb de variables test?es ? chaque division 
print(fit.temp)
#mean square residuals ? 50492, % var explained plus faible ? 93.96%

fit.temp.2000 <- randomForest(TOTAL~ ., data = base.temp, na.action = na.roughfix,ntree=2000, mtry=2)
print(fit.temp.2000)
#mean square residuals ? 50135, % var explained ? 94.01%

# LASSO----
# 0) Installation et chargement des librairies nécessaires
install.packages('glmnet')
library(glmnet)

# 1) Les fonctions dont on va se servir prennent en paramètres des matrices et vecteur
#    Variables explicatives : x
#    Variable de réponse : y
x=model.matrix(TOTAL~.-1,data=base.sv) 
y=base.sv$TOTAL

# 2.a) En une ligne, on va effectuer une 10 folds cross validation
#    Pour chacun des 10*k-1 folds on construit  modèles pour 71 valeurs du paramètre lambda
#    Sur les 10 keme fold, on applique nos 71 modèles, on collecte la MSE et on la moyenne
#    Enfin, on plot la moyenne des MSE
#    NB : en réalité cv.glmnet crée 99 modèles pour 99 valeurs de lambda différents
#    seulement, il exclue de lui même les modèles avec un lambda trop proche de zéro
#    qui correspond à une simple régression des moindres carrés
#    NB : alpha=1 pour un modèle LASSO ; alpha=0 pour un modèle RIDGE
lasso_model_cv=cv.glmnet(x,y,alpha=1)
plot(lasso_model_cv)
?cv.glmnet

# 2.b) On récupère le meilleur modèle, celui associé à la MSE la plus faible
#      On voit que certaines variables sont à zéro
#      Le modèle LASSO fait de la selection de variables
numero_du_best_model=which(lasso_model_cv$lambda==lasso_model_cv$lambda.min)
lasso_model_cv$glmnet.fit$beta[,numero_du_best_model]