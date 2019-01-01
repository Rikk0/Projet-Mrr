rm(list=ls())
###Extraction et Analyse des données

tab = data.frame(read.table("FA.dat", sep = "", header = TRUE))
#suppression des données eronnées apprise grâce aux notes spéciales du jeu de donnée
tab = tab[-c(48, 76, 96,182), -1]

#modification d'une donnée par rapport aux notes spéciales du jeu de donnée
tab$Height[42] = 69.5


###Régression
library(corrplot)
corrplot(cor(tab))

###préparation
library(glmnet)

tab2= tab[-5]
summary(tab$Weight)

Y= tab$Weight

x=as.matrix(data.frame(tab2))

##lasso
modlasso = glmnet(x, tab$Weight, alpha=1, family="gaussian")
plot(modlasso, xvar="lambda")
##attributes(modlasso)
##Les coefficients
coef(modlasso)[, 10]

##Crossvalidation
cvModlasso= cv.glmnet(x,y= Y, alpha=1)
plot(cvModlasso)

bestLambdaLasso = cvModlasso$lambda.min
##bestLambdaLasso
##[1] 0.1237602

##############################Ridge
modridge = glmnet(x, tab$Weight, alpha=0, family="gaussian")
plot(modridge, xvar="lambda")
##Coefficients
coef(modridge)[, 10]

##Crossvalidation
cvModridge= cv.glmnet(x,y= Y, alpha=0)
plot(cvModridge)

bestLambdaRidge = cvModlasso$lambda.min
##bestLambdaRidge
##[1] 0.1237602


