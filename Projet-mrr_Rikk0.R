rm(list=ls())
library(MASS)
library(ROCR)
library(corrplot)
library(glmnet)
library(hydroGOF)
###Extraction et Analyse des donnes

tab = data.frame(read.table("FA.dat", sep = "", header = TRUE))
#suppression des données eronnées apprise grâce aux notes spéciales du jeu de donnée
tab = tab[-c(48, 76, 96,182),-1]
#modification d'une donnée par rapport aux notes spéciales du jeu de donnée
tab$Height[42] = 69.5

#Repérage de valeurs abérantes à enlever
boxplot(tab)
#pour la colonne Weight nous avons deux valeurs abérante à enlever
m = which(grepl(max(tab$Weight),tab$Weight))
tab = tab[-c(m),]
m = which(grepl(max(tab$Weight),tab$Weight))
tab = tab[-c(m),]
# pour la colonne Percent_body_fat_Brozek_equation nous avons 1 valeur abérrante
m = which(grepl(max(tab$Percent_body_fat_Brozek_equation),tab$Percent_body_fat_Brozek_equation))
tab = tab[-c(m),]
#cette valeur était aussi une valeur abérante pour Percent_body_fat_using_Siri_equation, Density, Adiposity_index, Abdomen_circumference
#pour Adiposity_index
m = which(grepl(max(tab$Adiposity_index),tab$Adiposity_index))
tab = tab[-c(m),]
boxplot(tab)
#pour Fat free Weight
m = which(grepl(max(tab$Fat_Free_Weight),tab$Fat_Free_Weight))
tab = tab[-c(m),]
m = which(grepl(max(tab$Fat_Free_Weight),tab$Fat_Free_Weight))
tab = tab[-c(m),]
m = which(grepl(max(tab$Fat_Free_Weight),tab$Fat_Free_Weight))
tab = tab[-c(m),]
# pour Neck_circumference
m = which(grepl(min(tab$Neck_circumference),tab$Neck_circumference))
tab = tab[-c(m),]
m = which(grepl(min(tab$Neck_circumference),tab$Neck_circumference))
tab = tab[-c(m),]
# pour Chest_circumference
m = which(grepl(max(tab$Chest_circumference),tab$Chest_circumference))
tab = tab[-c(m),]
boxplot(tab)


###R?gression

corrplot(cor(tab))

###pr?paration


tab2= tab[-5]
summary(tab$Weight)

Y= tab$Weight

x=as.matrix(data.frame(tab2))
n = dim(tab)[1]
tabSim = tab[setdiff(1:n,0:floor(n/5)),] 
tabTest = tab[0:floor(n/5),]
tabTest2 = tabTest[-5]
tabSim2= tabSim[-5]
Sim = as.matrix(data.frame(tabSim2))
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
##[1] 0.8081123
modlasso2 = glmnet(Sim, tabSim$Weight, alpha=1, family="gaussian")
cvModlasso2= cv.glmnet(Sim,y= tabSim$Weight, alpha=1)
bestLambdaLasso2 = cvModlasso2$lambda.min
predictLass = predict(modlasso2, as.matrix(tabTest2),s=bestLambdaLasso2,type="class")
rmse(as.numeric(tabTest$Weight),as.numeric(predictLass))
mean((tabTest$Weight-predictLass)^2)
boxplot(predictLass)
##############################Ridge
modridge = glmnet(x, tab$Weight, alpha=0, family="gaussian")
plot(modridge, xvar="lambda")
##Coefficients
coef(modridge)[, 10]

##Crossvalidation
cvModridge= cv.glmnet(x,y= Y, alpha=0)
plot(cvModridge)

bestLambdaRidge = cvModridge$lambda.min
##bestLambdaRidge
##[1]0.8081123

modridge2 = glmnet(Sim, tabSim$Weight, alpha=0, family="gaussian")
cvModridge2= cv.glmnet(Sim,y= tabSim$Weight, alpha=0)
bestLambdaRidge2 = cvModridge2$lambda.min
predictRidge = predict(modridge2, as.matrix(tabTest2),s=bestLambdaRidge2,type="class")
rmse(as.numeric(tabTest$Weight),as.numeric(predictRidge))
boxplot(predictRidge)
boxplot(predictLass, add = TRUE)
###################################Stepwise, forward, backward

library(MASS)
# Fit the full model 
  lmModel = lm(Weight~., data = tab)
# Stepwise regression model
stepModel = stepAIC(lmModel, direction = "both", trace = FALSE)
summary(stepModel)

# forward regression model
forwardModel = stepAIC(lmModel, direction = "forward", trace = FALSE)
summary(forwardModel)

# backward regression model
backwardModel = stepAIC(lmModel, direction = "backward", trace = FALSE)
summary(backwardModel)

