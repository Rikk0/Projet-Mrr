###Extraction et Analyse des données
rm(list=ls());
tab = data.frame(read.table("/home/hanna_benj/Documents/semestre5/MRR/Projet-Mrr/FA.dat", sep = "", header = TRUE))
#suppression des données eronnées apprise grâce aux notes spéciales du jeu de donnée
tab = tab[-c(48, 76, 96,182),]
#modification d'une donnée par rapport aux notes spéciales du jeu de donnée
tab$Height[42] = 69.5

#Repérage de valeurs abérantes à enlever
boxplot(tab)
#pour la colonne Weight nous avons deux valeurs abérante à enlever
m = which(grepl(max(tab$Weight),tab$Weight))
tab2 = tab[-c(m),]
boxplot(tab2)
m = which(grepl(max(tab2$Weight),tab2$Weight))
tab2 = tab2[-c(m),]
boxplot(tab2)
# pour la colonne Percent_body_fat_Brozek_equation nous avons 1 valeur abérrante
m = which(grepl(max(tab2$Percent_body_fat_Brozek_equation),tab2$Percent_body_fat_Brozek_equation))
tab2 = tab2[-c(m),]
boxplot(tab2)
#cette valeur était aussi une valeur abérante pour Percent_body_fat_using_Siri_equation, Density, Adiposity_index, Abdomen_circumference
#pour Adiposity_index
m = which(grepl(max(tab2$Adiposity_index),tab2$Adiposity_index))
tab2 = tab2[-c(m),]
boxplot(tab2)
#pour Fat free Weight
m = which(grepl(max(tab2$Fat_Free_Weight),tab2$Fat_Free_Weight))
tab2 = tab2[-c(m),]
boxplot(tab2)
m = which(grepl(max(tab2$Fat_Free_Weight),tab2$Fat_Free_Weight))
tab2 = tab2[-c(m),]
boxplot(tab2)
m = which(grepl(max(tab2$Fat_Free_Weight),tab2$Fat_Free_Weight))
tab2 = tab2[-c(m),]
boxplot(tab2)
# pour Neck_circumference
m = which(grepl(min(tab2$Neck_circumference),tab2$Neck_circumference))
tab2 = tab2[-c(m),]
boxplot(tab2)
m = which(grepl(min(tab2$Neck_circumference),tab2$Neck_circumference))
tab2 = tab2[-c(m),]
boxplot(tab2)
# pour Chest_circumference
m = which(grepl(max(tab2$Chest_circumference),tab2$Chest_circumference))
tab2 = tab2[-c(m),]
boxplot(tab2)
# pour Hip_circumference Quand on enlève des données celà crée des données abérantes pour d'autres variables
# pour Thigh_circumference (ajoute aussi des valeurs abérantes)
m = which(grepl(max(tab2$Thigh_circumference),tab2$Thigh_circumference))
tab3 = tab2[-c(m),]
boxplot(tab3)

#Knee circ
m = which(grepl(max(tab2$Knee_circumference),tab2$Knee_circumference))
tab4 = tab2[-c(m),]
boxplot(tab4)

#Forearm_circumference
m = which(grepl(max(tab2$Forearm_circumference),tab2$Forearm_circumference))
tab2 = tab2[-c(m),]
boxplot(tab2)
m = which(grepl(min(tab2$Forearm_circumference),tab2$Forearm_circumference))
tab2 = tab2[-c(m),]
boxplot(tab2)
m = which(grepl(min(tab2$Forearm_circumference),tab2$Forearm_circumference))
tab2 = tab2[-c(m),]
boxplot(tab2)
#Wrist circumference
m = which(grepl(max(tab2$Wrist_circumference),tab2$Wrist_circumference))
tab2 = tab2[-c(m),]
boxplot(tab2)
m = which(grepl(min(tab2$Wrist_circumference),tab2$Wrist_circumference))
tab2 = tab2[-c(m),]
boxplot(tab2)

###Régression
library(corrplot)
corrplot(cor(tab))
