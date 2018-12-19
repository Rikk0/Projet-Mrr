###Extraction et Analyse des données
tab = data.frame(read.table("/home/hanna.benjedidia/MRR/projet/FA.dat", sep = "", header = TRUE))
#suppression des données eronnées apprise grâce aux notes spéciales du jeu de donnée
tab[-c(48, 76, 96,182),]
#modification d'une donnée par rapport aux notes spéciales du jeu de donnée
tab$Height[42] = 69.5

###Régression
library(corrplot)
corrplot(cor(tab))
