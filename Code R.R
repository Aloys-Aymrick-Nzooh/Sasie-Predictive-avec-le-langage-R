                                                #Saisie Prédictive
  
  
library(NLP) ##Natural Language Processing 
library(tm) ##Pour le traitement de du dictionaire


--------------------------------------------------------------------------
  #Preparation du dictionnaire pour les n-grams
--------------------------------------------------------------------------
 # 1. Charger la bibliotheque

#Chargement de la bibliotheque dans un vcorpus du package tm

Data1 <- VCorpus(DirSource("C:/Users/aymer/OneDrive/Desktop/ESSFAR/Projet ESSFAR/R/Projet R 2022/App/Bibliotheque"))

#Convertir en minuscule

Data2 <- tm_map(Data1, content_transformer(tolower))


#Enlever les ponctuation

Data3 <- tm_map(Data2, removePunctuation)



###Maintenant notre dictionaire est pret pour la modalisation en n-grams

###Pour proceder nous allons utiliser le package "Rweka" qui neccesite un environnement java pour pouvoir etre execute

#2. Transformation en Bi-gram

library("RWeka")

#Convertir le dictionnaire en Bigram

##Creation d'une fonction bigram tokenizer qui convertira une chaine en bigram a l'aide de la fonction weka_control disponible sur le package RWeka
#Creation d'une fonction bigram tokenizer qui convertira une chaine en bigram a l'aide de la fonction weka_control disponible sur le package RWeka

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
##Nous appliquons maintenant la fonction BigramTokenizer sur l'ensemble des phrases dans notre corpus et nous conertissons les résultats en une matrice DocumentTerm


#3. Frequence de chaque token
#Nous appliquons maintenant la fonction BigramTokenizer sur l'ensemble des phrases dans notre corpus et nous convertissons les résultats en une matrice DocumentTerm

Data_bigrams <- DocumentTermMatrix(Data3, control = list(tokenize = BigramTokenizer))
inspect(Data_bigrams)

##Comme nous pouvons le remarquer les colonnes contient des bigrams et leurs frequence


bigram_frequency <- sort(colSums(as.matrix(Data_bigrams)), 
                         decreasing=TRUE)

##Maintenant nous avons la frequence de chaque bigram en ordre decroissant en utilisant la fonction sort 

#Nous convertissons le tableau des fréquence en data frame pour pouvoir faciliter l'analyse

bigram_df <- data.frame(bigrams=names(bigram_frequency), 
                        freq=bigram_frequency)
head(bigram_df)


#Maintenant, nous allons créér une base de données bigram avec ce data frame
#Ici pour chaque ligne nous allons séparer les bigram en premier et deuxieme mot puis le stocker dans le meme data frame

for ( i in 1:nrow(bigram_df)) 
{
  grams = unlist(strsplit(as.character(bigram_df$bigrams[i])," "))
  
  bigram_df$first[i]= grams[1]
  bigram_df$second[i]= grams[2]
}
bigram_df[1:10,]

#Testons notre bigram

#Nous allons prédire le qui suit apres "back"

#Nous allons filtrer le dataframe ou le premier mot est "back"
mot_filtre = bigram_df[
  bigram_df$first == "back", 
  c("freq", "second")]

#Ordre de frequence decroissante
prochain_mot = mot_filtre[
  with(mot_filtre, order(-freq)), ]

#Le prochaine mot prédit
(prochain_mot$second)

#Donc nous avons la liste des mots les plus probables apres le mot back

