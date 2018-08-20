####################################################################################################################
###########                                                                                              ###########
###########                                   Prise en main de R                                         ###########
###########                                                                                              ###########
####################################################################################################################

###
rm(list = ls()) # permet de nettoyer l'environnement

getwd()# ==> répertoire de travail

setwd()# si l'on souhaite changer le répertoire de travail


####################
###              ###
###  Variables   ###
###              ###
####################

###############################
### 1) Variables numériques ###
###############################


var <- 1

class(var)

ls() # ==> permet de lister l'ensemble des objets
rm(var) # permet de supprimer un objet

### Opérations numériques

a <- 3
b <- 4

# addition
c <- a + b
c

# soustraction
d <- a - b
d

# multiplication
e <- a * b
e

# division
f <- a / b
f

# puissance
g <- a ^ b
g

g <- a ** b
g

###############################
### 2) Variables caractères ###
###############################

phrase_1 <- "Intelligence artificielle"
phrase_2 <- "et le big data"


phrase_3 <- paste(phrase_1, phrase_2, sep = " ")
phrase_3

class(phrase)


#############################
### 3) Variables booléens ###
#############################

nombre <- 45

test <- nombre == 43
class(test)

nombre != 43

nombre > 0

nombre == 43 & nombre > 0

nombre == 43 | nombre > 0



####################
###              ###
###   Vecteurs   ###
###              ###
####################

vecteur_1 <- c(1,2,6,8)
vecteur_2 <- c(4,7,2,2)

sum(vecteur_1)
mean(vecteur_1)
sd(vecteur_1)

vecteur[3] # 3ème élément du vecteur

class(vecteur_1)
length(vecteur_1)

vecteur_3 <- vecteur_1 - vecteur_2
vecteur_3

# produit nombre à nombre
vecteur_1 * vecteur_2

# produit vectoriel
vecteur_1 %*% vecteur_2

# concaténation de vecteurs
vecteur_4 <- c(vecteur_1, vecteur_2)
vecteur_4

####################
###              ###
###   Matrices   ###
###              ###
####################

matrice_A <- matrix(data = vecteur_1, nrow = 2, ncol = 2)
matrice_A

matrice_A[1, 2] # élément de la 1ère ligne et de la 2ème colonne
matrice_A[1,] # éléments de la 1ère ligne

matrice_B <- matrix(data = vecteur_2, nrow = 2, ncol = 2, byrow = T)
matrice_B

# transposée
t(matrice_A)

# produit nombre à nombre
matrice_A * matrice_B


# produit matriciel
matrice_A %*% matrice_B

dim(matrice_A)

is.matrix(matrice_A)

numbers <- c(4, 8)
# rajout de lignes
matrice_C <- rbind(matrice_B, numbers)
matrice_C
dim(matrice_C)

numbers <- c(4, 8, 12)
# rajout de colonnes
matrice_D <- cbind(matrice_C, numbers)
matrice_D
dim(matrice_D)

colnames(matrice_D)
rownames(matrice_D)


rowMeans(matrice_D)
colMeans(matrice_D)


matrice_E <- matrix(data = c("a", "z", "e", "r", "t", "y"), nrow = 3, ncol = 2)
matrice_E

cbind(matrice_E, numbers) # ==> tout est converti en type string

####################
###              ###
###    Listes    ###
###              ###
####################

# Les vecteurs vus précédemment avaient l'inconvénient de ne pouvoir être constitués d'éléments appartenant 
# au même type (numeric, character ou logical)
# Les listes permettent de pallier à cette limite en permettant le stockage d'élements divers

premiere_liste <- list(c(1, 23, 98, 64, 5), phrase = "Deep learning", rep(c(FALSE, TRUE), times = 2))
premiere_liste

class(premiere_liste)
names(premiere_liste)

premiere_liste[[1]]
premiere_liste[[2]]
premiere_liste[[3]]

premiere_liste[[1]][4]
premiere_liste[[3]][4]

langages <- c("R", "Python", "Scala", "SQL")

deuxieme_liste <- append(premiere_liste, langages)
deuxieme_liste

premiere_liste[[4]] <- langages
premiere_liste

# Une liste peut elle même contenir ... une autre liste !

troisieme_liste <- list(premiere_liste, deuxieme_liste)
length(troisieme_liste)


str(troisieme_liste)

################################
###                          ###
###    Tableaux de données   ###
###                          ###
################################

# De la même manière, on a vu qu'une matrice ne pouvait stocker uniquement que des élements du même type
# On va maintenant utliser l'objet data frame qui nous permettra de stocker des observations possédant des 
# attributs (= variables) de différente nature (Exemple:  age ==> numérique, sexe: variable qualitative, ...)
# Les observations seront en ligne tandis que les variables seront en colonnes

donnees <- iris

str(donnees)
# On a 150 observations (= lignes) et 5 variables ( = colonnes)
names(donnees)

donnees[2,5] # valeur de la 5 ème variable (variable espèce) pour la 2ème observation

donnees$Species[2]

nrow(donnees)
ncol(donnees)

################################
###                          ###
###    Conditions logiques   ###
###                          ###
################################

# On va tester si un nombre est pair ou non


nombre <- 41

if (!is.numeric(nombre) | round(nombre, digits = 0)!= nombre){
  
  print("Veuillez entrer un nombre entier !")
  
}else if (nombre %% 2 == 0) { # reste de la division par deux
  
  print("Le nombre est pair !")
  
}else{
  
  print("Le nombre est impair !")
  
}

################################
###                          ###
###   Boucles For et While   ###
###                          ###
################################


###############
###   For   ###
###############

# On va chercher à calculer la somme des n premiers chiffres
n <- 50

somme <- 0

for (i in 1:50){
  
  somme <- somme + i
  
}

somme 

# vérification

somme_bis <- n*(n+1)/2
somme_bis


#################
###   While   ###
#################

# On tire un nombre réel tiré d'une loi normale centrée réduite tant qu'on ne dépasse pas 2

resultats <- c()
nombre_aleatoire <- 0
  
while (nombre_aleatoire < 2){
  
  nombre_aleatoire <- rnorm(1, mean = 0, sd = 1)
  resultats <- c(resultats, nombre_aleatoire)
  
}

resultats

#################################
###                           ###
###   Ecrire une fonction R   ###
###                           ###
#################################

mettre_au_carre <- function(nombre) {
  
  carre <- nombre ^ 2
  return(carre)
  
}

mettre_au_carre(6)

######################
###                ###
###   Packages R   ###
###                ###
######################

# Installer un package
install.packages("dplyr")

# Charger un package pour pouvoir l'utiliser
library(dplyr)

# différence entre require et library 
# require renvoie un booléen si le package a bien été chargé (utile à l'intérieur de fonctions pour les messages
# d'erreur) tandis que library renvoie un erreur si le package n'est pas trouvé

# Chemin des packages dans lesquels R va chercher les packages
.libPaths()
