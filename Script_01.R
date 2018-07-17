###################################################################
######                                                        #####
######                  Prédiction de churn                   #####
######                  secteur des telecoms                  #####
######                Simon CORDE 20 Juin 2018                #####
######                                                        #####
###################################################################


rm(list = ls())

### Installation des packages R

# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("randomForest")
# install.packages("ROCR")
# install.packages("caret")
# install.packages("kernlab")
# install.packages("gbm")
# install.packages("e1071")
# install.packages("adabag")
# install.packages("AUC")
# install.packages("data.table")
# install.packages("glmnet")
# install.packages("doMC") # probleme non available 
# install.packages("dummies")
# install.packages("pROC")

### Chargement des librairies R

library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(ROCR)
library(gbm)
library(e1071)
library(AUC)
library(data.table)
library(MASS)
library(glmnet)
library(doMC)
library(dummies)
library(scales)
library(pROC)

getwd()

##################################
### 1) Importation des données ###
##################################

data <- read.csv(file = "Data/telco.csv")

str(data)

View(head(data))
# Nous avons un data set composé de 1 000 observations et 23 variables
# La variable d'intérêt que nous souhaitons prédire est Churn (Oui / Non)
# Nous sommes donc dans un problème de classification

apply(data, 2, function(x) any(is.na(x)))
# ==> Aucune variable ne comporte de NA

summary(data)

# Nous disposons de 23 variables:
  # 1 variable d'intéret que nous souhaiton sprédire: Churn
  # 5 variables catégorielles: région, statut marital, éducation, sexe, catégorie client)
  # 11 variables indicatrices (Oui/Non) : location matériel, Internet, double appel, ...
  # 6 variables numériques: nombre de mois d'ancienneté, âge, revenu, ...

######################################################
### 2) Split échantillon d'apprentissage / de test ###
######################################################

### Pour le caret package
#levels <- unique(data$churn) 
#data$churn <- factor(data$churn, labels=make.names(levels))

set.seed(100)

number_rows <- nrow(data)
rows <- c(1:number_rows)

index_training_set <- sample(x = rows, size = 0.7*number_rows, replace = FALSE)

training_set <- data[index_training_set,]
test_set <- data[-index_training_set,]

nrow(training_set) # notre échantillon d'apprentissage comprend 700 observations
nrow(test_set) # notre échantillon de test comprend 300 observations

prop.table(table(data$Churn))
prop.table(table(training_set$Churn))
prop.table(table(test_set$Churn))
# Le taux d'attrition sur les échantillons d'apprentissage 
# et de test semble consistent

####################################
### 3) Statistiques Descriptives ###
####################################

prop.table(table(training_set$Churn))

# Près de 28% des clients ont résilié leur contrat dans la base d'apprentissage

# On peut identifier les variables impactant le risque d'attrition en:
  # - implémentant un test du CHI deux afin tester lhypothèse nulle
  # d'indépendance entre une vraible catégorielle et le churn
  # - analysant la rpéartition du churn selon les variables à l'aide 
  # de diagrammes en barre par exemple

table(training_set$Region)
chisq.test(table(training_set$Churn, training_set$Region))
# ==> On ne peut pas rejetter l'hypothèse nulle d'indépendance

# variable churn

training_set %>% 
  ggplot() +
  aes(x = Churn) +
  geom_bar(width = 0.5, fill = "#56B4E9") +
  ggtitle("Taux d'attrition dans l'échantillon d'apprentissage") +
  xlab("Churn") +
  ylab("Fréquence") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Output/churn_rate_1.png", plot = last_plot())

training_set %>% 
  ggplot() +
  aes(x = Churn) +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill = "#56B4E9") +
  ggtitle("Taux d'attrition dans l'échantillon d'apprentissage") +
  xlab("Churn") +
  ylab("Fréquence") + 
  theme_bw() +
  #geom_text(aes(label=round(prop.table(table(training_set$Churn)), 3)), vjust= - 1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = percent)

ggsave("Output/churn_rate_2.png", plot = last_plot())


training_set %>% 
  ggplot() +
  aes(x = Region, fill = Churn) +
  geom_bar(width = 0.5) +
  ggtitle("Attrition selon la région") +
  xlab("Région") +
  ylab("Fréquence") + 
  theme_bw() +
  #geom_text(aes(label=round(prop.table(table(training_set$Churn)), 3)), vjust= - 1) +
  theme(plot.title = element_text(hjust = 0.5)) 

# La région du client ne semble pas impacter l'attrition

training_set %>% 
  ggplot() +
  aes(x = StatutMarital, fill = Churn) +
  geom_bar(width = 0.5) +
  ggtitle("Attrition selon le statut marital") +
  xlab("Statut marital") +
  ylab("Fréquence") + 
  theme_bw() +
  #geom_text(aes(label=round(prop.table(table(training_set$Churn)), 3)), vjust= - 1) +
  theme(plot.title = element_text(hjust = 0.5)) 

# Le statut marital ne semble pas influer sur le risque d'attrition

training_set %>% 
  ggplot() +
  aes(x = Retraite, fill = Churn) +
  geom_bar(width = 0.5) +
  ggtitle("Attrition selon le statut retraité ou non") +
  xlab("Retraite") +
  ylab("Fréquence") + 
  theme_bw() +
  #geom_text(aes(label=round(prop.table(table(training_set$Churn)), 3)), vjust= - 1) +
  theme(plot.title = element_text(hjust = 0.5)) 

# Un client retraité ne semble pas être exposé à un risque d'attrition contrairement
# aux clients actifs


training_set %>% 
  ggplot() +
  aes(x = Sexe, fill = Churn) +
  geom_bar(width = 0.5) +
  ggtitle("Attrition selon sexe") +
  xlab("Sexe") +
  ylab("Fréquence") + 
  theme_bw() +
  #geom_text(aes(label=round(prop.table(table(training_set$Churn)), 3)), vjust= - 1) +
  theme(plot.title = element_text(hjust = 0.5)) 

# Il semble y avoir un risque plus élevé d'attrition chez les hommes mais peu
# significatif

training_set %>% 
  ggplot() +
  aes(x = PaieElectronique, fill = Churn) +
  geom_bar(width = 0.5) +
  ggtitle("Attrition selon le mode de paiement") +
  xlab("Paiement électronique") +
  ylab("Fréquence") + 
  theme_bw() +
  #geom_text(aes(label=round(prop.table(table(training_set$Churn)), 3)), vjust= - 1) +
  theme(plot.title = element_text(hjust = 0.5)) 

# Le mode de paiement électronique est associé à un fort risque d'attrition
# Ceci peut être expliqué par le fait que ces clients utilisent souvent internet
# les applications mobiles et vont davanatge comparer les offres d'abonnement 
# téléphonique entre les différents opérateurs. Ils ont ainsi tendance à 
# partir à la concurrence.

training_set %>% 
  ggplot() +
  aes(x = LocMateriel, fill = Churn) +
  geom_bar(width = 0.5) +
  ggtitle("Attrition selon le mode d'acquisition du matériel") +
  xlab("Location du matériel") +
  ylab("Fréquence") + 
  theme_bw() +
  #geom_text(aes(label=round(prop.table(table(training_set$Churn)), 3)), vjust= - 1) +
  theme(plot.title = element_text(hjust = 0.5)) 

# ==> les clients qui louent leur matériel sont soumis à un risque d'attrition
# supérieur à ceux qui ne louent pas. 

training_set %>% 
  ggplot() +
  aes(x = CatClient, fill = Churn) +
  geom_bar(width = 0.5) +
  ggtitle("Attrition selon le mode d'acquisition du matériel") +
  xlab("Location du matériel") +
  ylab("Fréquence") + 
  theme_bw() +
  #geom_text(aes(label=round(prop.table(table(training_set$Churn)), 3)), vjust= - 1) +
  theme(plot.title = element_text(hjust = 0.5)) 

# ==> La catégorie de clients la moins impacté par l'attrition est Service Plus

############################
### 4) Machine Learning  ###
############################

##################
### Avec CARET ###
##################

set.seed(42)

nrow(training_set)
nrow(test_set)


#####################################
### Modèles Linéaires Généralisés ###
###    Régression Logistique      ###
#####################################

fitControl <- trainControl(method = "cv", # cross-validation
                           number = 5, # kfolds -> 5
                           summaryFunction = twoClassSummary, # if we want maximise ROC
                           classProbs = TRUE) 

tic <- Sys.time()
# Apprentissage du modèle
model_glm <- train(Churn ~ .,
                   data = training_set, 
                   method = "glm",
                   metric = "ROC",
                   preProcess = c("center", "scale"),
                   trControl = fitControl)


tac <- Sys.time()
print(paste("The model training has taken ", as.character(round(difftime(tac, tic, units = "secs"), 2)), " seconds to run", sep = ""))

#summary(model_glm)


# Prediction of probabilities on the test set

predictions_glm <- predict(object = model_glm, newdata = test_set, type = "prob")[,2]
str(predictions_glm)
summary(predictions_glm)

roc(test_set$Churn, predictions_glm)$auc

# Si nous mettons un seuil à 0.5
class_predictions_glm <- ifelse(predictions_glm > 0.5, "Oui", "Non")

mean(class_predictions_glm == test_set$Churn)
# 77.3 % d'accuracy

# Si nous utilisons un modèle qui prédit l'attrition pour chaque client
mean(rep("Non", nrow(test_set)) == test_set$Churn)
# 70.7 % of accuracy

#########################
### Forêts aléatoires ###
#########################

fitControl <- trainControl(method = "cv", # cross-validation
                           number = 5, # kfolds -> 5
                           summaryFunction = twoClassSummary, # if we want maximise ROC
                           classProbs = TRUE) 

grid_search <- expand.grid(mtry = c(7:15), 
                           splitrule = c("gini", "extratrees"), 
                           min.node.size = seq(0, 30, 5))

tic <- Sys.time()
# Apprentissage du modèle
model_rf <- train(Churn ~ .,
                  data = training_set, 
                  method = "ranger",
                  metric = "ROC",
                  trControl = fitControl,
                  tuneGrid = grid_search)

tac <- Sys.time()
print(paste("The model training has taken ", as.character(round(difftime(tac, tic, units = "secs"), 2)), " seconds to run", sep = ""))


#summary(model_rf)
model_rf$bestTune
#model_rf$results

# Prediction of probabilities on the test set
predictions_rf <- predict(object = model_rf, newdata = test_set, type = "prob")[,2]
str(predictions_rf)
summary(predictions_rf)

roc(test_set$Churn, predictions_rf)$auc

################################################################
### 5) Benchmark de la performance prédictive de nos modèles ###
################################################################

# Nous allons mesurer la performance prédictive des modèles à l'aide
# de la courbe ROC et l'AUC (aire sous la courbe) car cette dernière permet
# d'avoir une mesure objective d'autant plus si les courbes ROC se croisent

#####################
###  Courbes ROC  ###
#####################

roc_glm <- roc(test_set$Churn, predictions_glm)
roc_rf <- roc(test_set$Churn, predictions_rf)

ggroc(data = list(roc_glm, roc_rf), legacy.axes = TRUE) +
  ggtitle("Courbe ROC") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") + 
  scale_color_manual(name = "Modèles Statistiques", labels = c("Régression Logistique", "Forêts Aléatoires"), values = c("blue", "green")) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), lty = "dashed", color = "grey") +
  theme(legend.position = c(0.1, 0.2)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggsave("Output/roc_curve.png", plot = last_plot())

####################
### Métrique AUC ###
####################

metric_auc <- c(roc_glm$auc, roc_rf$auc)

ggplot() +
  aes(x = c("Régression Logistique", "Forêts Aléatoires"), y = metric_auc) +
  geom_col(fill = "#56B4E9", width  = 0.6) +
  ggtitle("Area Under the Curve") +
  xlab("Modèles statistiques") +
  ylab("AUC") + 
  coord_cartesian(ylim = c(0.5, 1)) +
  geom_text(aes(label=round(metric_auc, 3)), vjust= - 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggsave("Output/auc.png", plot = last_plot())


