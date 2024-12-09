if (!require(mlbench)) install.packages('mlbench')
if (!require(rpart)) install.packages('rpart')
if (!require(caret)) install.packages('caret')

library(mlbench)
library(rpart)
library(caret)
library(ggplot2)

# Charger les données
data <- read.csv("statlog_heart.csv")

# Préparer la cible et les prédicteurs
target <- "heart.disease"
predictors <- setdiff(colnames(data), target)

# Extraire les données
X <- data[, predictors]
y <- as.factor(data[[target]])

# Séparer les ensembles d'entraînement et de test
train_index <- caret::createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Combiner les données pour rpart
train_data <- data.frame(X_train, Target = y_train)
test_data <- data.frame(X_test, Target = y_test)

# Entraîner le modèle
my_parms <- list(split = 'gini')
rpart_model <- rpart(Target ~ ., data = train_data, method = "class", parms = my_parms)

# Afficher le modèle
print(rpart_model)



rpart.plot(
  rpart_model,
  type = 2,      # Affiche les caractéristiques utilisées à chaque nœud
  extra = 104,   # Montre les proportions et les probabilités
  fallen.leaves = TRUE,  # Affiche les feuilles plus lisiblement
  shadow.col = "gray"    # Ajoute des ombres pour mieux voir la hiérarchie
)




# Prédictions
predictions <- predict(rpart_model, newdata = test_data, type = "class")

# Afficher les prédictions
print(predictions)






# Matrice de confusion
conf_matrix <- table(Predicted = predictions, Actual = test_data$Target)
print(conf_matrix)

# Calcul de l'exactitude
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))




# Précision, Rappel, F1-score avec caret
confusion_matrix <- caret::confusionMatrix(predictions, test_data$Target)
print(confusion_matrix)



if (!require(randomForest)) install.packages('randomForest')
library(randomForest)

rf_model <- randomForest(Target ~ ., data = train_data, ntree = 100)
rf_predictions <- predict(rf_model, newdata = test_data)

# Évaluer la forêt aléatoire
conf_matrix_rf <- table(Predicted = rf_predictions, Actual = test_data$Target)
accuracy_rf <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf)
print(paste("Random Forest Accuracy:", round(accuracy_rf * 100, 2), "%"))


if (!require(pROC)) install.packages('pROC')
library(pROC)


# Obtenir les probabilités pour la classe positive
probs <- predict(rpart_model, newdata = test_data, type = "prob")[, 2]



# Générer l'objet ROC
roc_curve <- roc(test_data$Target, probs)

# Afficher la courbe ROC
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve")





# Calculer et afficher l'AUC
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 2)))

# Ajouter l'AUC à la courbe
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "blue", lwd = 2)























# Entraîner une Forêt Aléatoire
rf_model <- randomForest(Target ~ ., data = train_data, ntree = 100)

# Afficher un arbre individuel
if (!require(rattle)) install.packages('rattle')
library(rattle)
tree <- randomForest::getTree(rf_model, k = 1, labelVar = TRUE)
print(tree)


# Prédire les probabilités et les classes
probs_rf <- predict(rf_model, newdata = test_data, type = "prob")[, 2]
predictions_rf <- predict(rf_model, newdata = test_data)



# Matrice de confusion
conf_matrix_rf <- table(Predicted = predictions_rf, Actual = test_data$Target)
print(conf_matrix_rf)

# Calcul de l'exactitude
accuracy_rf <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf)
print(paste("Accuracy (Random Forest):", round(accuracy_rf * 100, 2), "%"))



# Générer la courbe ROC
roc_curve_rf <- roc(test_data$Target, probs_rf)

# Tracer la courbe ROC
plot(roc_curve_rf, col = "green", lwd = 2, main = "ROC Curve - Random Forest")

# Ajouter l'AUC
auc_rf <- auc(roc_curve_rf)
legend("bottomright", legend = paste("AUC =", round(auc_rf, 2)), col = "green", lwd = 2)












# Entraîner une régression logistique
logistic_model <- glm(Target ~ ., data = train_data, family = "binomial")


# Prédire les probabilités et les classes
probs_logistic <- predict(logistic_model, newdata = test_data, type = "response")
predictions_logistic <- ifelse(probs_logistic > 0.5, "1", "0")




# Matrice de confusion
conf_matrix_logistic <- table(Predicted = predictions_logistic, Actual = test_data$Target)
print(conf_matrix_logistic)

# Calcul de l'exactitude
accuracy_logistic <- sum(diag(conf_matrix_logistic)) / sum(conf_matrix_logistic)
print(paste("Accuracy (Logistic Regression):", round(accuracy_logistic * 100, 2), "%"))


# Générer la courbe ROC
roc_curve_logistic <- roc(test_data$Target, probs_logistic)

# Tracer la courbe ROC
plot(roc_curve_logistic, col = "red", lwd = 2, main = "ROC Curve - Logistic Regression")

# Ajouter l'AUC
auc_logistic <- auc(roc_curve_logistic)
legend("bottomright", legend = paste("AUC =", round(auc_logistic, 2)), col = "red", lwd = 2)

