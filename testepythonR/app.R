library(shiny)
library(reticulate)
reticulate::py_module_available("sklearn")

# Spécifie l'environnement Python

# Importer les bibliothèques Python nécessaires
sklearn <- import("sklearn")
metrics <- import("sklearn.metrics")
model_selection <- import("sklearn.model_selection")
linear_model <- import("sklearn.linear_model")
ensemble <- import("sklearn.ensemble")

# Interface Utilisateur
ui <- fluidPage(
  titlePanel("Modélisation avec scikit-learn"),
  sidebarLayout(
    sidebarPanel(
      selectInput("model_choice", "Choisissez un modèle :", 
                  choices = c("Régression Logistique", "Forêt Aléatoire")),
      actionButton("train", "Entraîner le modèle")
    ),
    mainPanel(
      h3("Métriques du modèle"),
      verbatimTextOutput("model_metrics")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Charger et diviser les données
  data <- reactive({
    # Exemple simple de données
    df <- mtcars
    target <- ifelse(df$mpg > 20, 1, 0)  # Convertir mpg en binaire (1 ou 0)
    df$mpg <- NULL
    list(X = as.matrix(df), y = as.integer(target))
  })
  
  split_data <- reactive({
    req(data())
    train_test_split <- model_selection$train_test_split
    train_test_split(data()$X, data()$y, test_size = 0.3, random_state = 42)
  })
  
  # Entraîner un modèle
  model <- eventReactive(input$train, {
    req(split_data())
    splits <- split_data()
    X_train <- splits[[1]]
    X_test <- splits[[2]]
    y_train <- splits[[3]]
    y_test <- splits[[4]]
    
    if (input$model_choice == "Régression Logistique") {
      model <- linear_model$LogisticRegression()
    } else if (input$model_choice == "Forêt Aléatoire") {
      model <- ensemble$RandomForestClassifier()
    }
    
    model$fit(X_train, y_train)
    
    list(model = model, X_test = X_test, y_test = y_test)
  })
  
  # Calculer les métriques
  output$model_metrics <- renderPrint({
    req(model())
    fitted_model <- model()$model
    X_test <- model()$X_test
    y_test <- model()$y_test
    
    y_pred <- fitted_model$predict(X_test)
    accuracy <- metrics$accuracy_score(y_test, y_pred)
    precision <- metrics$precision_score(y_test, y_pred, average = "binary")
    recall <- metrics$recall_score(y_test, y_pred, average = "binary")
    f1 <- metrics$f1_score(y_test, y_pred, average = "binary")
    
    list(
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall,
      F1_Score = f1
    )
  })
}

# Lancer l'application
shinyApp(ui, server)
