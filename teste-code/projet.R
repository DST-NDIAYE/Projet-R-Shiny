library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(shinycssloaders)
library(plotly)
library(shinyWidgets)
library(caret)
library(rpart)
library(randomForest)
library(e1071)
library(pROC)




ui <- dashboardPage(
  dashboardHeader(
    title = "Application Shiny",
    dropdownMenu(
      type = "notifications",
      notificationItem(
        text = "3 nouvelles données chargées",
        icon = icon("database"),
        status = "info"
      ),
      notificationItem(
        text = "1 erreur détectée",
        icon = icon("exclamation-triangle"),
        status = "danger"
      )
    ),
    dropdownMenu(
      type = "tasks",
      taskItem(
        text = "Préparation des données",
        value = 60,
        color = "blue"
      ),
      taskItem(
        text = "Modélisation",
        value = 30,
        color = "green"
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "home", icon = icon("home")),
      menuItem("Prétraitement", tabName = "preprocessing", icon = icon("sliders")),
      menuItem("Analyse exploratoire", tabName = "exploration", icon = icon("chart-bar")),
      menuItem("Modélisation", tabName = "modeling", icon = icon("robot"))
    )
  ),
  dashboardBody(
    tabItems(
      # Page Dashboard
      tabItem(
        tabName = "home",
        
        fluidRow(
          column(4, 
                 fileInput(inputId = "file1", label = "Choose CSV File", accept = c(".csv"))),
          column(4, 
                 actionButton(inputId = "go", label = "Charger les données"))
        ),
        
        hr(),
        
        fluidRow(
          infoBoxOutput("rows_columns_info", width = 4),
          infoBoxOutput("missing_values_info", width = 4),
          infoBoxOutput("numeric_categorical_info", width = 4)
        ),
        
      
        
        fluidRow(
          box(
            title = "Résumé des données",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("data_summary")
          )
        ),
        
        fluidRow(
          box(
            title = "Tableau des données",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("demo_datatable") %>% withSpinner(color = "blue")
          )
        )
      ),
      
      # Autres onglets
      tabItem(tabName = "preprocessing", h2("Prétraitement des données")),
      tabItem(tabName = "exploration", h2("Analyse exploratoire")),





      tabItem(
  tabName = "modeling",
  h2("Modélisation supervisée"),
  
  # Choix du modèle
  fluidRow(
    column(4,
           selectInput("model_choice", "Choisissez un modèle :", 
                       choices = c("Régression Logistique", "Arbre de Décision", "Forêt Aléatoire", "SVM"))
    ),
    column(4,
           actionButton("train_model", "Entraîner le modèle")
    )
  ),
  
  hr(),
  
  # Résultats des métriques
  fluidRow(
    box(
      title = "Métriques du modèle",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      verbatimTextOutput("model_metrics")
    )
  ),
  
  # Courbe ROC
  fluidRow(
    box(
      title = "Courbe ROC",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      plotlyOutput("roc_curve")
    )
  )
  
 
)











    )
  )
)

server <- function(input, output, session) {
  # Charger les données
  data <- eventReactive(input$go, {
    req(input$file1)
    read.csv(input$file1$datapath, header = TRUE)
  })
  
  # InfoBox : Lignes et Colonnes
  output$rows_columns_info <- renderInfoBox({
    req(data())
    infoBox(
      title = "Lignes et Colonnes",
      value = paste0(nrow(data()), " lignes, ", ncol(data()), " colonnes"),
      icon = icon("table"),
      color = "blue"
    )
  })
  
  # InfoBox : Valeurs Manquantes
  output$missing_values_info <- renderInfoBox({
    req(data())
    infoBox(
      title = "Valeurs Manquantes",
      value = sum(is.na(data())),
      icon = icon("exclamation-circle"),
      color = "red"
    )
  })
  
  # InfoBox : Types de Variables
  output$numeric_categorical_info <- renderInfoBox({
    req(data())
    num_numeric <- sum(sapply(data(), is.numeric))
    num_categorical <- sum(sapply(data(), is.factor) | sapply(data(), is.character))
    infoBox(
      title = "Types de Variables",
      value = paste0(num_numeric, " numériques, ", num_categorical, " catégorielles"),
      icon = icon("list"),
      color = "green"
    )
  })
  
  
  
  # Résumé des données
  output$data_summary <- renderPrint({
    req(data())
    summary(data())
  })
  
  # Tableau des données
  output$demo_datatable <- DT::renderDataTable({
    req(data())
    data()
  }, options = list(pageLength = 10))
  



 # Préparation des données
prepare_data <- reactive({
  req(data())
  target <- colnames(data())[14]  
  predictors <- colnames(data())[-14]  
  
  dataset <- data()
  dataset[[target]] <- as.factor(dataset[[target]])  
  
  list(
    full_data = dataset,  
    target = target       
  )
})


split_data <- reactive({
  req(prepare_data())
  data_split <- prepare_data()
  dataset <- data_split$full_data
  target <- data_split$target
  
  set.seed(123)
  train_index <- caret::createDataPartition(dataset[[target]], p = 0.8, list = FALSE)
  
  list(
    train_data = dataset[train_index, ],
    test_data = dataset[-train_index, ]
  )
})

# Entraîner le modèle
model <- eventReactive(input$train_model, {
  req(split_data())
  data_split <- split_data()
  train_data <- data_split$train_data
  target <- prepare_data()$target
  
  if (input$model_choice == "Régression Logistique") {
    caret::train(as.formula(paste(target, "~ .")), data = train_data, method = "glm", family = "binomial")
  } else if (input$model_choice == "Arbre de Décision") {
    rpart(as.formula(paste(target, "~ .")), data = train_data, method = "class")
  } else if (input$model_choice == "Forêt Aléatoire") {
    randomForest(as.formula(paste(target, "~ .")), data = train_data, ntree = 100)
  } else if (input$model_choice == "SVM") {
    e1071::svm(as.formula(paste(target, "~ .")), data = train_data, probability = TRUE)
  }
})

# Évaluer les performances
output$model_metrics <- renderPrint({
  req(model())
  data_split <- split_data()
  test_data <- data_split$test_data
  target <- prepare_data()$target
  
  predictions <- predict(model(), newdata = test_data, type = if (input$model_choice == "Régression Logistique") "raw" else "class")
  cm <- caret::confusionMatrix(as.factor(predictions), test_data[[target]])
  
  list(
    Accuracy = cm$overall["Accuracy"],
    Precision = cm$byClass["Precision"],
    Recall = cm$byClass["Recall"],
    F1_Score = cm$byClass["F1"]
  )
})

output$roc_curve <- renderPlotly({
  req(model())
  data_split <- split_data()
  test_data <- data_split$test_data
  target <- prepare_data()$target
  
  # Obtenir les probabilités pour les modèles qui les supportent
  prob_predictions <- if (input$model_choice == "Régression Logistique" || input$model_choice == "SVM") {
    predict(model(), newdata = test_data, type = "prob")[, 2]  # Probabilité de la classe positive
  } else if (input$model_choice == "Arbre de Décision" || input$model_choice == "Forêt Aléatoire") {
    as.numeric(predict(model(), newdata = test_data, type = "prob")[, 2])  # Probabilité de la classe positive
  } else {
    stop("Modèle non pris en charge pour la courbe ROC")
  }
  
  # Vérifiez que les longueurs correspondent
  if (length(prob_predictions) != length(test_data[[target]])) {
    stop("La longueur des prédictions ne correspond pas à celle des valeurs réelles.")
  }
  
  # Générer la courbe ROC
  roc_obj <- pROC::roc(test_data[[target]], prob_predictions)
  
  plot_ly(
    x = 1 - roc_obj$specificities,
    y = roc_obj$sensitivities,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'blue')
  ) %>% layout(
    title = "Courbe ROC",
    xaxis = list(title = "1 - Specificité"),
    yaxis = list(title = "Sensibilité")
  )
})




}

shinyApp(ui, server)
