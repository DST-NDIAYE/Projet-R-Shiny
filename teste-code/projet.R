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
library(rpart.plot)




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
    menuItem("Analyse exploratoire", icon = icon("chart-bar"),
      menuSubItem("Unidimensionnel", tabName = "univariate"),
      menuSubItem("Bidimensionnel", tabName = "bivariate")
    ),
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
     

tabItem(
  tabName = "univariate",
  h2("Analyse unidimensionnelle"),
  
  fluidRow(
    column(4,
           selectInput("variable_uni", "Variable à analyser :", choices = NULL),
           actionButton("update_uni", "Mettre à jour")
    )
  ),
  
  fluidRow(
    box(
      title = "Statistiques descriptives",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      tableOutput("summary_uni")
    )
  ),
  
  fluidRow(
    box(
      title = "Distribution de la variable",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      plotOutput("dist_uni_plot")
    )
  )
)
,

tabItem(
  tabName = "bivariate",
  h2("Analyse bidimensionnelle"),
  
  fluidRow(
    column(4,
           selectInput("variable_x", "Variable X (quantitative) :", choices = NULL),
           selectInput("variable_y", "Variable Y :", choices = NULL),
           actionButton("update_bi", "Mettre à jour")
    )
  ),
  
  fluidRow(
    box(
      title = "Nuage de points (Quantitative vs Quantitative)",
      status = "success",
      solidHeader = TRUE,
      width = 6,
      plotOutput("scatter_plot")
    ),
    box(
      title = "Boxplot (Quantitative vs Qualitative)",
      status = "warning",
      solidHeader = TRUE,
      width = 6,
      plotOutput("box_plot")
    )
  ),
  
  fluidRow(
    box(
      title = "Corrélation et métriques",
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      verbatimTextOutput("correlation_metrics")
    )
  )
)



     
     ,

      tabItem(
  tabName = "modeling",
  h2("Modélisation supervisée"),












  
  ################################### Choix du modèle
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
  infoBoxOutput("accuracy_info", width = 3),
  infoBoxOutput("precision_info", width = 3),
  infoBoxOutput("recall_info", width = 3),
  infoBoxOutput("f1_score_info", width = 3)
),


fluidRow(
  box(
    title = "Visualisation du modèle",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    plotOutput("tree_plot") %>% withSpinner(color = "blue")  # Spinner pour le chargement
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
  ),


  fluidRow(
  box(
    title = "Importance des variables (Régression Logistique)",
    status = "info",
    solidHeader = TRUE,
    width = 12,
    plotOutput("variable_importance_plot")
  )
)
)
    )
  )
)




                        ##################### PARTIE SERVER #########################



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
# Accuracy InfoBox
output$accuracy_info <- renderInfoBox({
  req(model())
  data_split <- split_data()
  test_data <- data_split$test_data
  target <- prepare_data()$target
  
  predictions <- predict(model(), newdata = test_data, type = if (input$model_choice == "Régression Logistique") "raw" else "class")
  cm <- caret::confusionMatrix(as.factor(predictions), test_data[[target]])
  
  infoBox(
    title = "Accuracy",
    value = round(cm$overall["Accuracy"] * 100, 2),  # Afficher en pourcentage
    icon = icon("check-circle"),
    color = "green"
  )
})

# Precision InfoBox
output$precision_info <- renderInfoBox({
  req(model())
  data_split <- split_data()
  test_data <- data_split$test_data
  target <- prepare_data()$target
  
  predictions <- predict(model(), newdata = test_data, type = if (input$model_choice == "Régression Logistique") "raw" else "class")
  cm <- caret::confusionMatrix(as.factor(predictions), test_data[[target]])
  
  infoBox(
    title = "Precision",
    value = round(cm$byClass["Precision"] * 100, 2),  # Afficher en pourcentage
    icon = icon("balance-scale"),
    color = "blue"
  )
})

# Recall InfoBox
output$recall_info <- renderInfoBox({
  req(model())
  data_split <- split_data()
  test_data <- data_split$test_data
  target <- prepare_data()$target
  
  predictions <- predict(model(), newdata = test_data, type = if (input$model_choice == "Régression Logistique") "raw" else "class")
  cm <- caret::confusionMatrix(as.factor(predictions), test_data[[target]])
  
  infoBox(
    title = "Recall",
    value = round(cm$byClass["Recall"] * 100, 2),  # Afficher en pourcentage
    icon = icon("sync-alt"),
    color = "yellow"
  )
})

# F1-Score InfoBox
output$f1_score_info <- renderInfoBox({
  req(model())
  data_split <- split_data()
  test_data <- data_split$test_data
  target <- prepare_data()$target
  
  predictions <- predict(model(), newdata = test_data, type = if (input$model_choice == "Régression Logistique") "raw" else "class")
  cm <- caret::confusionMatrix(as.factor(predictions), test_data[[target]])
  
  infoBox(
    title = "F1-Score",
    value = round(cm$byClass["F1"] * 100, 2),  # Afficher en pourcentage
    icon = icon("chart-line"),
    color = "red"
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
  
  # Générer la courbe ROC et calculer l'AUC
  roc_obj <- pROC::roc(test_data[[target]], prob_predictions)
  auc_value <- round(pROC::auc(roc_obj), 3)  # Calcul de l'AUC
  
  # Transformer les données ROC en data.frame pour ggplot2
  roc_data <- data.frame(
    Specificity = 1 - roc_obj$specificities,
    Sensitivity = roc_obj$sensitivities
  )
  
  # Tracer la courbe ROC avec ggplot2
  ggplot(roc_data, aes(x = Specificity, y = Sensitivity)) +
    geom_line(color = "blue", size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Ligne de base (random)
    labs(
      title = paste("Courbe ROC (AUC =", auc_value, ")"),
      x = "1 - Specificité",
      y = "Sensibilité"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12)
    )
})


output$tree_plot <- renderPlot({
  req(model())
  
  # Vérifier quel modèle est utilisé
  if (input$model_choice == "Arbre de Décision") {
    # Visualiser l'arbre de décision
    
    rpart.plot::rpart.plot(model(), main = "Arbre de Décision")
    
  } else if (input$model_choice == "Forêt Aléatoire") {
    # Visualiser un arbre individuel dans la forêt
    
    selected_tree <- randomForest::getTree(model(), k = 1, labelVar = TRUE)  # Sélectionner le premier arbre
    plot(selected_tree, main = "Arbre individuel dans la Forêt Aléatoire")
  } else {
    # Si ce n'est pas un modèle compatible
    plot(0, 0, type = "n", ann = FALSE)
    text(0, 0, "Visualisation non disponible pour ce modèle", cex = 1.5)
  }
})


output$variable_importance_plot <- renderPlot({
  req(model())
  
  if (input$model_choice == "Régression Logistique") {
    # Vérifier si le modèle est un objet caret::train ou glm
    if (!is.null(model()$finalModel)) {
      # Modèle caret::train
      coefficients <- coef(model()$finalModel)
    } else {
      # Modèle glm standard
      coefficients <- coef(model())
    }
    
    # Vérifier que les coefficients sont numériques
    if (!is.numeric(coefficients)) {
      stop("Les coefficients extraits ne sont pas numériques. Vérifiez le modèle.")
    }
    
    # Créer un data frame pour l'importance
    variable_importance <- data.frame(
      Variable = names(coefficients),
      Importance = abs(as.numeric(coefficients))  # Valeur absolue des coefficients
    )
    
    # Tracer le barplot
    ggplot(variable_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Importance des variables (Régression Logistique)",
        x = "Variable",
        y = "Importance (Valeur absolue du coefficient)"
      ) +
      theme_minimal()
  } else if (input$model_choice == "Arbre de Décision") {
    # Vérifier que le modèle est bien un objet rpart
    if (!inherits(model(), "rpart")) {
      stop("Le modèle sélectionné n'est pas un arbre de décision.")
    }
    
    # Extraire l'importance des variables
    importance_vals <- model()$variable.importance
    
    # Vérifier que l'importance des variables est disponible
    if (is.null(importance_vals)) {
      stop("Aucune importance de variable disponible pour cet arbre de décision.")
    }
    
    # Créer un data frame pour l'affichage
    variable_importance <- data.frame(
      Variable = names(importance_vals),
      Importance = importance_vals
    )
    
    # Tracer le barplot
    ggplot(variable_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "orange") +
      coord_flip() +
      labs(
        title = "Importance des variables (Arbre de Décision)",
        x = "Variable",
        y = "Importance"
      ) +
      theme_minimal()
  } else if (input$model_choice == "Forêt Aléatoire") {
    # Gestion déjà correcte pour la Forêt Aléatoire
    importance_vals <- randomForest::importance(model())
    variable_importance <- data.frame(
      Variable = rownames(importance_vals),
      Importance = importance_vals[, 1]  # MDA ou MDG
    )
    
    ggplot(variable_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      coord_flip() +
      labs(
        title = "Importance des variables (Forêt Aléatoire)",
        x = "Variable",
        y = "Importance"
      ) +
      theme_minimal()
  } 
})










############### analyse ##################


observe({
  req(data())
  numeric_vars <- names(data())[sapply(data(), is.numeric)]
  categorical_vars <- names(data())[sapply(data(), function(x) is.factor(x) || is.character(x))]
  
  updateSelectInput(session, "variable_uni", choices = names(data()))
  updateSelectInput(session, "variable_x", choices = numeric_vars)
  updateSelectInput(session, "variable_y", choices = c(numeric_vars, categorical_vars))
})



output$summary_uni <- renderTable({
  req(input$variable_uni)
  
  variable <- data()[[input$variable_uni]]
  
  if (is.numeric(variable)) {
    summary_stats <- summary(variable)
    data.frame(Statistique = names(summary_stats), Valeur = as.character(summary_stats))
  } else {
    summary_stats <- table(variable)
    data.frame(Catégorie = names(summary_stats), Fréquence = as.numeric(summary_stats))
  }
})



output$dist_uni_plot <- renderPlot({
  req(input$variable_uni)
  if (is.numeric(data()[[input$variable_uni]])) {
    ggplot(data(), aes_string(x = input$variable_uni)) +
      geom_histogram(fill = "blue", color = "white", bins = 30) +
      labs(title = paste("Distribution de", input$variable_uni), x = input$variable_uni, y = "Fréquence") +
      theme_minimal()
  } else {
    ggplot(data(), aes_string(x = input$variable_uni)) +
      geom_bar(fill = "orange") +
      labs(title = paste("Distribution de", input$variable_uni), x = input$variable_uni, y = "Fréquence") +
      theme_minimal()
  }
})


output$scatter_plot <- renderPlot({
  req(input$variable_x, input$variable_y)
  ggplot(data(), aes_string(x = input$variable_x, y = input$variable_y)) +
    geom_point(color = "darkgreen") +
    labs(title = paste("Nuage de points entre", input$variable_x, "et", input$variable_y),
         x = input$variable_x, y = input$variable_y) +
    theme_minimal()
})


output$box_plot <- renderPlot({
  req(input$variable_x, input$variable_y)
  
  # Charger les données sans affecter les autres parties
  df <- data()
  
  # Convertir temporairement la variable Y en facteur si elle est catégorique
  if (input$variable_y %in% c("sex", "chest-pain", "fasting-blood-sugar", 
                              "electrocardiographic", "angina", "slope", 
                              "major-vessels", "thal", "heart-disease")) {
    df[[input$variable_y]] <- as.factor(df[[input$variable_y]])
  }
  
  # Vérifier si la variable Y est maintenant catégorique
  if (is.factor(df[[input$variable_y]])) {
    ggplot(df, aes_string(x = input$variable_y, y = input$variable_x)) +
      geom_boxplot(fill = "purple", color = "black") +
      labs(
        title = paste("Boxplot de", input$variable_x, "par", input$variable_y),
        x = input$variable_y,
        y = input$variable_x
      ) +
      theme_minimal()
  } else {
    ggplot() +
      annotate("text", x = 1, y = 1, label = "La variable Y doit être catégorique.", size = 5) +
      theme_void()
  }
})




output$correlation_metrics <- renderPrint({
  req(input$variable_x, input$variable_y)
  if (is.numeric(data()[[input$variable_x]]) && is.numeric(data()[[input$variable_y]])) {
    correlation <- cor(data()[[input$variable_x]], data()[[input$variable_y]], use = "complete.obs")
    paste("Coefficient de corrélation linéaire (Pearson):", round(correlation, 2))
  } else {
    "Les métriques de corrélation ne sont pas disponibles pour ces variables."
  }
})





}

shinyApp(ui, server)
