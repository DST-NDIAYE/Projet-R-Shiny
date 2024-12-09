library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

ui <- dashboardPage(
  # Barre d'en-tête avec un menu déroulant
  dashboardHeader(
    title = "Application Shiny",
    dropdownMenu(
      type = "notifications",  # Menu déroulant pour les notifications
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
      type = "tasks",  # Menu déroulant pour les tâches
      taskItem(
        text = "Préparation des données",
        value = 60,  # Barre de progression
        color = "blue"
      ),
      taskItem(
        text = "Modélisation",
        value = 30,
        color = "green"
      )
    )
  ),
  
  # Barre latérale pour naviguer entre les sections
  dashboardSidebar(

    sidebarMenu(
      
      menuItem("Dashboard", tabName = "home", icon = icon("home")),
      menuItem("Prétraitement", tabName = "preprocessing", icon = icon("sliders"),
        menuSubItem("Choix des colonnes", tabName = "columns"),
        menuSubItem("Normalisation", tabName = "normalization"),
        menuSubItem("Gestion des classes", tabName = "class_balance")
      ),
      menuItem("Analyse exploratoire", tabName = "exploration", icon = icon("chart-bar")),
      menuItem("Modélisation", tabName = "modeling", icon = icon("robot"))
    )
  ),
  
  # Corps principal où le contenu s'affichera
  dashboardBody(
    tabItems(
      # Page d'Dashboard
      tabItem(
  tabName = "home",
  
  fluidRow(
    column(4, 
           # Bouton de recherche du fichier à charger
           fileInput(inputId = "file1", label = "Choose CSV File",
                     accept = c("text/plain", ".csv"))
    ),
    column(4, 
           actionButton(inputId = "go", label = "Charger les données")
    )
  ),
  
  hr(),
  
  # Row for InfoBoxes
  fluidRow(
    infoBoxOutput("rows_columns_info"),
    infoBoxOutput("missing_values_info")
  ),
  
  fluidRow(
    infoBoxOutput("numeric_categorical_info")
  ),
  
  hr(),
  
  DT::dataTableOutput("demo_datatable",
                      width = "50%",
                      height = "auto")
)
,
      
      # Section Prétraitement
      tabItem(
        tabName = "columns",
        h2("Choix des colonnes à utiliser") ,
      ),
      tabItem(
        tabName = "normalization",
        h2("Normalisation des données")
      ),
      tabItem(
        tabName = "class_balance",
        h2("Gestion des classes déséquilibrées")
      ),
      
      # Section Analyse exploratoire
      tabItem(
        tabName = "exploration",
        h2("Analyse exploratoire")
      ),
      
      # Section Modélisation
      tabItem(
        tabName = "modeling",
        h2("Modélisation des données")
      )
    )
  )
)

server <- function(input, output) {
  
  # Charger les données de manière réactive
  data <- eventReactive(input$go, {
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, header = FALSE)
  })
  
  # InfoBox : Nombre de lignes et colonnes
  output$rows_columns_info <- renderInfoBox({
    req(data())  # Vérifie que les données sont chargées
    
    infoBox(
      title = "Lignes et Colonnes",
      value = paste0(nrow(data()), " lignes, ", ncol(data()), " colonnes"),
      icon = icon("table"),
      color = "blue"
    )
  })
  
  # InfoBox : Nombre de valeurs manquantes
  output$missing_values_info <- renderInfoBox({
    req(data())
    
    infoBox(
      title = "Valeurs Manquantes",
      value = sum(is.na(data())),
      icon = icon("exclamation-circle"),
      color = "red"
    )
  })
  
  # InfoBox : Variables numériques et catégorielles
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
  
  # Affichage des données dans un tableau
  output$demo_datatable <- DT::renderDataTable({
    data()
  }, options = list(pageLength = 10))
}


shinyApp(ui, server)