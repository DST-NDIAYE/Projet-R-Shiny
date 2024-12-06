library(shiny)
library(shinydashboard)

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
      menuItem("Accueil", tabName = "home", icon = icon("home")),
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
      # Page d'accueil
      tabItem(
        tabName = "home",
        h2("Bienvenue sur l'application Shiny !"),
        p("Utilisez le menu pour naviguer entre les différentes sections.")
      ),
      
      # Section Prétraitement
      tabItem(
        tabName = "columns",
        h2("Choix des colonnes à utiliser")
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

server <- function(input, output) {}

shinyApp(ui, server)