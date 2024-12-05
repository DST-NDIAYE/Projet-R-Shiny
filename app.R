

library(shiny)

ui <- fluidPage(



  fluidRow(
    column(3 ,fileInput("file1", "Choisissez un fichier")),

    column(9 ,verbatimTextOutput(outputId = "metadata") )

  ),
  hr(),

  fluidRow(

    column(2 , actionButton(inputId = "go" , label ="charger") ),
    column(10 , tableOutput(outputId = "mytable"))

  )

  

      
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  data <- eventReactive( input$go ,  {
    file = input$file1  
    if (is.null(file)) return("Aucun fichier téléchargé")
    read.csv( file$datapath , header = TRUE )
  })

  tabStats <- reactive({
    # Calculer les effectifs et les effectifs cumulés
    table.tmp <- as.data.frame(table(data()))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    # Calculer les fréquences et les fréquences cumulés
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(data())*100,
                       table.tmp[[3]]/nrow(data())*100)
    # Ajouter des noms de colonnes
    colnames(table.tmp) <- c("Ages", "Effectifs", "Effectifs Cum.",
                             "Fréquences", "Fréquences Cum.")
    
    table.tmp <- table.tmp[,c(1, 4, 2, 5, 3)]
    # Renvoyer le tableau statistique
    table.tmp
  })



  output$metadata <- renderPrint({
    summary(data())
  })


 #Must be set within the server
output$mytable <- renderTable({
  tabStats()

})




}

# Run the application 
shinyApp(ui = ui, server = server)

