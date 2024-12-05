

library(shiny)

ui <- fluidPage(



  fluidRow(
    column(3 ,fileInput("file1", "Choisissez un fichier")),

    column(9 ,verbatimTextOutput(outputId = "metadata") )

  ),

  fluidRow(

    column(2 , actionButton(inputId = "go" , label ="charger") ),
    column(8 , tableOutput(outputId = "mytable"))

  )

  

      
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   



  data <- eventReactive( input$go ,  {

    file = input$file1  
     if (is.null(file)) return("Aucun fichier téléchargé")

  read.csv( file$datapath , header = TRUE )
  }
  )

  output$metadata <- renderPrint({
    summary(data())
  })



 #Must be set within the server
output$mytable <- renderTable({
  data()

})




}

# Run the application 
shinyApp(ui = ui, server = server)

