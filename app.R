

library(shiny)

ui <- fluidPage(



  fluidRow(
    column(3 ,fileInput("file1", "Choisissez un fichier")),

    column(9 ,verbatimTextOutput(outputId = "metadata") )

  ),
  hr(),

  fluidRow(

    column(2 , actionButton(inputId = "go" , label ="charger") ),
    column(6 , tableOutput(outputId = "mytable")),
    column(4 , tableOutput(outputId = "centredis"))

  ),



  fluidRow(

    column(4,  plotOutput(outputId = "effectif")  ) , 
    column(4, plotOutput(outputId = "effecumul")) ,
    column(4, plotOutput(outputId = "boiteAmoustache"))

  ),

  fluidRow(
    column(4, 
           # Zone d'affichage de l'histogramme
           plotOutput(outputId = "effectifsHist")),
    column(4, 
           # Zone d'affichage de l'histogramme
           plotOutput(outputId = "effectifsHistFreqDens"))
  ),
  
  fluidRow(
    column(4, 
           # Zone d'affichage de la courbe cumulative
           plotOutput(outputId = "effectifsCumCurve")),
    column(4, 
           # Zone d'affichage de la courbe cumulative
           plotOutput(outputId = "freqCumCurve"))
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

output$effectif <- renderPlot({ 
    plot(table(data()), col ="green4", xlab ="âge", ylab ="Effectifs", 
    main ="Distribution des effectifs pour l'âge")
  })


output$effecumul <- renderPlot({ 
    plot(ecdf(as.numeric(as.character(tabStats()[,1]))), 
         col ="green4", xlab ="âge", ylab ="Fréquences cumulées", 
         main ="Fréquences cumulés pour l'âge")
  })


output$boiteAmoustache <- renderPlot({
    # Boîte à moustaches
    boxplot( data(), col = grey(0.8), 
             main = "Age des salariés",
             ylab = "Age", las = 1)
    # Affichage complémentaires en Y des différents âges
    rug(data()[,1], side = 2)
  })



tabCentreDisp <- reactive({
    # Noms des caractéristiques
    names.tmp <- c("Maximum", "Minimum", "Moyenne", "Médiane",
                   "1e quartile", "3e quartile", "Variance", "Ecart-type")
    # Calcul des caractéristiques
    summary.tmp <- c(max(data()[,1]), min(data()[,1]), mean(data()[,1]), median(data()[,1]),
                     quantile((data()[,1]))[2], quantile((data()[,1]))[4],
                     var(data()[,1]), sqrt(var(data()[,1])))
    # Ajout des nomes au vecteur de valeurs
    summary.tmp <- cbind.data.frame(names.tmp, summary.tmp)
    # Ajout des noms de colonnes
    colnames(summary.tmp) <- c("Caractéristique", "Valeur")
    
    summary.tmp
  })



output$centredis <- renderTable({
  tabCentreDisp()
})



 # Récupération des valeurs fecondite
  fecondite <- reactive({
    if(!"fecondite" %in% colnames(data())) return(NULL)
    data()$fecondite
  })




 # Histogrammes
  # ----
  output$effectifsHist <- renderPlot({
    # Histogramme des effectifs
    hist( fecondite(), freq = TRUE, cex.axis = 1.5, cex.main = 1.5,
          main = "Histogramme de l'indice de fécondite", col = "blue",
          xlab = "Indice de fécondité", ylab = "Effectifs", las = 1,
          breaks = seq(0.8, 3, by = 0.2), right = FALSE, cex.lab = 1.5)
  })
  






}

# Run the application 
shinyApp(ui = ui, server = server)

