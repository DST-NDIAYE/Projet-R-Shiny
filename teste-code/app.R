library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(shinyjs)


ui <- dashboardPage(skin = "purple",
                  
                  
  dashboardHeader(title = "Basic Template"),
                            
 dashboardSidebar(
            sidebarMenu(id = "tabs",
            menuItem("Tab Title",
            tabName = "demo_tab",
            icon = icon("dragon")
            ))
    
 ),
                    
                    
                    
                    
 dashboardBody(shinyjs::useShinyjs(),
      tags$head(
            # links to files in www/
            tags$link(rel = "stylesheet",
            type = "text/css",
            href = "custom.css"),
            tags$script(src = "custom.js")
      ),
      
      
      tabItems( 
                
        tabItem( tabName = "demo_tab" ,
                 
                 # in the ui
                 box(title = "Questions",
                     solidHeader = TRUE,
                     selectInput("first_pet", "What was your first pet?",
                                 c("", "dog", "cat", "ferret", "other")),
                     hidden(textInput("first_pet_other", NULL,
                                      placeholder = "Specify the other pet"))
                 )
          
          
        )
      )
  
 )
)
      
      
      

server <- function(input, output, session) {
  # in the server
  observeEvent(input$first_pet, {
    if (input$first_pet == "other") {
      show("first_pet_other")
    } else {
      hide("first_pet_other")
    }
  })
}

shinyApp(ui = ui, server = server)