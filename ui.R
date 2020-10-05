library(shiny)

fluidPage(
  
  titlePanel("Smamta"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "url", label = "Give URL")
    ),
    
    mainPanel(
      plotOutput("wordcloud")
    )
    
  )
  
  
)