library(shiny)

fluidPage(
  
  titlePanel("Smata"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "url", label = "Give URL")
    ),
    
    mainPanel(
      plotOutput("wordcloud")
    )
    
  )
  
  
)