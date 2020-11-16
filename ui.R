library(shiny)

fluidPage(
  
  titlePanel("Smamta"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "file", label = "Give file name"),
      actionButton("button","Run")
    ),
    
    mainPanel(
      plotOutput("wordcloud")
    )
    
  )
  
  
)