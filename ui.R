library(shiny)

fluidPage(
  
  titlePanel("Smamta"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "word", label = "Give word or words:", ),
      selectInput("file", "Select file:", c("output202006.csv" = "data/output202006.csv")),
      actionButton("button", "Run")
    ),
    
    mainPanel(
      textOutput("topicbox"),
      plotOutput("topic"),
      textOutput("sentimentbox"),
      plotOutput("sentiment")
    )
    
  )
  
  
)