library(shiny)

fluidPage(
  
  titlePanel("Smamta"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "word", label = "Give word:", ),
      selectInput("file", "Select file:", c("output202006.csv" = "output202006.csv")),
      actionButton("button", "Run")
    ),
    
    mainPanel(
      textOutput("textbox"),
      textOutput("textbox2"),
      plotOutput("topic"),
      plotOutput("sentiment")
    )
    
  )
  
  
)