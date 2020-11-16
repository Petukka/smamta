library(shiny)
library(tm)
library(wordcloud)

function(input, output) {
  
  observeEvent(input$button, {
    output$wordcloud <- renderPlot({

      filename <- input$file

      filecsv <- read.csv(filename, stringsAsFactors = FALSE)

      filecsv.vec <- VectorSource(filecsv)

      textcorpus <- VCorpus(filecsv.vec)

      clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeWords, stopwords("en"))
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, content_transformer(tolower))
        return(corpus)
      }

      textcorpus <- clean_corpus(textcorpus)

      wordcloud(textcorpus, max.words = 150)
    })
  })
  

}