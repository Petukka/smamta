library(shiny)
library(rvest)
library(tm)
library(SnowballC)
library(wordcloud)

function(input, output) {
  output$wordcloud <- renderPlot({
    url <- input$url
    text <- read_html(url)
    
    tweets <- text %>%
      html_nodes("article p") %>%
      html_text()
    
    tweets.vec <- VectorSource(tweets)
    
    textcorpus <- VCorpus(tweets.vec)
    
    clean_corpus <- function(corpus){
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeWords, stopwords("finnish"))
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, content_transformer(tolower))
      return(corpus)
    }
    
    textcorpus <- clean_corpus(textcorpus)
    
    text_tdm <- TermDocumentMatrix(textcorpus)
    
    matrix_text_tdm <- as.matrix(text_tdm)
    
    matrix_text_tdm <- sort(rowSums(matrix_text_tdm), decreasing = TRUE)
    
    matrix_text_tdm <- data.frame(word = names(matrix_text_tdm), freq = matrix_text_tdm)
    
    head(matrix_text_tdm)
    
    wordcloud(words = matrix_text_tdm$word, freq = matrix_text_tdm$freq, max.words = 150, random.order = FALSE)
    
  })

}