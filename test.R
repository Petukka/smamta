rm(list = ls())

#text scraping from Suomi24

library(rvest)

text <- read_html("https://keskustelu.suomi24.fi/t/16298423/kysy-mita-vain-kumppanuusvanhemmuudesta-amp-apilaperheista-asiantuntijat-vastaavat-to-26-3--klo-14-16")

tweets <- text %>%
  html_nodes("article p") %>%
  html_text()

#cleaning the text

library(tm)
tweets.vec <- VectorSource(tweets)

textcorpus <- VCorpus(tweets.vec)

library(SnowballC)

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("finnish"))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  return(corpus)
}

textcorpus <- clean_corpus(textcorpus)

#creating wordcloud

library(wordcloud)

text_tdm <- TermDocumentMatrix(textcorpus)

matrix_text_tdm <- as.matrix(text_tdm)

matrix_text_tdm <- sort(rowSums(matrix_text_tdm), decreasing = TRUE)

matrix_text_tdm <- data.frame(word = names(matrix_text_tdm), freq = matrix_text_tdm)

head(matrix_text_tdm)

wordcloud(words = matrix_text_tdm$word, freq = matrix_text_tdm$freq, max.words = 150, random.order = FALSE)

#create a term-document matrix

text_tdm <- TermDocumentMatrix(textcorpus)

matrix_text_tdm <- as.matrix(text_tdm)

head(matrix_text_tdm)

#commonality cloud

library(wesanderson)

commonality.cloud(matrix_text_tdm, random.order = FALSE, max.words = 100)

comparison.cloud(matrix_text_tdm, max.words = 100)



