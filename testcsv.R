rm(list = ls())

library(tm)
library(wordcloud)
library(sjmisc)

# filecsv <- read.csv("output202006.csv", stringsAsFactors = FALSE, sep="")

filecsv <- readLines("output202006.csv")

parsedfile <- c()

for(i in filecsv){
  if(str_contains(i, "horse") == TRUE) {
    parsedfile <- c(parsedfile, i)
  }
}

filecsv.vec <- VectorSource(filecsv)


textcorpus <- VCorpus(filecsv.vec)

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(stemDocument))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  return(corpus)
}

textcorpus <- clean_corpus(textcorpus)

wordcloud(textcorpus, max.words = 150)