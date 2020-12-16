rm(list = ls())

library(tm)
library(wordcloud)
library(sjmisc)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(SentimentAnalysis)

# filecsv <- read.csv("output202006.csv", stringsAsFactors = FALSE, sep="")

filecsv <- readLines("output202006.csv")

parsedfile <- c()

for(i in filecsv){
  if(str_contains(i, "horse") == TRUE) {
    parsedfile <- c(parsedfile, i)
  }
}

filecsv.vec <- VectorSource(parsedfile)


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

text_dtm <- DocumentTermMatrix(textcorpus)

text_lda <- LDA(text_dtm, k = 2, control = list(seed = 1234))

text_topics <- tidy(text_lda, matrix = "beta")

text_top_terms <- text_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

text_top_terms %>%
  mutate(term=reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~topic, scales = "free") +
  scale_x_reordered() +
  ggtitle("Top term-topic-probabilities")


sentimentscore <- table(convertToDirection(analyzeSentiment(text_dtm)$SentimentQDAP))

plot(sentimentscore)