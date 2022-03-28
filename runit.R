rm(list = ls())
.rs.restartR()

library(tm)
library(wordcloud)
library(sjmisc)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(SentimentAnalysis)
library(stm)

filecsv <- readLines("data/output202001.csv")

filecsv <- append(filecsv, readLines("data/output202002.csv"))

filecsv <- append(filecsv, readLines("data/output202003.csv"))

parsedfile <- c()

searchword <- c("smart city", 
                "smart cities", 
                "smartcity", 
                "smartcities", 
                "Smart city", 
                "Smart cities", 
                "Smartcity", 
                "Smartcities", 
                "Smart City", 
                "Smart Cities",
                "SmartCity",
                "SmartCities",
                "smart City",
                "smart Cities",
                "smartCity",
                "smartCities")

for(i in filecsv){
  if(str_contains(i, searchword, logic = "or") == TRUE) {
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

temp <- textProcessor(parsedfile, stem = TRUE, verbose = FALSE)
out <- prepDocuments(temp$documents, temp$vocab, verbose = FALSE)

set.seed(5707363)
bestK <- searchK(out$documents, out$vocab, K = c(4:12), seed = 5707363, verbose = FALSE)

bestKsearch <- data.frame(bestK$results$K, bestK$results$semcoh)

colnames(bestKsearch) <- c("K", "semcohs")

bestpickedK <- bestKsearch[which.max(bestKsearch$semcohs),]

pick <- as.integer(bestpickedK$K)

text_dtm <- DocumentTermMatrix(textcorpus)

#
#
#
#       Topic model
#
#
#

text_lda <- LDA(text_dtm, k = pick, control = list(seed = 1234))

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

topicframe <- data.frame(text_top_terms)

write.csv2(topicframe, "outputtopic.csv", row.names = FALSE)


#
#
#
#       Sentiment analysis
#
#
#

sentimentscore <- table(convertToDirection(analyzeSentiment(text_dtm)$SentimentQDAP))

plot(sentimentscore)

legend("topleft", legend = c("Negative", sentimentscore[1], "Neutral", sentimentscore[2], "Positive",  sentimentscore[3]))

positive <- c(sentimentscore[3])
neutral <- c(sentimentscore[2])
negative <- c(sentimentscore[1])

sentimentframe <- data.frame(positive, neutral, negative)

write.csv2(sentimentframe, "outputsentiment.csv", row.names = FALSE)