library(shiny)
library(tm)
library(sjmisc)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(SentimentAnalysis)
library(stm)

function(input, output) {
  
  fileparsed <- function(wordInput, fileInput) {
    filecsv <- readLines(fileInput)
    
    searchword <- c(wordInput)
    
    parsedfile <- c()
    
    for(i in filecsv){
      if(str_contains(i, searchword, logic = "or") == TRUE) {
        parsedfile <- c(parsedfile, i)
      }
    }
    
    return(parsedfile)
  }
  
  dtm <- function(parsedfile){
    
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
    
    text_dtm <- DocumentTermMatrix(textcorpus)
    
    return(text_dtm)
  }
  
  word <- eventReactive(input$button, {
    if (nchar(input$word) < 3) {
      return()
    } else {
      
      doneparsed <- fileparsed(input$word, input$file)
      returnlist <- list("dtm" = dtm(doneparsed), "parsed" = doneparsed)
      return(returnlist)
    }
  })
  
  
  output$topic <- renderPlot({
    
    if(is.null(word())) {
      return()
    } else {
      
      mylist <- word()
      
      temp <- textProcessor(mylist$parsed, stem = TRUE, verbose = FALSE)
      out <- prepDocuments(temp$documents, temp$vocab, verbose = FALSE)
      
      set.seed(5707363)
      bestK <- searchK(out$documents, out$vocab, K = c(4:12), seed = 5707363, verbose = FALSE)
      
      bestKsearch <- data.frame(bestK$results$K, bestK$results$semcoh)
      
      colnames(bestKsearch) <- c("K", "semcohs")
      
      bestpickedK <- bestKsearch[which.max(bestKsearch$semcohs),]
      
      pick <- as.integer(bestpickedK$K)
      
      text_lda <- LDA(mylist$dtm, k = pick, control = list(seed = 1234))
      
      text_topics <- tidy(text_lda, matrix = "beta")
      
      text_top_terms <- text_topics %>%
        group_by(topic) %>%
        top_n(10, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
      
      topicframe <- data.frame(text_top_terms)
      
      write.csv2(topicframe, "outputtopic.csv", row.names = FALSE)
      
      text_top_terms %>%
        mutate(term=reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        facet_wrap(~topic, scales = "free") +
        scale_x_reordered() +
        ggtitle("Top term-topic-probabilities")
      
    }
    
  })

  output$sentiment <- renderPlot({
    
    if(is.null(word())) {
      return()
    } else {
      
      mylist <- word()
      
      sentimentscore <- table(convertToDirection(analyzeSentiment(mylist$dtm)$SentimentQDAP))
      
      positive <- c(sentimentscore[3])
      neutral <- c(sentimentscore[2])
      negative <- c(sentimentscore[1])
      
      sentimentframe <- data.frame(positive, neutral, negative)
      
      write.csv2(sentimentframe, "outputsentiment.csv", row.names = FALSE)
      
      
      plot(sentimentscore)
      legend("topleft", legend = c("Negative", sentimentscore[1], "Neutral", sentimentscore[2], "Positive",  sentimentscore[3]))
      
    }
  })
  
  output$topicbox <- renderText({
    if(is.null(word())) {
      "Wrong input"
    }

  })
  
  output$sentimentbox <- renderText({
    if(is.null(word())) {
      "Wrong input"
    }


  })

}