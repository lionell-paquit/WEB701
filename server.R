#
# Programmer: Lionell Carlo Paquit 
# Class: WEB701
#
# Simple Twitter Data Analysis in R Programming using Shiny in Web Integration
# 
#    https://nell-paquit.shinyapps.io/WEB701/
#


library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  #input update - hashtag
  
  observeEvent(input$update, once = T, handlerExpr = {
    #api validation at beginning of session
    api_key <- "API_KEY"
    api_secret <- "API_SECRET_KEY"
    access_token <- "ACCESS_TOKEN_KEY"
    access_token_secret <- "ACESS_TOKEN_SECRET"
    setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
    
    twitteR:::setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
  })
  
  #data gathering and plot generation
  observeEvent(input$update, handlerExpr = {
    #tweet gathering and error handling based on input type
    searchword <- paste(input$keyword, "exclude:retweets", sep=" ")
    tweets <- try(searchTwitter(searchword, lang = "en",
                                n = input$tws, retryOnRateLimit = 2000))
    tweets <- unique(tweets)
    
    #put tweets and metadata into dataframe
    tweets.df <- try(twListToDF(tweets))
    if(inherits(tweets.df ,'try-error')){
      return(NULL)
    }
    #copy of dataframe for use later
    tweets.df.copy <- tweets.df
    
    #remove ASCII characters from tweets
    cleanStrVec <- function(string_vec) {
      clean_vec <- c()
      for (k in c(1:length(string_vec))) {
        n <-iconv(string_vec[k], "latin1", "ASCII",sub='')
        clean_vec <- c(clean_vec, n)
      }
      return(clean_vec)
    }
    text <- tweets.df[1]
    cleaned_text<-cleanStrVec(text)

    # cleaning and pre-processing function
    cleanCorpus <- function(corpus) {
      # convert to lower case 
      myCorpusTmp <- tm_map(corpus, content_transformer(tolower))
      # remove punctuation
      myCorpusTmp <- tm_map(myCorpusTmp, removePunctuation) 
      # remove URLs
      removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
      myCorpusTmp <- tm_map(myCorpusTmp, content_transformer(removeURL))
      # add two extra stop words
      myStopwords <- c(stopwords("english"),stopwords("SMART"), "amp", "use", "see", "used", "will", "im")
      # remove stopwords from corpus
      myCorpusTmp <- tm_map(myCorpusTmp, removeWords, myStopwords)
      #extra stopwords based on input hashtag
      word <- tolower(input$keyword)
      stop <- gsub("#", "", word)
      moreStopwords <- c(word, "rt", "fb", "retweet", "a", "b" , "c", "d", "e", "f", "g", "h", "i", 
                         "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t","u","v", "w", "x", "y", "z")
      myCorpusTmp <- tm_map(myCorpusTmp, removeWords, moreStopwords)
      #create copy
      myCorpusCopy <- myCorpusTmp
      # stem words
      myCorpusTmp <- tm_map(myCorpusTmp, stemDocument)
      # remove numbers
      myCorpusTmp <- tm_map(myCorpusTmp, removeNumbers)
      
      #complete stems
      stemCompletion2 <- function(x, dictionary) {
        x <- unlist(strsplit(as.character(x), " "))
        x <- x[x != ""]
        x <- stemCompletion(x, dictionary=dictionary)
        x <- paste(x, sep="", collapse=" ")
        PlainTextDocument(stripWhitespace(x))
      }
      myCorpusTmp <- lapply(myCorpusTmp, stemCompletion2, dictionary=myCorpusCopy)
      return(myCorpusTmp)
    }
    #create the Corpus object and clean the tweet using cleanCorpus function
    myCorpus <- cleanCorpus(Corpus(VectorSource(cleaned_text)))
    myCorpus <- Corpus(VectorSource(myCorpus))
    
    #word frequency dataframe function  
    getConditionedDataFrame <- function(myCorpus) {
      #create the term matrix
      tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
      # calculate the frequency of words and sort it by frequency
      word.freq <- sort(rowSums(as.matrix(tdm)), decreasing = T)
      word.freq <- subset(word.freq, word.freq >=1)
      df <- data.frame(term = names(word.freq), freq = word.freq)
      return(df)
    }
    
    #word frequency dataframe
    tweets.df <- getConditionedDataFrame(myCorpus)
    
    #TDM/DTM for topic modeling 
    tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
    dtm <- as.DocumentTermMatrix(tdm)
    #remove empty values from DTM and original dataframe copy
    rowTotals <- apply(dtm , 1, sum) 
    dtm <- dtm[rowTotals> 0, ]
    tweets.df.copy <- tweets.df.copy[rowTotals> 0,]
  
    #randomized color values for plots
    hues <- c(60:330)
    pals <- c(3:8)
    
    observeEvent(input$update, once = T, handlerExpr = {
      #word frequency barplot
      output$freqPlot <- renderPlot({
        ggplot(tweets.df[1:input$numWords,], aes(x=reorder(term, freq), y=freq, fill = as.factor(term))) +
          geom_bar(stat = "identity", position = "dodge", col= "black") + xlab("Terms") + ylab("Count") +
          coord_flip() + scale_fill_hue(c = sample(hues, 1)) + guides(fill=FALSE)
      })

      #wordcloud
      output$wordPlot <- renderWordcloud2({
        validate(
          need(input$max <= nrow(tweets.df), "Selected size greater than number of elements in data. Reduce the size of wordcloud.")
        )
        new_df <- tweets.df[1:input$max,]
        wordcloud2(new_df, color="random-light", backgroundColor = "grey", size = .6, shuffle=T, rotateRatio = sample(c(1:100) / 100))
      })
      
      #wordcloud generation
      observeEvent(input$newCloud, handlerExpr = {
        output$wordPlot <- renderWordcloud2({
          validate(
            need(input$max <= nrow(tweets.df), "Selected size greater than number of elements in data. Reduce the size of wordcloud.")
          )
          new_df <- tweets.df[1:input$max,]
          wordcloud2(new_df, color = "random-light", backgroundColor = "grey", shuffle=T, size = .6, rotateRatio = sample(c(1:100) / 100))
        })
      })
    })
  })
})
