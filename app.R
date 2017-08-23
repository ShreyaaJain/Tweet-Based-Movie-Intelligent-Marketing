library("shiny")
ui<-fluidPage(
  tags$style(".span12 {background-color: black;}"),
  headerPanel("Application"),
  
  sidebarPanel(
    # sliderInput(inputId="num", label="choose a number", value=3, min=1, max = 100),
    
    
    fluidRow(column(3, selectInput("select", label = h3("Select location"), 
                                   choices = c("Boston"="40.698470,-73.951442,50mi", "NewYork"="40.698470,-73.951442,50mi", "Los Angeles"="34.0522300,-118.2436800,50mi"), 
                                   selected = 1)	)	)  
    ,
    fluidRow(column(3, selectInput("select1",label = h3("Select movie"),
                                   choices = c("Captain America"="Captain America","X-Men"="X-Men"),selected=1))
             
    )
  ),
  
  
  mainPanel 
  (
  tabsetPanel(
    tabPanel("Plot of emotions",plotOutput("emotionplot"), width=10),
    tabPanel("Plot of Polarity",plotOutput("polarityplot"), width=10),
    tabPanel("Word Cloud",plotOutput("emotionswordcloud"),width=10)
  ),
  
  tabsetPanel(
    
    
    tabPanel("Histogram of Sentiment Scores", plotOutput("histogramscores"),width=10),
    tabPanel("Boxplot of Sentiment Scores", plotOutput("boxplotscores"),width=10)
  )  
  )
)

server<-function(input, output)
{
  output$value<-renderPrint({input$select})
  
  
  library("twitteR")
  library("Rstem")
  library("sentiment")
  library("httr")
  library("tm")
  library("NLP")
  library("wordcloud")
  library("RColorBrewer")
  library("RCurl")
  library("bitops")
  library("ROAuth")
  library("plyr")
  library("sentiment")
  library("ggplot2")
  library("stringr")
  library("plyr")
  library("lattice")
  
  key <- "brFgnrk6dFewBOWBiD2m0tANA" 
  secret <- "kvByOQydL4AIFnMnmAVVYMF4klgSNJwtkNbA5qmTCAisqv6QAT" 
  secrettk <- "7XfA0v9j0utKeUuf44n2YEB3AtzqVlMM0ue4IrJC0v2cK"
  mytoken <- "708481334482698240-QTn0EaokD6IVWFH0ZUhzlW48rdl42Qt"
  
  setup_twitter_oauth(key,secret,mytoken,secrettk)
  
  output$emotionplot<-renderPlot({
    captainamericatweets = searchTwitter("Captain America",n = 100, lang = "en",geocode=input$select)
    #head(captainamericatweets)
    
    #using function getText to extract text part of tweets
    text <- sapply(captainamericatweets,function(x) x$getText())
    
    #converting latin1 characters to ASCII.
    text <- sapply(text,function(row) iconv(row, "latin1", "ASCII", sub = ""))
    
    # remove retweet entities
    text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
    #head(text)
    
    # remove at people
    text = gsub("@\\w+", "", text)
    # remove punctuation
    text = gsub("[[:punct:]]", "", text)
    # remove numbers
    text = gsub("[[:digit:]]", "", text)
    # remove html links
    text = gsub("http\\w+", "", text)
    
    #remove captain america civil war from texts
    text= gsub("Captain America","",text)
    text = gsub("Civil War","",text)
    text = gsub("war","",text)
    # remove unnecessary spaces
    text = gsub("[ \t]{2,}", "", text)
    text = gsub("^\\s+|\\s+$", "", text)
    
    # define "tolower error handling" function 
    try.error = function(x)
    {
      # create missing value
      y = NA
      # tryCatch error
      try_error = tryCatch(tolower(x), error=function(e) e)
      # if not an error
      if (!inherits(try_error, "error"))
        y = tolower(x)
      # result
      return(y)
    }
    # lower case using try.error with sapply 
    text = sapply(text, try.error)
    write.csv(text,file="text.csv")
    # remove NAs in some_txt
    #text = text[!is.na(text)]
    #names(text) = NULL
    
    # classify emotion
    class_emo = classify_emotion(text, algorithm="bayes", prior=1.0)
    
    # get emotion best fit
    emotion = class_emo[,7]
    
    # substitute NA's by "unknown"
    emotion[is.na(emotion)] = "unknown"
    
    # classify polarity
    class_pol = classify_polarity(text, algorithm="bayes")
    # get polarity best fit
    polarity = class_pol[,4]
    
    # data frame with results
    sent_df = data.frame(text=text, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)
    
    # sort data frame
    sent_df = within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
    write.csv(head(sent_df),file="sent_df.csv")
    
    # emotionplot<-ggplot(sent_df, aes(x=emotion)) +
    #  geom_bar(aes(y=..count.., fill=emotion)) +
    #  scale_fill_brewer(palette="Dark2") +
    #  labs(x="emotion categories", y="number of tweets") 
    # theme(title = "Sentiment Analysis of Tweets about Captain America\n(classification by emotion)")
    
    ggplot(sent_df, aes(x=emotion)) +
      geom_bar(aes(y=..count.., fill=emotion)) +
      scale_fill_brewer(palette="Dark2") +
      labs(x="emotion categories", y="number of tweets")
  })
  
  output$polarityplot<-renderPlot({captainamericatweets = searchTwitter("Captain America",n = 100, lang = "en",geocode=input$select)
  head(captainamericatweets)
  
  #using function getText to extract text part of tweets
  text <- sapply(captainamericatweets,function(x) x$getText())
  
  #converting latin1 characters to ASCII.
  text <- sapply(text,function(row) iconv(row, "latin1", "ASCII", sub = ""))
  
  # remove retweet entities
  text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
  head(text)
  
  # remove at people
  text = gsub("@\\w+", "", text)
  # remove punctuation
  text = gsub("[[:punct:]]", "", text)
  # remove numbers
  text = gsub("[[:digit:]]", "", text)
  # remove html links
  text = gsub("http\\w+", "", text)
  
  #remove captain america civil war from texts
  text= gsub("Captain America","",text)
  text = gsub("Civil War","",text)
  text = gsub("war","",text)
  # remove unnecessary spaces
  text = gsub("[ \t]{2,}", "", text)
  text = gsub("^\\s+|\\s+$", "", text)
  
  # define "tolower error handling" function 
  try.error = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # lower case using try.error with sapply 
  text = sapply(text, try.error)
  #nrow(text)
  # remove NAs in some_txt
  #text = text[!is.na(text)]
  #names(text) = NULL
  
  # classify emotion
  class_emo = classify_emotion(text, algorithm="bayes", prior=1.0)
  #class_emo
  # get emotion best fit
  emotion = class_emo[,7]
  #emotion
  # substitute NA's by "unknown"
  emotion[is.na(emotion)] = "unknown"
  
  # classify polarity
  class_pol = classify_polarity(text, algorithm="bayes")
  # get polarity best fit
  polarity = class_pol[,4]
  
  # data frame with results
  sent_df = data.frame(text=text, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)
  
  # sort data frame
  sent_df = within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
  #write.csv(sent_df,file="sent_df.csv")

  # plot distribution of polarity
  ggplot(sent_df, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="RdGy") +
    labs(x="polarity categories", y="number of tweets") #+
  # theme(title = "Sentiment Analysis of Tweets about Captain America\n(classification by polarity)",
  # plot.title = theme_text(size=12))
  })
  
  output$emotionswordcloud<-renderPlot({
    captainamericatweets = searchTwitter("Captain America",n = 100, lang = "en",geocode=input$select)
    head(captainamericatweets)
    
    #using function getText to extract text part of tweets
    text <- sapply(captainamericatweets,function(x) x$getText())
    
    #converting latin1 characters to ASCII.
    text <- sapply(text,function(row) iconv(row, "latin1", "ASCII", sub = ""))
    
    # remove retweet entities
    text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
    head(text)
    
    # remove at people
    text = gsub("@\\w+", "", text)
    # remove punctuation
    text = gsub("[[:punct:]]", "", text)
    # remove numbers
    text = gsub("[[:digit:]]", "", text)
    # remove html links
    text = gsub("http\\w+", "", text)
    
    #remove captain america civil war from texts
    text= gsub("Captain America","",text)
    text = gsub("Civil War","",text)
    text = gsub("war","",text)
    # remove unnecessary spaces
    text = gsub("[ \t]{2,}", "", text)
    text = gsub("^\\s+|\\s+$", "", text)
    
    # define "tolower error handling" function 
    try.error = function(x)
    {
      # create missing value
      y = NA
      # tryCatch error
      try_error = tryCatch(tolower(x), error=function(e) e)
      # if not an error
      if (!inherits(try_error, "error"))
        y = tolower(x)
      # result
      return(y)
    }
    # lower case using try.error with sapply 
    text = sapply(text, try.error)
    nrow(text)
    # remove NAs in some_txt
    #text = text[!is.na(text)]
    #names(text) = NULL
    
    # classify emotion
    class_emo = classify_emotion(text, algorithm="bayes", prior=1.0)
    class_emo
    # get emotion best fit
    emotion = class_emo[,7]
    emotion
    # substitute NA's by "unknown"
    emotion[is.na(emotion)] = "unknown"
    
    # classify polarity
    class_pol = classify_polarity(text, algorithm="bayes")
    # get polarity best fit
    polarity = class_pol[,4]
    
    # data frame with results
    sent_df = data.frame(text=text, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)
    
    # sort data frame
    sent_df = within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
    #write.csv(sent_df,file="sent_df.csv")
    
    emos = levels(factor(sent_df$emotion))
    nemo = length(emos)
    emo.docs = rep("", nemo)
    for (i in 1:nemo)
    {
      tmp = text[emotion == emos[i]]
      emo.docs[i] = paste(tmp, collapse=" ")
    }
    
    # remove stopwords
    emo.docs = removeWords(emo.docs, "captain")
    emo.docs = removeWords(emo.docs, "america")
    emo.docs = removeWords(emo.docs, "civil")
    emo.docs = removeWords(emo.docs, "war")
    # create corpus
    corpus = Corpus(VectorSource(emo.docs))
    tdm = TermDocumentMatrix(corpus)
    tdm = as.matrix(tdm)
    colnames(tdm) = emos
    
    # comparison word cloud
    comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                     scale = c(3,.5), random.order = FALSE, title.size = 1.5)
  })
  
  output$histogramscores<-renderPlot({
    captainamericatweets = searchTwitter(input$select1,n = 100, lang = "en",geocode="40.712940,-73.987920,3000mi")
    #head(captainamericatweets)
    
    #using function getText to extract text part of tweets
    text <- sapply(captainamericatweets,function(x) x$getText())
    
    #converting latin1 characters to ASCII.
    text <- sapply(text,function(row) iconv(row, "latin1", "ASCII", sub = ""))
    
    # remove retweet entities
    text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
    head(text)
    
    # remove at people
    text = gsub("@\\w+", "", text)
    # remove punctuation
    text = gsub("[[:punct:]]", "", text)
    # remove numbers
    text = gsub("[[:digit:]]", "", text)
    # remove html links
    text = gsub("http\\w+", "", text)
    
    #remove captain america civil war from texts
    text= gsub("Captain America","",text)
    text = gsub("Civil War","",text)
    text = gsub("war","",text)
    # remove unnecessary spaces
    text = gsub("[ \t]{2,}", "", text)
    text = gsub("^\\s+|\\s+$", "", text)
    
    # define "tolower error handling" function 
    try.error = function(x)
    {
      # create missing value
      y = NA
      # tryCatch error
      try_error = tryCatch(tolower(x), error=function(e) e)
      # if not an error
      if (!inherits(try_error, "error"))
        y = tolower(x)
      # result
      return(y)
    }
    # lower case using try.error with sapply 
    text = sapply(text, try.error)
    
    pos = readLines("positive_words.txt")
    neg = readLines("negative_words.txt")
    
    score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
    {
      
      scores = laply(sentences,
                     function(sentence, pos.words, neg.words)
                     {
                       
                       # split sentence into words with str_split (stringr package)
                       word.list = str_split(sentence, "\\s+")
                       words = unlist(word.list)
                       # compare words to the dictionaries of positive & negative terms
                       # find the first occurrence of the first argument in the second argument: 
                       pos.matches = match(words, pos.words)
                       neg.matches = match(words, neg.words)
                       # get the position of the matched term or NA
                       # we just want a TRUE/FALSE
                       pos.matches = !is.na(pos.matches)
                       neg.matches = !is.na(neg.matches)
                       # final score
                       score = sum(pos.matches) - sum(neg.matches)
                       return(score)
                     }, pos.words, neg.words, .progress=.progress )
      # data frame with scores for each sentence
      scores.df = data.frame(text=sentences, score=scores)
      return(scores.df)
    }
    
    nooftweets = c(length(text))
    movie<-c(text)
    #applying function score.sentiment
    scores = score.sentiment(movie, pos, neg, .progress='text')
    scores$movie = factor(rep(c(input$select1), nooftweets))
    write.csv(scores,file="scores.csv")
    histogram(data=scores, ~score|movie, main="Sentiment Analysis of movie",col = col, sub="Sentiment Score")
  })
  
  output$boxplotscores<-renderPlot({
    captainamericatweets = searchTwitter(input$select1,n = 100, lang = "en",geocode="40.712940,-73.987920,3000mi")
    #head(captainamericatweets)
    
    #using function getText to extract text part of tweets
    text <- sapply(captainamericatweets,function(x) x$getText())
    
    #converting latin1 characters to ASCII.
    text <- sapply(text,function(row) iconv(row, "latin1", "ASCII", sub = ""))
    
    # remove retweet entities
    text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
    head(text)
    
    # remove at people
    text = gsub("@\\w+", "", text)
    # remove punctuation
    text = gsub("[[:punct:]]", "", text)
    # remove numbers
    text = gsub("[[:digit:]]", "", text)
    # remove html links
    text = gsub("http\\w+", "", text)
    
    #remove captain america civil war from texts
    text= gsub("Captain America","",text)
    text = gsub("Civil War","",text)
    text = gsub("war","",text)
    # remove unnecessary spaces
    text = gsub("[ \t]{2,}", "", text)
    text = gsub("^\\s+|\\s+$", "", text)
    
    # define "tolower error handling" function 
    try.error = function(x)
    {
      # create missing value
      y = NA
      # tryCatch error
      try_error = tryCatch(tolower(x), error=function(e) e)
      # if not an error
      if (!inherits(try_error, "error"))
        y = tolower(x)
      # result
      return(y)
    }
    # lower case using try.error with sapply 
    text = sapply(text, try.error)
    
    pos = readLines("positive_words.txt")
    neg = readLines("negative_words.txt")
    
    score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
    {
      
      scores = laply(sentences,
                     function(sentence, pos.words, neg.words)
                     {
                       
                       # split sentence into words with str_split (stringr package)
                       word.list = str_split(sentence, "\\s+")
                       words = unlist(word.list)
                       # compare words to the dictionaries of positive & negative terms
                       # find the first occurrence of the first argument in the second argument: 
                       pos.matches = match(words, pos.words)
                       neg.matches = match(words, neg.words)
                       # get the position of the matched term or NA
                       # we just want a TRUE/FALSE
                       pos.matches = !is.na(pos.matches)
                       neg.matches = !is.na(neg.matches)
                       # final score
                       score = sum(pos.matches) - sum(neg.matches)
                       return(score)
                     }, pos.words, neg.words, .progress=.progress )
      # data frame with scores for each sentence
      scores.df = data.frame(text=sentences, score=scores)
      return(scores.df)
    }
    
    nooftweets = c(length(text))
    movie<-c(text)
    #applying function score.sentiment
    scores = score.sentiment(movie, pos, neg, .progress='text')
    scores$movie = factor(rep(c(input$select1), nooftweets))
    par(bty="l")
   # write.csv(scores,file="scores.csv")
    boxplot(score~movie, data=scores) #making a boxplot of sentiments
  })
}

shinyApp(ui=ui, server=server)
