library(tidyverse)
library(wordcloud)
library(wordcloud2)
library(tm)

df <- read_csv("C:/Users/unaeem/Dropbox (OPML)/Machine Learning/nltk/WordCloud_Imrun.csv")
words <- read_csv("C:/Users/unaeem/Dropbox (OPML)/Machine Learning/nltk/MaxWords.csv")

df <- df %>%
  mutate(date = as.Date(df$time, "%Y/%m/%d"))

before <- df %>%
  filter(date<="2018-07-25")
after <- df %>%
  filter(date>"2018-07-25")

word_clound <- function(data) {
  text <- data$tweet
  docs <- Corpus(VectorSource(text))
  dtm <- TermDocumentMatrix(docs)
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df1 <- data.frame(word = names(words),freq=words)
  
  wordcloud2(data=df1, size=1.6, color='random-dark')
}

cloud_before <- word_clound(before)


text <- df$tweet
docs <- Corpus(VectorSource(text))
dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df1 <- data.frame(word = names(words),freq=words)

set.seed(7924) # for reproducibility 
wordcloud(words = df1$word, freq = df1$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud2(data=df1, size=1.6, color='random-dark')

words %>%
  ggplot() + geom_freqpoly(aes(x = month, y = freq))
