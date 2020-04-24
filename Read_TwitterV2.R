 #Load packages
install.packages("twitteR")
install.packages("rtweet")
install.packages("tidyquant")
install.packages ("tidyverse")
library("tidyverse")
library("tidyquant")
library("rtweet")
library("twitteR")
library("dplyr")
library("ggplot2")
library(wordcloud)
#load credentials
consumer_key <- "dNzadV3QlfF1hfAqey42ozUss"
consumer_secret<- "3eJSe7UOjKOeypkYchRNb0pN0gEIdTp4qDqECyCKB28hNYfSH0"
access_token <- "10124492-tN8oW8LGn1dHPnMdgSbSe3QXB8ZPkajWej7VSOKeD"
access_secret <- "zfcFyzRpdySnaisTuOnFdCj7lHz2LoLoFXExF6isH5EAx"

## access token method: create token and save it as an environment variable
create_token(
  app = "my_twitter_research_app",
  consumer_key ,  consumer_secret,   access_token ,   access_secret )
#set up to authenticate
setup_twitter_oauth(consumer_key ,consumer_secret,access_token ,access_secret)

###################################################################################
#parameters (keywork to be searched, user to be analyzed)
keyword <- "secun"
keyword <- "pablocasado_"
keyword <- "pnique"

###################################################################################
#######Retrieve tweets
tweets <- userTimeline(keyword,n=3200,excludeReplies = TRUE)
#Convert to df
tweets.df <- twListToDF(tweets)

# tweet #5
tweets.df[5, c("id", "created", "screenName", "replyToSN",
                 "favoriteCount", "retweetCount", "longitude", "latitude", "text")]
# print tweet #5 and make text fit for slide width
writeLines(strwrap(tweets.df$text[5], 60))
#######Build corpus
library(tm)
myCorpus <- Corpus(VectorSource(tweets.df$text))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), 
                         c("a", "as","than","at","no","the","in","and", "in", "for","of","from")))
myStopwords <- c(setdiff(stopwords('spanish'),myStopwords))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
# keep a copy for stem completion later
myCorpusCopy <- myCorpus

#######Count specific word frequency
wordFreq <- function(corpus, word) {
  results <- lapply(corpus,
                    function(x) { grep(as.character(x), pattern=paste0("\\<",word)) }
  )
  sum(unlist(results))
}
wordFreq(myCorpusCopy, "data")

#######Build term document matrix
tdm <- TermDocumentMatrix(myCorpus,qt(c(.025, .975), df=5)
                          control = list(wordLengths = c(1, Inf)))
# inspect frequent words
freq_limit <- 5
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= freq_limit)
df <- data.frame(term = names(term.freq), freq = term.freq)

ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))
#######Word cloud

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]
# plot word cloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)


#######Find associations
# which words are associated with 'strategy'?
findAssocs(tdm, "strategy", 0.2)
