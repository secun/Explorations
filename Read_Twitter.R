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

#load credentials
consumer_key <- "dNzadV3QlfF1hfAqey42ozUss"
consumer_secret<- "3eJSe7UOjKOeypkYchRNb0pN0gEIdTp4qDqECyCKB28hNYfSH0"
access_token <- "10124492-tN8oW8LGn1dHPnMdgSbSe3QXB8ZPkajWej7VSOKeD"
access_secret <- "zfcFyzRpdySnaisTuOnFdCj7lHz2LoLoFXExF6isH5EAx"

## access token method: create token and save it as an environment variable
create_token(
  app = "my_twitter_research_app",
  consumer_key <- "dNzadV3QlfF1hfAqey42ozUss",
  consumer_secret<- "3eJSe7UOjKOeypkYchRNb0pN0gEIdTp4qDqECyCKB28hNYfSH0",
  access_token <- "10124492-tN8oW8LGn1dHPnMdgSbSe3QXB8ZPkajWej7VSOKeD",
  access_secret <- "zfcFyzRpdySnaisTuOnFdCj7lHz2LoLoFXExF6isH5EAx")
  
#set up to authenticate
setup_twitter_oauth(consumer_key ,consumer_secret,access_token ,access_secret)

###################################################################################
#parameters (keywork to be searched, user to be analyzed)
keyword <- "@happymoodscore"
Tw_user <-  getUser("happymoodscore")
###################################################################################
#OPTION 1:Select the most retwweted tweet, fetch tweets associated with that hashtag,
#since the indicated date yyyy-mm-dd
tweets <- searchTwitter(keyword,n=1800, since='2015-04-01')
#strip retweets
tweets_no_retweets <- strip_retweets(tweets)
#convert to data frame using the twListtoDF function
df1 <- twListToDF(tweets_no_retweets) #extract the data frame save it locally
saveRDS(df, file="tweets.rds")
df1 <- readRDS("tweets.rds")

#Most retweetedd tweet:clean up any duplicate tweets from the data frame using #dplyr::distinct
df1<- distinct(df1)
#Most retweeted 
winner <-df1 %>% select(text,retweetCount,favoriteCount,screenName,id, )%>% 
  filter(retweetCount == max(retweetCount))
paste ("The most retweeted with :   " ,winner[2]  ,  "times, with the TEXT " , winner[1],
       "edited by " , winner[4] )

#Most liked
winner <-df1 %>% select(text,retweetCount,favoriteCount,screenName,id, )%>% 
  filter(favoriteCount == max(favoriteCount))
paste ("The most liked with :   " ,winner[3]  ,  "times, with the TEXT " , winner[1],
       "edited by " , winner[4] )

#PLOT: Frequency over twitter for the last days
g_title <- paste("Frequency of ", keyword ," Twitter statuses from past days")
ts_plot(df1, "12 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = g_title,
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )



###################################################################################
#OPTION 2: Search users tweeting about that topic
users <- search_users(keyword, n = 500)

# how many locations are represented
len <- length(unique(users$location))
paste( len , " locations are talking about this topic right now" )
#PLOT: Location of users
users %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Where Twitter users are from - unique locations")
###################################################################################
#OPTION 3: analyze data by users' timeline
################RETRIEVE twitter user timeline
#Retrieve and store max. number of tweets  on timeline
u_timeline <- userTimeline(Tw_user, n = 3200, includeRts=TRUE)
timeline_df <- twListToDF(u_timeline)
save(timeline_df, file = "Timeline.RData")


#Retrieve friends
friends <- Tw_user$getFriends() # who it follows
friends_df <- twListToDF(friends)
save(friends_df, file = "u_friends.RData")
print (paste( length(friends_df$id), " friends"))

#Retrieve followers
followers <- Tw_user$getFollowers() # its followers
followers_df <- twListToDF(followers)
save(followers_df, file = "u_followers.RData")
print (paste( length(followers_df$id), " followers"))

################ANALYZE user's engagement
v_t <- paste("User engagement:", Tw_user$name)
ggplot(timeline_df) + 
  geom_histogram(aes(x=retweetCount+1),binwidth=0.1, fill='red') +
  geom_histogram(aes(x=favoriteCount+1), binwidth=0.1,fill='blue',alpha=0.5) + 
  scale_fill_manual( values=c("red","blue"),labels=c("retweetCount","favoriteCount"))+
  scale_x_log10()  +
  labs( x = "Number of actions (log10 scale)", y = "Number of times", 
        title= v_t, caption= "Red - retweetCount, Blue-favoriteCount")



################ANALYZE FRIENDS AND FOLLOWERS
library(tidyverse)
library(tidyquant)
################Languages the followers speak
#load("u_friends.RData")
#load("u_followers.RData")

followers_df %>%
  count(lang) %>%
  droplevels() %>%
  ggplot(aes(x = reorder(lang, desc(n)), y = n)) +
  geom_bar(stat = "identity", colour = palette()[1], fill = palette()[1], alpha = 0.8) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "language ISO 639-1 code",
       y = "number of followers")

################Top-20 most influenctial followers
followers_df %>%
  mutate(date = as.Date(created, format = "%Y-%m-%d"),
         today = as.Date("2018-09-01", format = "%Y-%m-%d"),
         days = as.numeric(today - date),
         statusesCount_pDay = statusesCount / days) %>%
  select(screenName, followersCount, statusesCount_pDay) %>%
  arrange(desc(followersCount)) %>%
  top_n(20)

################Top-20 most active users
followers_df %>%
  mutate(date = as.Date(created, format = "%Y-%m-%d"),
         today = as.Date("2018-09-01", format = "%Y-%m-%d"),
         days = as.numeric(today - date),
         statusesCount_pDay = statusesCount / days) %>%
  select(screenName, followersCount, statusesCount_pDay) %>%
  arrange(desc(statusesCount_pDay)) %>%
  top_n(20)


################Create word-cloud by user timeline
install.packages("tm")
install.packages("wordcloud2")
install.packages("stringr")
library(tm)
library(wordcloud)
library(stringr)

data_cloud<-Corpus(VectorSource(timeline_df$text))
#Clean data
data_cloud<-tm_map(data_cloud,stripWhitespace)
data_cloud<-tm_map(data_cloud,tolower)
data_cloud<-tm_map(data_cloud,removeNumbers)
data_cloud<-tm_map(data_cloud,removePunctuation)
data_cloud<-tm_map(data_cloud,removeWords,stopwords('english'))
data_cloud<-tm_map(data_cloud,removeWords,stopwords('spanish'))
data_cloud<-tm_map(data_cloud,removeWords,c("will","and","the","our","that","for","are","also","more","has","must","have","can","get"))
data_cloud<-tm_map(data_cloud,removeWords,c("http", "valaafshar", "amp", "still", "business"))
data_cloud<-tm_map(data_cloud,removeWords,c("camino", "via", "new" ,"way","caminodesantiago", "followthecamino" ,"santiago", "stjamesway"))


cloud_view <- str_replace_all(data_cloud,"[^[:graph:]]", " ") 
wordcloud(words=cloud_view, scale=c(2,0.1), max.words=180, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))