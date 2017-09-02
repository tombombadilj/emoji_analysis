#EmojiData Analysis
# this is an analysis based on the tutorial provided by 
getwd()
ls()
setwd("/Users/jasonchiu0803/Desktop/data_science_projects/emoji_analysis")

#downloading and loading necessary packages
install.packages("twitteR","reshape","ROAuth","RJSONIO")
install.packages("leaflet")
library(leaflet)
library(reshape)
library(twitteR)
library(ROAuth)
library(RJSONIO)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(htmltools)

#Setting up twitter 
api_key <- 'vqkC5KKVv7q56WI3ItfzVLWvg'
api_secret <- 'KLeHGMATYpCtlBGGzc7PrTr5cAseitCku8pTqloyOvx6W4BTfH'
access_token <- '4827569197-a2KGhNPs3jNEIsDoIe72ylKORpYt4JGDv8Uy88X'
access_token_secret <- 'Lls08SALl3kOIc2OpeIYQBcFeGCpOtNztzOq9oa4W1KvC'
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# downloading tweets with the following hashtags or keywords
set.seed(20170902)

extract_tweets <- function(keywords, RT, since_date, until_date){
  raw_tweets <- searchTwitter(keywords,
                              n = 50000, lang = "en", 
                              since = since_date, until = until_date)
  if (RT == TRUE) {
    data = twListToDF(strip_retweets(raw_tweets, strip_manual = TRUE, strip_mt = TRUE))
  } else {
    data = twListToDF(raw_tweets)
  }
  data$hashtag <- keywords
  data$text <- iconv(data$text, 'latin1', 'ASCII', 'byte')
  data$url <- paste0('https://twitter.com/', data$screenName, '/status/', data$id)
  return(data)
}

start_date = "2017-08-25"
end_date = "2017-09-02"
keyword1 <- '#ClassicalMusic'
keyword2 <- '#Composer'
keyword3 <- '#MusicHistory'
keyword4 <- '#Opera'
keyword5 <- '#Classicfm'
classical_music <- extract_tweets(keyword1, TRUE, start_date, end_date)
composer <- extract_tweets(keyword2, TRUE, start_date, end_date)
musichistory <- extract_tweets(keyword3, TRUE, start_date, end_date)
opera <- extract_tweets(keyword4, TRUE, start_date, end_date)
classicfm <- extract_tweets(keyword5, TRUE, start_date, end_date)

dim(classical_music)
sum(duplicated(classical_music$text))
#88 duplicated tweets in classical_music
dim(composer)
sum(duplicated(composer$text))
# 55 duplicated tweets in composer
dim(musichistory)
sum(duplicated(musichistory$text))
#4 duplicated tweets in musichistory
dim(opera)
sum(duplicated(opera$text))
#13 duplicated tweets in opera
dim(classicfm)
sum(duplicated(classicfm$text))
# 0 duplicated tweets in classicfm

#extracting only the first row from the duplicates
library(plyr)
classical_nd <- ddply(classical_music,.(text),function(x) head(x,1))
composer_nd <- ddply(composer, .(text), function(x) head(x,1))
history_nd <- ddply(musichistory, .(text), function(x) head(x,1))
opera_nd <- ddply(opera, .(text), function(x) head(x,1))
classicfm_nd <- ddply(classicfm, .(text), function(x) head(x,1))

#combining everything into one dataset using append
df <- bind_rows(classical_nd,composer_nd)
df <- bind_rows(df, history_nd)
df <- bind_rows(df, opera_nd)
df_all <- bind_rows(df, classicfm_nd)
dim(df_all)
# a total of 4161 tweets were included
sum(duplicated(df_all$text))
# removing duplicated rows from the dataset
df <- ddply(df_all, .(text), function(x) head(x,1))
dim(df)
# 3687 tweets were included

# exploring each variable
names(df)
# text - tweets, 
head(df$text,3)

#hashtag
table(df$hashtag)
barchart <- ggplot(df, aes(hashtag))
barchart + geom_bar() + ggtitle("Hashtag used (8/25 - 9/1)") + 
  xlab("Hashtags") + ylab("Count") + theme_classic() +
  scale_x_discrete(limits=c("#ClassicalMusic","#Composer","#Opera","#MusicHistory","#Classicfm"))
# classicalmusic was the most used hashtag for the data followed by composer,
# opera, music history, and classicfm

# favorited - indicates whether this tweet has been liked by authenticating users
table(df$favorited)
# none of the tweets have been liked by any verified users

# favoriteCount - the number of times the tweet had been liked
summary(df$favoriteCount)
#max 243
df %>% filter(favoriteCount == 243) %>% select(text)
df %>% summarise(fav_max = max(favoriteCount), fav_min = min(favoriteCount))
# most popular tweet is about bob pepper's classical music album cover
favorite_tweets <- ggplot(df, aes(df$favoriteCount))
favorite_tweets + geom_histogram(bins = 30) + 
  ggtitle("Favorite Counts (8/25 - 9/1)") + xlab("No. of Likes") +
  ylab("Count") + theme_classic()
# 0 to 243 it's got a long right tail

#replyToSN - in reply to screen name
df %>% select(replyToSN) %>% na.omit() #%>% count()
# only 151 tweets are done in responding to a sn
#whenever the person tag someone else in the tweet

#created - when the tweet was created
df %>% select(created) %>% head(2)
# distribution of tweets by date
df$date_extracted <- date(df$created)
df$day_of_week <- wday(df$created, label = TRUE)
df$hour_extract <- hour(df$created)

df %>% select(day_of_week, date_extracted)
# 9/1 friday, 8/31 Thursday, 8/30 wednesday, 8/29 tuesday, 8/28 monday, 
# 8/27 sunday, 8/26 saturday, 8/25 friday
#histogram based on date
a <- ggplot(df, aes(date_extracted))
a + geom_bar() + ggtitle("Tweets by Date (8/25 - 9/1)") + xlab("Date") +
  ylab("Count") + theme_classic()
df %>% select(date_extracted) %>% group_by(date_extracted) %>% summarize(count= n())
# on sunday, there was the lowest number of tweets. the trend decreases on the
# weekend, and then increased again from monday to friday

heatmap_data <- df %>% dplyr::group_by(date_extracted, hour_extract) %>% 
  dplyr::summarise(counts = n())
heatmap_data
ggplot(heatmap_data, aes(date_extracted, hour_extract, fill = counts)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") + theme_classic() +
  ggtitle("Tweeting heatmap") + xlab("Date") + ylab("Hour")
# early to late afternoon people are the most frequent period that people 
# tweets about classical music

# truncated - if reteweet is over 140 characters it will be truncated
table(df$truncated)

#reply to SID (status id) reply to UID (user ID)
# statusSource -it tells you where they upload it from
unique(df$statusSource)
#178 different unique sources

# retweet count-  number of time the text has been retweeted
summary(df$retweetCount)
# maximum number of retweet is 800

retweet_tweets <- ggplot(df, aes(df$retweetCount))
retweet_tweets + geom_histogram(bins = 30) + 
  ggtitle("Retweet Counts (8/25 - 9/1)") + xlab("No. of Likes") +
  ylab("Count") + theme_classic()
# most tweets were not retweeted, 
# but there was a couple tweets that were tweeted 5849 times

#isRetweet
table(df$isRetweet)
# no retweets were inluded in the current analysis 

#retweeted - no text was retweeted by authenticated users (verified)
table(df$retweeted)

# long and lat
df %>% dplyr::select(longitude, latitude) %>% na.omit() %>% dplyr::count()
138/nrow(df)*100
# only 138 tweets have a certain location related to them (3.74% had location
# data)
names(df)

df_leaflet <- df %>% dplyr::select(text, longitude, latitude) %>% na.omit()
df_leaflet$longitude <- as.numeric(df_leaflet$longitude)
df_leaflet$latitude <- as.numeric(df_leaflet$latitude)
sapply(df_leaflet, class)

leaflet(df_leaflet) %>% addTiles() %>% 
  addMarkers(~longitude, ~latitude, popup = ~htmlEscape(text))

nrow(df)
ncol(df)
# the df has 3687 rows * 21 cols
#saving the datafile to a csv
write.csv(df, "musictweets_9.2.17.csv")



