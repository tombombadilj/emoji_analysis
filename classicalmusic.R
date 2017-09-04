#EmojiData Analysis
# this is an analysis based on the tutorial provided by Prismoji
# I changed some of the codes to be compatible to dplyr and tidyr
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
api_key <- '...'
api_secret <- '...'
access_token <- '...'
access_token_secret <- '...'
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

#read in emoji dictionary - code copied from tutorial 
emdict.la <- read.csv('emoticon_conversion_noGraphic.csv', header = F)
View(emdict.la)
# get ride of the header row
emdict.la <- emdict.la[-1, ]
#remove all row names
row.names(emdict.la) <- NULL
#assign new row names
names(emdict.la) <- c('unicode', 'bytes', 'name')
#adding emojiid - row number
emdict.la$emojiid <- row.names(emdict.la)

#read in second 
emdict.jpb <- read.csv('emDict.csv', header = F)
emdict.jpb <- emdict.jpb[-1, ]
row.names(emdict.jpb) <- NULL
names(emdict.jpb) <- c('name', 'bytes', 'rencoding')
emdict.jpb$name <- tolower(emdict.jpb$name)
emdict.jpb$bytes <- NULL

emojis <- merge(emdict.la, emdict.jpb, by = 'name')
emojis$emojiid <- as.numeric(emojis$emojiid)
emojis <- arrange(emojis, emojiid)

View(emojis)
dim(emojis)
# 841 * 5
#combined emoji dataset - name of the emoji, unicode, bytes - original text, 
# emohiid, rencoding (R coding of the emoji)

# creating a matrix with all NAs with nrow = number of tweets,
# and ncol = number of emojis
df.s <- matrix(NA, nrow = nrow(df), ncol = ncol(emojis))
dim(df.s)

#sapply regexpr (finding patterns) using rencoding as patterns to iterate
# through the tweets, case insensitive, and use bytes by bytes rather than
# character by character
system.time(df.s <- sapply(emojis$rencoding, regexpr, df$text, ignore.case =T,
                           useBytes = T))
# View(df.s)
# this will create a sparse matrix (when use in sapply it returns the number
# of first index where the pattern appears and if the pattern does not exist
# it returns -1)
names(df)
rownames(df.s) <- 1:nrow(df.s)
colnames(df.s) <- 1:ncol(df.s)
# converting df.s to data frame
df.t <- data.frame(df.s)
# copy tweeter id from the original database
df.t$tweetid <- df$id
#grabbing hashtag from original dataset
df.hash <- df %>% dplyr::select(id, hashtag)
df.u <- dplyr::left_join(df.t, df.hash, by = c("tweetid"="id")) 
dim(df.u)
# 3687 * 843

df.u <- df.u %>% dplyr::arrange(tweetid)
tweets.emojis.matrix <- df.u
names(tweets.emojis.matrix)

## create emoji count dataset
df_noid <- tweets.emojis.matrix[, c(1:841)]
count <- colSums(df_noid > -1)
summary(count)
which(count[2:length(count)] == max(count[2:length(count)]))
count[2:length(count)][which(count[2:length(count)] == max(count[2:length(count)]))]
# X396 is the most prevalent emoji with 53 tweets
length(count)
dim(emojis)

# adding count back to the original dataset
emojis.m <- cbind(count, emojis)
emojis.m <- dplyr::arrange(emojis.m, desc(count))
View(emojis.m)
total_emoji_used <- sum(emojis.m$count)
# a total of 546 emojis were used
unique_emojis <- emojis.m %>% dplyr::filter(count >= 1) %>% dplyr::summarise(tweets_with_emo = n())
# a total of 120 different emojis were used

# selecting emojis with more than one use
emojis.count <- emojis.m %>% dplyr::filter(count > 1)
# density = # of times an emoji is used/total number of tweets * 1000
# frequency per thousand tweets
emojis.count$dens <- round(1000*(emojis.count$count/nrow(df)), 1)
emojis.count$dens.sm <- (emojis.count$count + 1)/(nrow(df)+1)
emojis.count$rank <- as.numeric(row.names(emojis.count))

# printing summary statistics
names(emojis.count)
emojis.count.p <- emojis.count %>% select(name, dens, count, rank)
# top 10 used emojis
emojis.count.p %>% filter(rank<=10)
num.tweets <- nrow(df)
# number of tweets with emoji
num.tweets.with.emojis <- length(df_with[df_with > 0])
print(paste0("the total number of tweets included: ",num.tweets))
# how many emojis are used by each tweet
df_with <- rowSums(tweets.emojis.matrix[, c(1:841)]>-1)
# the highest number of emojis used by one text is 12
# number of tweets with emoji
num.tweets.with.emojis <- length(df_with[df_with > 0])
print(paste0("the total number of tweets with emojis included: ",num.tweets.with.emojis))
perc_with_emoji <- round(num.tweets.with.emojis/num.tweets*100, 1)
print(paste0("the percentage of tweets including emojis: ", perc_with_emoji))
print(paste0("the total number of emojis used: ", total_emoji_used ))
print(paste0("the total number of unique emojis used: ", unique_emojis))
max_emoji <- max(df_with)
print(paste0("the maximum number of emojis used in one tweet: ", max_emoji))
which(df_with == 12)
tweets.emojis.matrix[c(745,746,747,748),"tweetid"]
df %>% dplyr::filter(id %in% c("901491919632699392","901492158208897025",
                               "901492649550655488","901493188048941056")) %>%
  dplyr::select(text, url)
# all of them came from a photographer who promotes a diffferent person each
# time with plenty of hashtags

# generating the plot
df.plot <- subset(emojis.count.p, rank <= 10)
df.plot
xlab <- 'Rank'
ylab <- 'Overall Frequency (per 1,000 Tweets)'
setwd('../emoji_analysis/ios_9_3_emoji_files')

df.plot <- dplyr::arrange(df.plot, name);
imgs <- lapply(paste0(df.plot$name, '.png'), png::readPNG)
g <- lapply(imgs, grid::rasterGrob)
k <- 0.20 * (10/nrow(df.plot)) * max(df.plot$dens)
df.plot$xsize <- k
df.plot$ysize <- k
df.plot$ysize <- k * (df.plot$dens / max(df.plot$dens))
df.plot <- dplyr::arrange(df.plot, name);
g1 <- ggplot(data = df.plot, aes(x = rank, y = dens)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue4') +
  xlab(xlab) + ylab(ylab) +
  mapply(function(x, y, i) {
    annotation_custom(g[[i]], xmin = x-0.5*df.plot$xsize[i], xmax = x+0.5*df.plot$xsize[i], 
                      ymin = y-0.5*df.plot$ysize[i], ymax = y+0.5*df.plot$ysize[i])},
    df.plot$rank, df.plot$dens, seq_len(nrow(df.plot))) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(df.plot), 1), labels = seq(1, nrow(df.plot), 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.10 * max(df.plot$dens))) +
  theme(panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 14), 
        axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black')) + 
  ggtitle("Popular Emojis for Classical Music")
g1











