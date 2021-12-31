library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(ggplot2)
library(httr)
library(wordcloud)
library(syuzhet)
library(tm)
library(stringr)
library(RCurl)
library(openssl)
library(httpuv)
library(base64enc)
library(tidytext)


api_key <- "XXXXX"
api_secret <- "XXXXXX"
access_token <- "XXXXX"
access_secret <- "XXXXX"

#Setup the OAuth

setup_twitter_oauth(api_key, api_secret, access_token, access_secret)

#finding trend locations available on twitter
all_locations = availableTrendLocations()
View(all_locations) #shows all trend locations in a dataframe in new window

#finding trending topic in a location
woe_id = subset(all_locations,name=='Lucknow') #shows row with details of Lucknow
View(woe_id)

woe_id = subset(all_locations,name=='Pune')$woeid #shows woeid column value for Pune 
View(woe_id)

trending_topics = getTrends(woe_id)
View(trending_topics)
# search for tweets in Pune
tweets = searchTwitter(searchString = "Airtel", n=5000, lang = "en")
tweets #show the tweets fetched
length(tweets) #shows the no. of tweets fetched

#Convert it to Dataframe to save in a file

tweets_df = ldply(tweets,function(t)t$toDataFrame())
View(tweets_df)

#Save to a CSV file
write.csv(tweets_df, "D:\\SEM 2\\Social Media Analytics\\tweetsPune.csv") 

#Step 3 - Data Cleaning
text = sapply(tweets, function(x)x$getText())
View(text)
text

#Remove all the @
text = gsub("@\\w+", "", text)
text

# Cleaning steps, remove people name, RT etc.
some_txt1 = gsub("(RT)((?:\\b\\w*@\\w+)+)","",text)
some_txt1 = gsub("RT : ","",text)
some_txt1
#cleaning 2 remove hyperlinks

some_txt2 = gsub("http[^[:blank:]]+","", some_txt1)
some_txt2

#cleaning 3 remove people names

some_txt3 = gsub("@\\w+","",some_txt2)
some_txt3

#Cleaning 4 remove punctuations

some_txt4 = gsub("[[:punct:]]","",some_txt3)
some_txt4

#Cleaning 5 remove non alpha numeric words

some_txt5 = gsub("[^[:alnum:]],[^[:space:]]","",some_txt4)
some_txt5

#remove emoji
some_txt_remove_emoji = gsub("[^\x01-\x7F]", "", some_txt5)
some_txt_remove_emoji

some_txt_remove_newline = gsub("[\r\n]", "", some_txt_remove_emoji)
some_txt_remove_newline

some_txt = some_txt_remove_newline

#Exporting to Excel
write.csv(some_txt, "ptweets.csv")
getwd() #get working directory
#setwd() #set working directory

#Cleaning for Word Corpus
some_txt6 = Corpus(VectorSource(some_txt5))
some_txt6 [[1]]$content #shows 1st tweet content

some_txt6 = tm_map(some_txt6, content_transformer(tolower))

some_txt6 = tm_map(some_txt6,removeWords,stopwords("english"))
some_txt6

some_txt6 = tm_map(some_txt6, stripWhitespace)

#Building word cloud

pal = brewer.pal(8,"Dark2")
pal

wordcloud(some_txt6, min.freq = 20,max.words = Inf, width = 2000, height = 1000,
          random.order = FALSE, colors = pal )

#RUN FROM DATAFRAME SOME_TXT5 ONLY AND NOT WITH CORPUS
#sentiment analysis code
#mysentiment <- get_nrc_sentiment(some_txt5)

SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")


# bing test
mysentiment <- get_sentiment(some_txt5, lexicon = "bing")
mysentiment <- get_sentiments("bing")
mysentiment

get_sentiments("bing") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)

library(tidyr)

mysentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)