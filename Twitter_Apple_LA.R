
#libraries
#install.packages('ggmap')
library(twitteR)
library(ROAuth)
library(RCurl)
library(httr)

library(stringr)
library(plyr)
library(dplyr)
library(tm)

library(ggmap)
library(wordcloud)

# Establishing A Connection - Direct Method

key="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
secret="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

atoken =  "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
asecret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

setup_twitter_oauth(key, secret, atoken, asecret)

#Reading Lexicon positive and negative words
pos = readLines("positive-words.txt")
neg = readLines("negative-words.txt")

# Searching for Apple(iPhone) in Los Angeles
tweets = searchTwitter("apple+iphone",
                       n=2000,
                       lan="en",
                       until = "2019-03-30",
                       since = "2019-03-16",
                       geocode = "34.052235,-118.243683,150mi")


#extracting the text
tweettext = sapply(tweets,
                   function(x) x$getText())

#Text Cleaning
tweettext = lapply(tweettext, function(x) iconv(x, "latin1", "ASCII", sub=""))
tweettext = lapply(tweettext, function(x) gsub("htt.*",'',x))
tweettext = lapply(tweettext, function(x) gsub("#",'',x))
tweettext = unlist(tweettext)

# Function is called sentimentfun
sentimentfun = function(tweettext, pos, neg, .progress='non')
{
  scores = laply(tweettext,
                 function(singletweet, pos, neg)
                 {
                   #removing punctuations, control characters and digits
                   singletweet = gsub("[[:punct:]]", "", singletweet)
                   singletweet = gsub("[[:cntrl:]]", "", singletweet)
                   singletweet = gsub("\\d+", "", singletweet)
                   
                   #function defined for converting tweets to lowercase
                   tryTolower = function(x)
                   {
                     y = NA
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   # converting each single tweets to lowercase
                   singletweet = sapply(singletweet, tryTolower)
                   # splitting each string to words
                   word.list = str_split(singletweet, "\\s+")
                   words = unlist(word.list)
                   # matching each word to the list of positive and negatives
                   pos.matches = match(words, pos)
                   neg.matches = match(words, neg)
                   # extracting words other than NA
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   # calculating the score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos, neg, .progress=.progress )
  sentiment.df = data.frame(text=tweettext, score=scores)
  return(sentiment.df)
}

# Apply function scoring
scores = sentimentfun(tweettext, pos, neg, .progress='text')

# Feature extraction
tweetdate=lapply(tweets, 
                 function(x) x$getCreated())

tweetdate=sapply(tweetdate,
                 function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

isretweet=sapply(tweets, 
                 function(x) x$getIsRetweet())

retweetcount=sapply(tweets, 
                    function(x) x$getRetweetCount())

favoritecount=sapply(tweets, 
                     function(x) x$getFavoriteCount())


# Creating the Data Frame
data=as.data.frame(cbind(ttext=tweettext,
                         date=tweetdate,
                         isretweet=isretweet, 
                         retweetcount=retweetcount,
                         favoritecount=favoritecount,
                         score = scores$score,
                         product = "Apple Iphone",
                         city = "Los Angeles", country = "USA"))

# Removing duplicates
data2 = duplicated(data[,1])
data$duplicate = data2

# Create CSV file
write.csv(data, file= "apple_langeles.csv")