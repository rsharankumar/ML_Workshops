#### Sentiment Analysis
#### For the Cognitive Analytics 101
## Code reference: from book Mastering Social Media Mining from R by Sharan Kumar Ravindran and Vikram Garg

setwd("C:\\Users\\skumarravindran\\Desktop\\Cognitive Analytics 101\\Demo\\Twitter Sentiment Analytics")
getwd()

t <- read.csv("Data\\telstra.csv")
head(t)
o <- read.csv("Data\\outage.csv")
head(o)

t <- rbind(t,o)
nrow(t)
Telstr_Tweets <- t$text
length(Telstr_Tweets)

#####################
catch.error = function(x)
{
  # let us create a missing value for test purpose
  y = NA
  # try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}

cleanTweets<- function(tweet){
  # Clean the tweet for sentiment analysis
  #  remove html links, which are not required for sentiment analysis
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  # First we will remove retweet entities from the stored tweets (text)
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Then remove all "#Hashtag"
  tweet = gsub("#\\w+", " ", tweet)
  # Then remove all "@people"
  tweet = gsub("@\\w+", " ", tweet)
  # Then remove all the punctuation
  tweet = gsub("[[:punct:]]", " ", tweet)
  # Then remove numbers, we need only text for analytics
  tweet = gsub("[[:digit:]]", " ", tweet)
  # finally, we remove unnecessary spaces (white spaces, tabs etc)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  # if anything else, you feel, should be removed, you can. For example "slang words" etc using the above function and methods.
  # Next we'll convert all the word in lowar case. This makes uniform pattern.
  tweet = catch.error(tweet)
  tweet
}

cleanTweetsAndRemoveNAs<- function (Tweets) {
  TweetsCleaned = sapply(Tweets, cleanTweets)
  # Remove the "na" tweets from this tweet list
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  names(TweetsCleaned) = NULL
  # Remove the repetitive tweets from this tweet list
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned
}

Telstr_Tweets_Cleaned = cleanTweetsAndRemoveNAs(Telstr_Tweets)

head(Telstr_Tweets_Cleaned)
write.csv(Telstr_Tweets_Cleaned, "Data\\telstraCleaned.csv")
#### positive and negative
opinion.lexicon.pos = scan('Data/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
opinion.lexicon.neg = scan('Data/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')
head(opinion.lexicon.neg)
head(opinion.lexicon.pos)

# adding few words to the dictionary
pos.words = c(opinion.lexicon.pos,'upgrade')
neg.words = c(opinion.lexicon.neg,'wait', 'waiting', 'wtf', 'cancellation')


################ score

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

install.packages("stringr")
install.packages("plyr")
library(stringr)
library(plyr)
TelstraResult = score.sentiment(Telstr_Tweets_Cleaned, pos.words , neg.words)

head(TelstraResult)
nrow(TelstraResult)
write.csv(TelstraResult, "Data\\scroing.csv")
