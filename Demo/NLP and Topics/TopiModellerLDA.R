# Demo for Congnitive Analytics 101
# Code reference : https://eight2late.wordpress.com

#package installation
install.packages("SnowballC")

#text mining library
library(tm)
library(SnowballC)
library(topicmodels)

# set default directory
setwd("C:\\Users\\skumarravindran\\Desktop\\Cognitive Analytics 101\\Demo\\NLP and Topics\\Documents")

#list of the sample text files
filenames <- list.files(getwd(),pattern="*.txt")

# read the text files
files <- lapply(filenames,readLines)

# create corpus from the vector files
docs <- Corpus(VectorSource(files))

#checking a document
writeLines(as.character(docs[[5]]))

# processing the text
docs <-tm_map(docs,content_transformer(tolower))


#removing symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, ".")
docs <- tm_map(docs, toSpace, """)
docs <- tm_map(docs, toSpace, """)


#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Good practice to check every now and then
writeLines(as.character(docs[[5]]))
#Stem document
docs <- tm_map(docs,stemDocument)


#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
#convert rownames to filenames
rownames(dtm) <- filenames

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 5

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))


#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))


#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))


#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))

