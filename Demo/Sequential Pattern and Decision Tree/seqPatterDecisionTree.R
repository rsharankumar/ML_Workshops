# updating to the latest R version
# installing/loading the package:
if(!require(installr)) {
  install.packages("installr"); require(installr)} #load / install+load installr
# using the package:
updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.

#package required
install.packages("arulesSequences")
library(arulesSequences)

setwd("C:\\Users\\skumarravindran\\Desktop\\Big Data analytics Sessions\\Sequential Patter and Decision Tree - TT3")
getwd()


# Data for sequence analysis
data(zaki)
summary(zaki)
as(zaki, "data.frame")
summary(zaki)

# Generate rules
seq_rules <- cspade(zaki, parameter = list(support = 0.55))
#Summary information about the rules generated
summary(seq_rules)

#The rules generated with the support details
as(seq_rules, "data.frame")

#################################


outlook = c('rainy', 'rainy', 'overcast', 'sunny', 'sunny', 
            'sunny', 'overcast', 'rainy', 'rainy', 'sunny', 
            'rainy', 'overcast', 'overcast', 'sunny')
humidity = c('high', 'high', 'high', 'high', 'normal', 
             'normal', 'normal', 'high', 'normal', 'normal', 
             'normal', 'high', 'normal', 'high')
temp = c('hot', 'hot', 'hot', 'mild', 'cool', 
         'cool', 'cool', 'mild', 'cool', 'mild', 
         'mild', 'mild', 'hot', 'mild')

windy = c('false', 'true', 'false', 'false', 'false', 
         'true', 'true', 'false', 'false', 'false', 
         'true', 'true', 'false', 'true')

play = c('no', 'no', 'yes', 'yes', 'yes', 
         'no', 'yes', 'no', 'yes', 'yes', 
         'yes', 'yes', 'yes', 'no')
game = data.frame(outlook, humidity, temp, windy, play)
head(game)
write.csv(game, "game.csv")

fit <- rpart(play ~ outlook + humidity + temp + windy, data=game)


#############

########### Building
# Classification Tree with rpart
library(rpart)

data(kyphosis)
head(kyphosis)

?rpart
# grow tree 
fit <- rpart(Kyphosis ~ Age + Number + Start, method="class", data=kyphosis)

# plot tree 
plot(fit, uniform=TRUE, main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=1)

################## ID#
IsPure <- function(data) {
  length(unique(data[,ncol(data)])) == 1
}


Entropy <- function( vls ) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}

InformationGain <- function( tble ) {
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}

TrainID3 <- function(node, data) {
  
  node$obsCount <- nrow(data)
  
  #if the data-set is pure (e.g. all toxic), then
  if (IsPure(data)) {
    #construct a leaf having the name of the pure feature (e.g. 'toxic')
    child <- node$AddChild(unique(data[,ncol(data)]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  } else {
    #calculate the information gain
    ig <- sapply(colnames(data)[-ncol(data)], 
                 function(x) InformationGain(
                   table(data[,x], data[,ncol(data)])
                 )
    )
    #chose the feature with the highest information gain (e.g. 'color')
    #if more than one feature have the same information gain, then take
    #the first one
    feature <- names(which.max(ig))
    node$feature <- feature
    
    #take the subset of the data-set having that feature value
    
    childObs <- split(data[ ,names(data) != feature, drop = FALSE], 
                      data[ ,feature], 
                      drop = TRUE)
    
    for(i in 1:length(childObs)) {
      #construct a child having the name of that feature value (e.g. 'red')
      child <- node$AddChild(names(childObs)[i])
      
      #call the algorithm recursively on the child and the subset      
      TrainID3(child, childObs[[i]])
    }
    
  }
  
  
  
}

#######
install.packages("data.tree")
library(data.tree)
data(mushroom)
mushroom

tree <- Node$new("mushroom")
TrainID3(tree, mushroom)
print(tree, "feature", "obsCount")

tree <- Node$new("game")
TrainID3(tree, game)
print(tree, "feature", "obsCount")
