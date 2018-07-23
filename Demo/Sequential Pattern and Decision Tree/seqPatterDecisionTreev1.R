# updating to the latest R version
# installing/loading the package:
if(!require(installr)) {
  install.packages("installr"); require(installr)} #load / install+load installr
# using the package:
updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.

#package required
install.packages("arulesSequences")
library(arulesSequences)

# setting directory location
setwd("C:\\Users\\skumarravindran\\Desktop\\Big Data analytics Sessions\\Sequential Patter and Decision Tree - TT3")
getwd()


# Data for sequence analysis
data(zaki)
summary(zaki)
as(zaki, "data.frame")
summary(zaki)
?cspade
# Generate rules
seq_rules <- cspade(zaki, parameter = list(support = 0.55))
#Summary information about the rules generated
summary(seq_rules)

#The rules generated with the support details
as(seq_rules, "data.frame")
head(zaki,10)

#################################
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

###########

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


