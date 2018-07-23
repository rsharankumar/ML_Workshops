#### Recommendation Engine
#### For the Cognitive Analytics 101
## Code reference : R Data Science Essential book by Sharan Kumar Ravindran and Raja B Koushik

# Set your working directory accordingly
setwd("C:\\Users\\skumarravindran\\Desktop\\Cognitive Analytics 101\\Demo\\Recommendation")
getwd()


# reading the dataset
rdata <- read.csv("Data/following.csv")
head(rdata, 10)



# Pivoting the data
install.packages("data.table")
library(data.table)
pivoting <- data.table(rdata)
pivotdata<-dcast.data.table(pivoting, Items ~ UserID, fun.aggregate=length, value.var="UserID")

head(pivotdata)
colnames(pivotdata)

write.csv(pivotdata, "Data/pivot-follows.csv")

# After deletion of the index column and the null user

# Read the pivoted data
ubs<-read.csv("Data/pivot-follows.csv")
head(ubs)
colnames(ubs)

# Function to calculate the cosine between two vectors
getCosine <- function(x,y) 
{
  dat <- cbind(x,y)
  #f <- as.matrix(dat)
  f <- as.data.frame(dat)
  # Remove the rows with zeros
  datn<- f[-which(rowSums(f==0)>0),]
  #colnames(datn)<-c("x","y")
  #dat <- as.data.frame(datn)
  if(nrow(datn) > 2)
  {
    this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  }
  else
  {
    this.cosine <- 0
  }
  return(this.cosine)
}


# Create a placeholder dataframe listing item vs. item
ubs.score  <- matrix(NA, nrow=ncol(ubs),ncol=ncol(ubs),dimnames=list(colnames(ubs),colnames(ubs)))


# Lets fill in those empty spaces with cosine similarities
# Loop through the columns
for(i in 1:ncol(ubs)) {
  # Loop through the columns for each column
  for(j in 1:ncol(ubs)) {
    # Fill in placeholder with cosine similarities
    ubs.score[i,j] <- getCosine(data.matrix(ubs[i]),data.matrix(ubs[j]))
  }
  print(i)
}


# Back to dataframe - Similarity matrix
ubs.score <- as.data.frame(ubs.score)
head(ubs.score)


# Get the top 10 neighbours for each
user.neighbours <- matrix(NA, nrow=ncol(ubs.score),ncol=11,dimnames=list(colnames(ubs.score)))
for(i in 1:ncol(ubs)) 
{
  # Setting threshold for avoiding zeros
  n <- length(ubs.score[,i])
  thres <- sort(ubs.score[,i],partial=n-10)[n-10]
  if(thres > 0.10)
  {
    # Choosing the top 10 recommendation
    user.neighbours[i,] <- (t(head(n=11,rownames(ubs.score[order(ubs.score[,i],decreasing=TRUE),][i]))))
  }
  else
  {
    user.neighbours[i,] <- ""
  }
}
head(user.neighbours, 10)
tail(user.neighbours, 20)
# Writing the recommendation to a file
write.csv(user.neighbours, "Data/SimilarUsersresults.csv")


allrec <- ""

# getting the item to recommend
for(i in 1:nrow(user.neighbours)) 
{
  # Setting threshold for avoiding zeros
  for (j in 2:3)
  {
    nItem <- user.neighbours[i,j]
    rname <- as.data.frame(nItem)
    rname <- rownames(rname)
    n <- as.numeric(substring(nItem, 2,3))
    new <- subset(rdata, UserID == n)
    usr <- rname
    rec <- cbind(usr,data.frame(new$Items))
    allrec <- rbind(allrec,rec)
  }
  print (i)
}
allrec <- allrec[complete.cases(allrec),]
colnames(allrec) <- c("UserID","Items")
head(allrec, 10)

# recommendation to the users 
install.packages("sqldf")
require(sqldf)
newItems <- sqldf('SELECT * FROM allrec EXCEPT SELECT * FROM rdata')
head(newItems, 10)

