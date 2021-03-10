# Program: Functions
# Author: Maya Lu
# Date: March 8, 2021

# =============================================
# CLUSTERING
# 1) define function to display  frequent words
clusterFreq <- function(k, dat) {
  out <- kmeans(dat, centers = k, nstart = 3)
  
  # create freq words dataframe
  freqWords <- matrix(NA, 10, k)
  freqWords <- data.frame(freqWords)
  
  for (i in 1:k) {
    clu <- i
    centroid.i <- out$centers[clu,]
    freqWords[,i] <- names(sort(centroid.i, decreasing = T)[1:10])
  }
  
  # return data frame
  return(freqWords)
}

# -------------------

# 2) function that displays distinct words
clusterDistinct <- function(k, dat)  {
  out <- kmeans(dat, centers = k, nstart = 3)
  
  # create distinct words dataframe
  distinctWords <- matrix(NA, 10, k)
  distinctWords <- data.frame(distinctWords)
  
  # populate data frame
  for (i in 1:k) {
    clu <- i
    centroid.i <- out$centers[clu,]
    centroid.noti <- apply(out$centers[-clu,], 2, mean)
    distinctWords[,i] <- names(sort(centroid.i - centroid.noti, decreasing = T)[1:10])
  }
  
  # return data frame
  return(distinctWords)
}

# ===================================================
# SENTIMENT ANALYSIS

# --------------------
# function that outputs ratio of positive word to total words for each email in dat
calcSentiment <- function(dat) {
  countPos = rep(NA, nrow(dat))
  countNeg = rep(NA, nrow(dat))
  
  for (i in 1:nrow(dat)) {
    wordsIdx = which(t(dat[i,]) > 1)
    wordNames <- colnames(dat)
    words <- wordNames[wordsIdx] 
    
    countPos[i] = sum(as.numeric(words %in% pos))
    countNeg[i] = sum(as.numeric(words %in% neg))
    
  }
  
  return(sum(countPos)/sum(countPos, countNeg))
}

# -------------------
# function that returns the model of sentiment analysis for a certain keyword
calcSentimentRegression <- function(keyword, dat) {
  countPos = c()
  countNeg = c()
  keyword.counts = dat[,keyword]
  
  for (i in 1:nrow(dat)) {
    wordsIdx = which(t(dat[i,]) > 1)
    wordNames <- colnames(dat)
    words <- wordNames[wordsIdx] 
    
    countPos[i] = sum(as.numeric(words %in% pos))
    countNeg[i] = sum(as.numeric(words %in% neg))
  }
  
  df <- data.frame(keyword = keyword.counts, countPos = countPos, countNeg = countNeg)
  df$ratio <- df$countPos/(df$countPos + df$countNeg)
  df$ratio[df$countNeg == 0 & df$countPos == 0] <- 0.5
  
  return(lm(ratio ~ keyword, df))
}



# -------------------
# function for sentiment analysis visualization
load("processedDTM.RData")
temp <- calcSentimentRegression("china", q5)
temp$coefficients[[2]]
summary(temp)$coefficients[[8]]


coef <- c()

for(i in 1:length(qdf)){
  coef[i] <- calcSentimentRegression("libya", qdf[[i]])[[2]]
}

coef_df <- data.frame(coef)

library(ggplot2)

visual_by_qtr <- function(keyword, df){
  
  
}

ggplot(c(1,2,3,4)) + geom_point()



visual_by_qtr("china", emails_clean)

