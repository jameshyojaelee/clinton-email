# CHALLENGE 2 - Initial Data Analysis
# Name: Maya Lu
# Date: March 6th, 2021

# -----------------------

# SET UP 
rm(list = ls())
library(tidyr)
emails <- read.csv("Clinton.csv")
table(emails$Recipient)

# create normalized version of dtm
dtm <- emails[, 10:3009]
row_lengths = apply(dtm, 1, function(z) sqrt(sum(z^2)))
dtm_norm <- dtm/row_lengths
dtm_norm <- na.omit(dtm_norm)

# -------------------------

# K-MEANS CLUSTERING
k <- 10
k10_norm <- kmeans(dtm_norm, centers = k, nstart = 3)
k10 <- kmeans(dtm, centers = k, nstart = 3)

table(k10$cluster)

# view clusters
k10_freqWords <- data.frame(
  cluster1 = rep(NA, 10),
  cluster2 = rep(NA, 10),
  cluster3 = rep(NA, 10),
  cluster4 = rep(NA, 10),
  cluster5 = rep(NA, 10),
  cluster6 = rep(NA, 10),
  cluster7 = rep(NA, 10),
  cluster8 = rep(NA, 10),
  cluster9 = rep(NA, 10),
  cluster10 = rep(NA, 10)
)
  
# Most frequent words
for (i in 1:10) {
  clu <- i
  centroid.i <- k10$centers[clu,]
  k10_freqWords[,i] <- names(sort(centroid.i, decreasing = T)[1:10])
}


# Most distinct words
k10_distWords <- data.frame(
  cluster1 = rep(NA, 10),
  cluster2 = rep(NA, 10),
  cluster3 = rep(NA, 10),
  cluster4 = rep(NA, 10),
  cluster5 = rep(NA, 10),
  cluster6 = rep(NA, 10),
  cluster7 = rep(NA, 10),
  cluster8 = rep(NA, 10),
  cluster9 = rep(NA, 10),
  cluster10 = rep(NA, 10)
)


for (i in 1:10) {
  clu <- i
  centroid.i <- k10$centers[clu,]
  centroid.noti <- apply(k10$centers[-clu,], 2, mean)
  k10_distWords[,i] <- names(sort(centroid.i - centroid.noti, decreasing = T)[1:10])
}

rm(k10, k10_distWords, k10_freqWords) #remove irrelevant variables

# -----------------------

# ANALYSIS ACROSS TIME

# Bin into years
head(emails$DateSent)
emails <-separate(emails, DateSent, c("year", "month") )

# view the distribution of years
table(emails$year)

# split into different dataframes per year (unigrams)
dtm_2008 <- emails[emails$year == "2008", ]
dtm_2009 <- emails[emails$year == "2009", ]
dtm_2010 <- emails[emails$year == "2010", ]
dtm_2011 <- emails[emails$year == "2011", ]
dtm_2012 <- emails[emails$year == "2012", ]
dtm_2014 <- emails[emails$year == "2014", ]

rm(dtm_2008, dtm_2014) # remove bc only 1/2 var(s)

# Recipients Analysis: recipients changing over time
recipients_time <- data.frame("year2009" = rep(NA, 10),
                              "year2010" = rep(NA, 10),
                              "year2011" = rep(NA, 10),
                              "year2012" = rep(NA, 10))

recipients_time[,1] <- names(sort(table(tolower(dtm_2009$Recipient)), decreasing = T)[1:10])
recipients_time[,2] <- names(sort(table(tolower(dtm_2010$Recipient)), decreasing = T)[1:10])
recipients_time[,3] <- names(sort(table(tolower(dtm_2011$Recipient)), decreasing = T)[1:10])
recipients_time[,4] <- names(sort(table(tolower(dtm_2012$Recipient)), decreasing = T)[1:10])

# ------------------
# Sentiment anlaysis: sentiment changing over time

neg <- unlist(read.table("negative-words.txt", stringsAsFactors = F))
pos <- unlist(read.table("positive-words.txt", stringsAsFactors = F))

# isolate only word vars
dtm_2009 = dtm_2009[11:4116]
dtm_2010 = dtm_2010[11:4116]
dtm_2011 = dtm_2011[11:4116]
dtm_2012 = dtm_2012[11:4116]

# sentiment for all docs using for loop
countPos2009 = rep(NA, nrow(dtm_2009))
countNeg2009 = rep(NA, nrow(dtm_2009))

for (i in 1:nrow(dtm_2009)) {
  wordsIdx = which(t(dtm_2009[i,]) > 1)
  wordNames <- colnames(dtm_2009)
  words <- wordNames[wordsIdx] 
  
  countPos2009[i] = sum(as.numeric(words %in% pos))
  countNeg2009[i] = sum(as.numeric(words %in% neg))
}

# function that outputs positive word counts for each email in dat
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

# overall ratio of sentiment in given year
overall2009sentiment = calcSentiment(dtm_2009)
overall2010sentiment = calcSentiment(dtm_2010)
overall2011sentiment = calcSentiment(dtm_2011)
overall2012sentiment = calcSentiment(dtm_2012)

#save(overall2009sentiment, overall2010sentiment, overall2011sentiment, overall2012sentiment, file = "sentiment.Rdata")
load(sentiment.Rdata)

# -----------------
# Cluster Analysis: Topics Changing Over Time

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

# run on all years
dw2009 <- clusterDistinct(10, dtm_2009)
dw2010 <- clusterDistinct(10, dtm_2010)
dw2011 <- clusterDistinct(10, dtm_2011)
dw2012 <- clusterDistinct(10, dtm_2012)

# save data bc it takes so long to load
#save(dw2009, dw2010, dw2011, dw2012, file = "topicsOverTime.Rdata")
load("topicsOverTime.Rdata")

# remove files to clear up space
rm(dw2009, dw2010, dw2011, dw2012, fw2009)
rm(dtm_2009, dtm_2010, dtm_2011, dtm_2012, dtm_2009.tmp, test, test2, test3)
rm(overall2009sentiment, overall2010sentiment,  overall2011sentiment, overall2012sentiment)

#-------------------------------

##### HUMA ABEDIN #####

# extract emails sent by huma
huma <- emails[emails$huma > 1,][11:4508]
huma <- na.omit(huma)

# calc info
humaDistinct <- clusterDistinct(10, huma)
humaFreq <- clusterFreq(10, huma)
humaSent <- calcSentiment(huma) #0.667

##### CHERYL MILLS #####

# extract emails sent by cheryl
cheryl <- emails[emails$cheryl > 1,][11:4508]
cheryl <- na.omit(cheryl) 

# calc info
cherylDistinct <- clusterDistinct(10, cheryl)
cherylFreq <- clusterFreq(10, cheryl)
cherylSent <- calcSentiment(cheryl) #0.669

##### JOHN J SULLIVAN #####

sullivan <- emails[emails$sullivanjj > 1 | emails$sullivanjj_state_gov | emails$sullivanjj_state,][11:4508]
sullivan <- na.omit(sullivan)

sullivanDistinct <- clusterDistinct(10, sullivan)
sullivanFreq <- clusterFreq(10, sullivan)
sullivanSent <- calcSentiment(sullivan) #0.650277

##### LONA VALMORO #####

lona <- emails[emails$valmoro > 1 | emails$valmoro_lona > 1,][11:4508]
lona <- na.omit(lona)

lonaDistinct <- clusterDistinct(5, lona)
lonaSent <- calcSentiment(lona) #0.7263


# -------------------------------------

# SENTIMENT REGRESSION ANALYSIS
calcSentimentRegression <- function(keyword, dat) {
  countPos = rep(NA, nrow(dat))
  countNeg = rep(NA, nrow(dat))
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

# EXAMPLE
calcSentimentRegression("china", emails)

# -------------------------------------

deleteColumn <- function(keyword, dat){
  newDat <- dat[,-which(names(dat) %in% keyword)]
  return(newDat)
}

newDat <- deleteColumn(c("the", "for"),dtm_norm)



