# Program: Analysis Over Time
# Author: Maya Lu
# Date: March 9, 2021

# --------------------
# import datasets
# load("processedDTM.RData")

# --------------------
# K-CLUSTERS OVER TIME

# create dataframes for each respective years - UNIGRAMS
dtm_2009 <- emails_clean[emails_clean$year == "2009", ][11:2786]
dtm_2010 <- emails_clean[emails_clean$year == "2010", ][11:2786]
dtm_2011 <- emails_clean[emails_clean$year == "2011", ][11:2786]
dtm_2012 <- emails_clean[emails_clean$year == "2012", ][11:2786]

# import distinct words function from functions.R
# find distinct words from 2009 to 2012
dw2009 <- clusterDistinct(10, dtm_2009)
fw2009 <- clusterFreq(10, dtm_2009)

dw2010 <- clusterDistinct(10, dtm_2010)
fw2009 <- clusterFreq(10, dtm_2009)

dw2011 <- clusterDistinct(10, dtm_2011)
fw2011 <- clusterFreq(10, dtm_2011)

dw2012 <- clusterDistinct(10, dtm_2012)
fw2012 <- clusterDistinct(10, dtm_2012)

save(dw2009, dw2010, dw2011, dw2012, file = "topicsOverTime.Rdata")

# ---------------------
# SENTIMENT OVER TIME
overall2009sentiment = calcSentiment(dtm_2009)
overall2010sentiment = calcSentiment(dtm_2010)
overall2011sentiment = calcSentiment(dtm_2011)
overall2012sentiment = calcSentiment(dtm_2012)
overallSents <- c(overall2009sentiment,overall2010sentiment,overall2011sentiment,overall2012sentiment )

# ------------

# REGIONAL ANALYSIS OVER TIME

asia <- c("china", "southeast", "asia", "korea", "japan", "singapore", "pakistan", "india", "indonesia")
middle_east <- c("syria", "gaza", "qatar", "turkey", "egypt", "jordan", "palestine", "saudi", "yemen", "israel", "iran", "iraq")
africa <- c("africa" , "kenya", "congo", "morocco", "uganda", "libya", "guinea", "sudan")
la_carib <- c("latin","caribbean", "brazil", "colombia", "venezuela", "cuba", "mexico", "haiti", "honduras")
europe <- c("germany", "greece", "ireland", "russia","uk","armenia",  "poland", "spain", "italy")

# ---------------
# NUMBER DOCUMENTS 
# ASIA
nAsiaDoc <- nDocs(dtm_clean, asia)

# MIDDLE EAST
nMeDoc <- nDocs(dtm_clean, me)

# AFRICA
nAfricaDoc <- nDocs(dtm_clean, africa) # 592
 
# LATIN AMERICA & CARIBBEAN
nLaCaribDoc <- nDocs(dtm_clean, la_carib) # 854

# EUROPE
nEuropeDoc <- nDocs(dtm_clean, europe) # 690

# -----------------------------
# VISUALIZE FREQUENCY
nRegionDocs <- c(nAsiaDoc, nMeDoc, nAfricaDoc, nLaCaribDoc, nEuropeDoc)
barplot(nRegionDocs, 
        names.arg = c("Asia", "Middle East", "Africa", "Latin America/Carib", "Europe"),
        main = "Number of Documents Mentioning Each Region")

# ------------------------------
# VISUALIZE SENTIMENT

# ASIA
asiaIdx <- which(asiaDat$has_asia == 1)
asiaDF <- dtm_clean[asiaIdx,]
asiaSent <- calcSentiment(asiaDF)

# MIDDLE EAST
meIdx <- which(meDat$has_me == 1)
meDF <- dtm_clean[meIdx,]
meSent <- calcSentiment(meDF)

# AFRICA
africaIdx <- which(africaDat$has_africa == 1)
africaDF <- dtm_clean[africaIdx,]
africaSent <- calcSentiment(africaDF)

# LATIN AMERICA & CARIBBEAN
lacaribIdx <- which(lacaribDat$has_lacarib == 1)
lacaribDF <- dtm_clean[lacaribIdx,]
lacaribSent <- calcSentiment(lacaribDF)

# EUROPE
europeIdx <- which(europeDat$has_europe == 1)
europeDF <- dtm_clean[europeIdx,]
europeSent <- calcSentiment(europeDF)

# ------------------------------------

# VISUALIZE SENTIMENT ACROSS REGIONS
regionSent <- c(asiaSent, meSent, africaSent, lacaribSent, europeSent)
barplot(regionSent, 
        names.arg = c("Asia", "Middle East", "Africa", "Latin America/Carib", "Europe"),
        main = "Sentiment Ratio (Pos : Pos & Neg) Across Documents that Mention Region")

# ----------------------------------
# VISUALIZE PROPORTION OF DOCUMENTS FOR EACH REGION ACROSS TIME
par(mfrow = c(3, 2))

# ASIA
asiaDocs2009 <- nDocs(dtm_2009, asia)/nrow(dtm_2009)
asiaDocs2010 <- nDocs(dtm_2010, asia)/nrow(dtm_2010)
asiaDocs2011 <- nDocs(dtm_2011, asia)/nrow(dtm_2011)
asiaDocs2012 <- nDocs(dtm_2012, asia)/nrow(dtm_2012)
asiaDocs <- c(asiaDocs2009, asiaDocs2010, asiaDocs2011, asiaDocs2012)
barplot(asiaDocs,
        names.arg = c("2009", "2010", "2011", "2012"), 
        main = "Proportion of Documents Pertaining to Asia")

# MIDDLE EAST
meDocs2009 <- nDocs(dtm_2009, middle_east)/nrow(dtm_2009)
meDocs2010 <- nDocs(dtm_2010, middle_east)/nrow(dtm_2010)
meDocs2011 <- nDocs(dtm_2011, middle_east)/nrow(dtm_2011)
meDocs2012 <- nDocs(dtm_2012, middle_east)/nrow(dtm_2012)
meDocs <- c(meDocs2009, meDocs2010, meDocs2011, meDocs2012)
barplot(meDocs,
        names.arg = c("2009", "2010", "2011", "2012"), 
        main = "Proportion of Documents Pertaining to Middle East")

# AFRICA
africaDocs2009 <- nDocs(dtm_2009, africa)/nrow(dtm_2009)
africaDocs2010 <- nDocs(dtm_2010, africa)/nrow(dtm_2010)
africaDocs2011 <- nDocs(dtm_2011, africa)/nrow(dtm_2011)
africaDocs2012 <- nDocs(dtm_2012, africa)/nrow(dtm_2012)
africaDocs <- c(africaDocs2009, africaDocs2010, africaDocs2011, africaDocs2012)
barplot(africaDocs,
        names.arg = c("2009", "2010", "2011", "2012"), 
        main = "Proportion of Documents Pertaining to Africa")

# LATIN AMERICA & CARIBBEAN
laCaribDocs2009 <- nDocs(dtm_2009, la_carib)/nrow(dtm_2009)
laCaribDocs2010 <- nDocs(dtm_2010, la_carib)/nrow(dtm_2010)
laCaribDocs2011 <- nDocs(dtm_2011, la_carib)/nrow(dtm_2011)
laCaribDocs2012 <- nDocs(dtm_2012, la_carib)/nrow(dtm_2012)
laCaribDocs <-c(laCaribDocs2009, laCaribDocs2010, laCaribDocs2011, laCaribDocs2012)
barplot(laCaribDocs,
        names.arg = c("2009", "2010", "2011", "2012"), 
        main = "Proportion of Documents Pertaining to Latin America/Carib")

# EUROPE
europeDocs2009 <- nDocs(dtm_2009, europe)/nrow(dtm_2009)
europeDocs2010 <- nDocs(dtm_2010, europe)/nrow(dtm_2010)
europeDocs2011 <- nDocs(dtm_2011, europe)/nrow(dtm_2011)
europeDocs2012 <- nDocs(dtm_2012, europe)/nrow(dtm_2012)
europeDocs <- c(europeDocs2009, europeDocs2010, europeDocs2011, europeDocs2012)
barplot(europeDocs,
        names.arg = c("2009", "2010", "2011", "2012"), 
        main = "Proportion of Documents Pertaining to Europe")

# --------------------------

# REGIONAL SENTIMENT OVER TIME

# ASIA
asiaSent2009 <- regionSentiment(dtm_2009, asia)
asiaSent2010 <- regionSentiment(dtm_2010, asia)
asiaSent2011 <- regionSentiment(dtm_2011, asia)
asiaSent2012 <- regionSentiment(dtm_2012, asia)
asiaSents <- c(asiaSent2009, asiaSent2010, asiaSent2011, asiaSent2012)

# MIDDLE EAST
meSent2009 <- regionSentiment(dtm_2009, middle_east)
meSent2010 <- regionSentiment(dtm_2010, middle_east)
meSent2011 <- regionSentiment(dtm_2011, middle_east)
meSent2012 <- regionSentiment(dtm_2012, middle_east)
meSents <- c(meSent2009, meSent2010, meSent2011, meSent2012 )

# AFRICA
africaSent2009 <- regionSentiment(dtm_2009, africa)
africaSent2010 <- regionSentiment(dtm_2010, africa)
africaSent2011 <- regionSentiment(dtm_2011, africa)
africaSent2012 <- regionSentiment(dtm_2012, africa)
africaSents <- c(africaSent2009, africaSent2010, africaSent2011, africaSent2012)

# LATIN AMERICA & CARIBBEAN
laCaribSent2009 <- regionSentiment(dtm_2009, la_carib)
laCaribSent2010 <- regionSentiment(dtm_2010, la_carib)
laCaribSent2011 <- regionSentiment(dtm_2011, la_carib)
laCaribSent2011 <- 0.5
laCaribSent2012 <- regionSentiment(dtm_2012, la_carib)
laCaribSents <- c(laCaribSent2009, laCaribSent2010, laCaribSent2011, laCaribSent2012 )

# EUROPE
europeSent2009 <- regionSentiment(dtm_2009, europe)
europeSent2010 <- regionSentiment(dtm_2010, europe)
europeSent2011 <- regionSentiment(dtm_2011, europe)
europeSent2012 <- regionSentiment(dtm_2012, europe)
europeSents <- c(europeSent2009, europeSent2010, europeSent2011, europeSent2012)

# --------------------

# VISUALIZE SENTIMENT OVER TIME

par(mfrow = c(1,1))
library(ggplot2)
regionalSents <- data.frame(asiaSents, meSents, africaSents, laCaribSents, europeSents, overallSents)

ggplot(dat = regionalSents, aes( x = c(1,2,3,4), y = asiaSents, col = "asia")) + 
  geom_line() +
  geom_line(aes(y = meSents, col = "middle east")) + 
  geom_line(aes(y = africaSents, col = "africa")) +
  geom_line(aes(y = laCaribSents, col = "latin america/caribbean")) +
  geom_line(aes(y = europeSents, col = "europe")) +
  geom_line(aes(y = overallSents, col = "overall"), linetype = "dashed", col = "gray") +
  xlab("year (2009-2012)") +
  ylab("Positive Sentiment Ratio") +
  ggtitle("Sentiment Towards Regions Across Years")


# ----------------------










