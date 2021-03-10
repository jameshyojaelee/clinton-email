# Program: Analysis Over Time

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

# SENTIMENT REGRESSION ANALYSIS

# Topics mentioned in 2009
  # huma 
  # turkey: -0.0009138
  # armenia: 
  # honduras: 
  # obama: 0.006449
  # guinea: 
  # africa: 
  # china: -0.001039  
  # rights: 0.026604 

obamaSent <- calcSentimentRegression("obama", dtm_2009)
chinaSent <- calcSentimentRegression("china", dtm_2009)
turkeySent <- calcSentimentRegression("turkey", dtm_2009)
rightsSent <- calcSentimentRegression("rights", dtm_2009)
humaSent <- calcSentimentRegression("huma", dtm_2009)

