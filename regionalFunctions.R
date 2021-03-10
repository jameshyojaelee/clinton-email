# =====================
# CALC NDOCS PER REGION
# =====================
# function that gets number of documents containing asia
# @param: dat - dataframe to be analyzed
# @param: region - vector of keywords for each region

nDocs <- function(dat, region) {
  idx <- which(colnames(dat) %in% region)
  df <- dat[ ,idx]
  df$total <- rowSums(df)
  
  # num documents containing regional keywords
  df$has_region <- df$total > 0
  ndocs <- sum(df$has_region)
  
  return(ndocs)
}

# =========================
# CALC SENTIMENT FOR REGION
# =========================
# function that calculates sentiment of a region
# @param: dataframe
# @param: region - vector of keywords for region
# @returns: ratio of positive to positive & negative words for dataframe

regionSentiment <- function(dat, region) {
  # create regional df
  idx <- which(colnames(dat) %in% region)
  df <- dat[ ,idx]
  df$total <- rowSums(df)
  df$has_region <- df$total > 0
  
  # get index of observations w/ dataframe
  idx <- which(df$has_region == 1)
  df <- dat[idx,]
  sent <- calcSentiment(df) #helper function to calculate sentiment of df
  
  return(sent)
}


keywordSentiment <- function(dat, keyword) {
  # create regional df
  idx <- which(colnames(dat) %in% keyword)
  df <- dat[ ,idx]
  df$total <- rowSums(df)
  df$has_keyword <- df$total > 0
  
  # get index of observations w/ dataframe
  idx <- which(df$has_keyword == 1)
  df <- dat[idx,]
  sent <- calcSentiment(df) #helper function to calculate sentiment of df
  
  return(sent)
}

