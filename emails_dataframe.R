# Cleaning DTM of Irrelevant Words
# Name: Maya Lu
# Date: March 8, 2021

# --------------------------------

# 1) get vector of all words in the DTM
emails <- read.csv("Clinton.csv")
words <- colnames(emails)

# ----------------------------------

# 2) find index's of irrelevant words

# beginning with X (e.g. X2004)
x <- words[grepl("^[xX]", words)]
xIdx <- which(words %in% x) 

# begiining with "sent"
sent <- words[grepl("^sent", words)]
sentIdx <- which(words %in% sent)

# stopwords
stopIdx <- which(words %in% stopwords(language = "en"))

# from
from <- words[grepl("^from", words)]
fromIdx <- which(words %in% from) 

# subject
subject <- words[grepl("subject", words)]
subjectIdx <- which(words %in% subject) 

# cc
cc <- words[grepl("cc_", words)]
ccIdx <- which(words %in% cc) 

# state
state <- words[grepl("^state_", words)]
stateIdx <- which(words %in% state) 

# release
release <- words[grepl("^release_in", words)]
releaseIdx <- which(words %in% release) 

# message
message <- words[grepl("message", words)]
messageIdx <- which(words %in% message) 

# full
full <- words[grepl("full_", words)]
fullIdx <- which(words %in% full) 

# department
department <- words[grepl("department", words)]
departmentIdx <- which(words %in% department) 

# -----------------------------------

# 3) Subset email
emails_clean <- subset(emails, select = -c(xIdx, sentIdx, stopIdx, 
                                           fromIdx, subjectIdx, ccIdx, stateIdx, 
                                           releaseIdx, messageIdx, fullIdx,
                                           departmentIdx))

# ---------------------------------
# 4) Create year indicator column for emails_clean
emails_clean <-separate(emails_clean, DateSent, c("year", "month") )
dtm_clean <- emails_clean[-c(1:10)]

save(emails_clean, dtm_clean, file = "processedDTM.Rdata")
