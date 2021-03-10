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



# --------------------------------------
# 5) subset by quarter
q1 <- emails_clean[which(emails_clean$year == "2009" & (emails_clean$month == "01" | emails_clean$month == "02" | emails_clean$month == "03" )), ][11:2786]
q2 <- emails_clean[which(emails_clean$year == "2009" & (emails_clean$month == "04" | emails_clean$month == "05" | emails_clean$month == "06" )), ][11:2786]
q3 <- emails_clean[which(emails_clean$year == "2009" & (emails_clean$month == "07" | emails_clean$month == "08" | emails_clean$month == "09" )), ][11:2786]
q4 <- emails_clean[which(emails_clean$year == "2009" & (emails_clean$month == "10" | emails_clean$month == "11" | emails_clean$month == "12" )), ][11:2786]

q5 <- emails_clean[which(emails_clean$year == "2010" & (emails_clean$month == "01" | emails_clean$month == "02" | emails_clean$month == "03" )), ][11:2786]
q6 <- emails_clean[which(emails_clean$year == "2010" & (emails_clean$month == "04" | emails_clean$month == "05" | emails_clean$month == "06" )), ][11:2786]
q7 <- emails_clean[which(emails_clean$year == "2010" & (emails_clean$month == "07" | emails_clean$month == "08" | emails_clean$month == "09" )), ][11:2786]
q8 <- emails_clean[which(emails_clean$year == "2010" & (emails_clean$month == "10" | emails_clean$month == "11" | emails_clean$month == "12" )), ][11:2786]

q9 <- emails_clean[which(emails_clean$year == "2011" & (emails_clean$month == "01" | emails_clean$month == "02" | emails_clean$month == "03" )), ][11:2786]
q10 <- emails_clean[which(emails_clean$year == "2011" & (emails_clean$month == "04" | emails_clean$month == "05" | emails_clean$month == "06" )), ][11:2786]
q11 <- emails_clean[which(emails_clean$year == "2011" & (emails_clean$month == "07" | emails_clean$month == "08" | emails_clean$month == "09" )), ][11:2786]
q12 <- emails_clean[which(emails_clean$year == "2011" & (emails_clean$month == "10" | emails_clean$month == "11" | emails_clean$month == "12" )), ][11:2786]

q13 <- emails_clean[which(emails_clean$year == "2012" & (emails_clean$month == "01" | emails_clean$month == "02" | emails_clean$month == "03" )), ][11:2786]
q14 <- emails_clean[which(emails_clean$year == "2012" & (emails_clean$month == "04" | emails_clean$month == "05" | emails_clean$month == "06" )), ][11:2786]
q15 <- emails_clean[which(emails_clean$year == "2012" & (emails_clean$month == "07" | emails_clean$month == "08" | emails_clean$month == "09" )), ][11:2786]
q16 <- emails_clean[which(emails_clean$year == "2012" & (emails_clean$month == "10" | emails_clean$month == "11" | emails_clean$month == "12" )), ][11:2786]

#list of emails by quarter
qdf <- list(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14, q15, q16)

save(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14, q15, q16, file= "emails_by_qtr.RData")
