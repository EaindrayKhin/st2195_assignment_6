
setwd("~/st2195_assignment_6")

library(dplyr)

#######################################
##LOAD AND MERGE DATA
#######################################

#Note that yours path to the data may be different from what are showing here 

#Load fx

fx <- read.csv("fx.csv", skip=6, header=FALSE, na.strings="-")
#missing values stored as '-' #fx <- read.csv("fx.csv", skip=6, header=FALSE)
fx <- fx[1:2] #subset the first 2 coloumns
colnames(fx) <- c("date", "exchange_rate")

str(fx)
nrow(fx)
length(unique(fx$date))
head(fx)

#Load speeches

speeches <- read.csv("speeches.csv", sep = '|', quote = "", encoding="UTF-8")
#speeches <- read.csv("speeches.csv", sep = '|', quote = """)
speeches <- speeches[!is.na(speeches$contents),c('date', 'contents')]

str(speeches)
nrow(speeches)
length(unique(speeches$date)) #there can be >1 speech for the same date
#head(speeches)

#Merge fx and speeches

#To merge correctly with fix data, we need to paste all contents for each date together
speeches <- speeches %>%
  group_by(date) %>%
  summarise(contents=paste(contents,collapse = ""))
str(speeches)
nrow(speeches)

#syntax:left_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
df <- fx %>% left_join(speeches)
#df <- fx %>% left_join(speeches, by="date")
str(df)

#change data types for date and exchange_rate
df$exchange_rate <- as.numeric(df$exchange_rate)
df$date <- as.Date(df$date)
str(df)

#####################################################
## REMOVE ENTRIES WITH OBVIOUS OUTLIERS OR MISTAKES
#####################################################

# check for any obvious outliers or mistaken by plotting the data
plot(df$date, df$exchange_rate, type='1' , xlab="date",
     ylab="EUR/USD reference exchange rate")

# Look at summary statistics
# Observation: No obvious outliers or mistakes, but there is missing data (NA)
summary(df)

# Alternatively, you could sort the dataframe by data in assending order
# Then you can use subsequent functions (e.g., diff, etc.) with default arguments
#df_asc <- df[order(df$date),]
#
#head(df_asc)

#####################################
## HANDLE MISSING DATA
#####################################

# There are serveral ways to fill missing data.

# We can use fill from tidyr package (part of tidyverse)
# Syntax is fill(data, ..., .direction = c("down", "up", "downup", "updown"))
# Use fill direction of "up" as date in descending order and we want prior older value

#install.packages("tidyr")
library(tidyr)
df2 <- df
df2 <- df2 %>% fill(exchange_rate, .direction="up")
summary(df2)

# We can also use the 'na.locf()' from zoo
# Note fromLast should set to TRUE as date is in descending order
# .locf = Last Observation Carried Forward
# fromLast = replacing each NA with the most recent non-NA prior to it 

#install.packages("zoo")
library(zoo)
df3 <- df
df3$exchange_rate <- na.locf(df3$exchange_rate, fromLast = TRUE)
summary(df3)

# Both methods achieve the same result
all.equal(df2, df3)

# Observation: Actually if you look at tye date carefully, you will see some
# "date gaps" in the data as well for this assignment we will not handle them

####################################
## EXCHANGE RATE RETURN
####################################

help(diff)

# Difference between two elements, with default lag=1
diff(df2$exchange_rate)

# As date is in descending order, multiply by -1?
# Depends on perspective? Euro? USD?
diff(df2$exchange_rate)*(-1)

df2$exchange_rate
df2$exchange_rate[-1] #exclude the first element

# Last element set to NA, as there aren't any further prior values for calculation
df2$return <- c( (diff(df2$exchange_rate)*(-1)) / df2$exchange_rate[-1], NA)
#df2$return <- c( (diff(df2$exchange_rate, lag=1)*(-1)) / df2$exchange_rate[-1], NA)

##########################################################
## EXTEND DATASET WITH VARIABLES FOR "GOOD NEWS" AND "BAD NEWS"
##########################################################

df2$good_news <- as.numeric(df2$return > 0.5/100)
df2$bad_news <- as.numeric(df2$return < -0.5/100)

######################################################
## ASSOCIATE WORDS WITH "GOOD NEWS" AND "BAD NEWS"
######################################################

library(tidyr)

# Remove rows with NA
df2 <- df2 %>% drop_na(contents)
str(df2)

# Get the contents related to "good_news" and "bad_news"
good_news_contents <- df2$contents[df2$good_news==1]
bad_news_contents <- df2$contents[df2$bad_news==1]

# Load in stop words, which are those used to form a sentence but does not add much meaning
stop_words <- read.csv("stop_words_english.txt", header = FALSE)[,1]
stop_words

# Function below helps us to get the mostcommon words (excluding stop_words) related to good_news and bad news
# Please read the comments to understand how it works:
install.packages("text2vec")
library(text2vec)
??text2vec

get_word_freq <- function(contents, stop_words, num_words) {
  
  #turn a paragraph to a vector of words
  words <- unlist(lapply(contents, word_tokenizer))
  
  #turn all words to lowercase
  words <- tolower(words)
  
  #find out the number of appearance of each word
  freq <- table(words)
  #length(freq)
  
  #remove the stop words
  freq <- freq[!(names(freq) %in% stop_words)]
  #length(freq)
  
  #sort the words from appearing most to least and return the result
  freq <- sort(freq, decreasing=TRUE)
  return(names(freq)[1:num_words])
  #return (names(freq[order(-freq)])[1:num_words]) #another way to do this
}

# Use the function above to get the 20 most common words associated with 
# good_news and bad_news

good_indicators <-get_word_freq(good_news_contents, stop_words, num_words = 30)
good_indicators

bad_indicators <-get_word_freq(bad_news_contents, stop_words, num_words = 30)
bad_indicators

