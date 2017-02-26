setwd('C:/Users/pradeepsingh.naulia/OneDrive - Xchanging/DS training plus model lutz')
library(twitteR)
require(devtools)

#Package sentiment has been archived so install it from archive location
install_url("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
library(sentiment)

library(plyr)

library(ggplot2)

#Package slam has been archived so install it from archive location

install_url("https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz")

library(slam)

library(wordcloud)

library(RColorBrewer)

#Set authentication with Twitter. Copy keys generated in step-1

api_key <- "8ynHwJal2dktOzpWMTY4ZFMap"

api_secret <- "oM7M70IvdkHqWGEE4TU26gtzmM6GmrdOCTvkNxX6GQvoMWvuV5"

access_token <- "4343362033-zcKWqTwbtZcWo3v7pK8ZJIfemwaSAzcqiOqidCt"

access_token_secret <- "j6K7tVkP2lJVqq1tlfbWDFhOPKVy98WNO4fvU04nKr48y"

#Step-2: Create R program to ingest twitter data and perform analysis

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
#Ingest tweets from twitter

#Here in this program search based on key word “demonetization” and number of #tweets ingested 1500, it can be increase/decrease by modifying parameter value of n

Demonetization_tweets = searchTwitter("demonetization", n=1500, lang="en")


# filter text from tweets

Demonetization_text = sapply(Demonetization_tweets, function(x) x$getText())

#Prepare/clean data for sentiment analysis

# delete re-tweet entries

Demonetization_text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", Demonetization_text)

# remove @ word

Demonetization_text = gsub("@\\w+", "", Demonetization_text)

# delete punctuation

Demonetization_text = gsub("[[:punct:]]", "", Demonetization_text )

# remove Digits: 0 1 2 3 4 5 6 7 8 9

Demonetization_text = gsub("[[:digit:]]", "", Demonetization_text)

# delete html links

Demonetization_text = gsub("http\\w+", "", Demonetization_text)

# delete unnecessary spaces like tab and space

Demonetization_text = gsub("[ \t]{2,}", "", Demonetization_text)

Demonetization_text = gsub("^\\s+|\\s+$", "", Demonetization_text)
# define error handling function 

try.error = function(x)
  
{
  
  # create missing value
  
  y = NA
  
  # tryCatch error
  
  try_error = tryCatch(tolower(x), error=function(e) e)
  
  # if not an error
  
  if (!inherits(try_error, "error"))
    
    y = tolower(x)
  
  # result
  
  return(y)
  
}

Demonetization_text = sapply(Demonetization_text, try.error)

# remove NAs in Demonetization_text

Demonetization_text = Demonetization_text [!is.na(Demonetization_text)]

names(Demonetization_text) = NULL

# Perform Sentiment Analysis by using naive bayes algorithm. 

#function classify_emotion is defined in “sentiment” package. 

#This function helps us to analyze some text and classify it in different types of #emotion: anger, disgust, #fear, joy, sadness, and surprise

class_emo = classify_emotion(Demonetization_text, algorithm="bayes", prior=1.0)

# get emotion best fit

emotion = class_emo[,7]
# replace NA's by "unknown"

emotion[is.na(emotion)] = "unknown"

# function classify_polarity is defined in “sentiment” package.

# The classify_polarity function allows us to classify some text as positive or negative

class_pol = classify_polarity(Demonetization_text, algorithm="bayes")

# get polarity best fit

polarity = class_pol[,4]

# Create data frame with the results and obtain some general statistics

# data frame with results

sent_df = data.frame(text= Demonetization_text, emotion=emotion,
                     
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame

sent_df = within(sent_df,
                 
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


# plot distribution of emotions

ggplot(sent_df, aes(x=emotion)) +
  
  geom_bar(aes(y=..count.., fill=emotion)) +
  
  scale_fill_brewer(palette="Dark2") +
  
  labs(x="emotion categories", y="number of tweets") +
  
  labs(title = "Sentiment Analysis of Tweets about Demonetization\n(classification by emotion)")
#plot distribution of polarity

ggplot(sent_df, aes(x=polarity)) +
  
  geom_bar(aes(y=..count.., fill=polarity)) +
  
  scale_fill_brewer(palette="Dark2") +
  
  labs(x="polarity categories", y="number of tweets") +
  
  labs(title = "Sentiment Analysis of Tweets about Demonetization\n(classification by polarity)")


#Separate the text by emotions and visualize the words with a comparison cloud

# separating text by emotion

emos = levels(factor(sent_df$emotion))

nemo = length(emos)

emo.docs = rep("", nemo)

for (i in 1:nemo)
  
{
  
  tmp = Demonetization_text[emotion == emos[i]]
  
  emo.docs[i] = paste(tmp, collapse=" ")
  
}



# remove stopwords

emo.docs = removeWords(emo.docs, stopwords("english"))

# create corpus

corpus = Corpus(VectorSource(emo.docs))

tdm = TermDocumentMatrix(corpus)

tdm = as.matrix(tdm)

colnames(tdm) = emos



# comparison word cloud

comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)

