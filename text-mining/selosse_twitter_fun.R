# Text-mining class #
# Margot Selosse #
# TwitteR #


# download twitter package
# install.packages("twitteR")
library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
# TODO : change path!
setwd("D:/master-DM/cours/text-mining/")

#### initialization ####

# Authorization Object :
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- " https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
# Handshake Object"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
# Note to the teacher : the csv I created is in the archive, you can directly read it
tweets <- searchTwitter('#jillstein', n=1000)
tweets.df <- do.call(rbind, lapply(tweets, as.data.frame))
write.csv(tweets.df, "twitter.csv")

# Creating a corpus : 
tweets.text <- read.csv("twitter.csv")
tweets.text <- tweets.text$text
length(tweets.text)
tweets.corpus <- VCorpus(VectorSource(tweets.text))

# I am not sure it is a good thing to get rid of punctuation on tweets
tweets.tdm <- TermDocumentMatrix(tweets.corpus) 


inspect(tweets.tdm[200:300, 1:5])
# get all hastags
idx_hashtags <- which(substr(rownames(tweets.tdm),1,1) == "#")
inspect(tweets.tdm[idx_hashtags,1:10])

# get rid of stop words :
tweets.corpus <- tm_map(tweets.corpus, removeWords, c(stopwords("english"), "&amp;"))
tweets.tdm <- TermDocumentMatrix(tweets.corpus) 
# most frequent terms :
findFreqTerms(tweets.tdm)

tweets.matrix <- as.matrix(tweets.tdm)
rownames(tweets.matrix)[1:5]
tweets.sorted <- sort(rowSums(tweets.matrix), decreasing = TRUE)
barplot(tweets.sorted[1:50], type = 'h', las=2)

# Wordcloud :
wordcloud(names(tweets.sorted[1:40]), tweets.sorted[1:40])

# wordcloud with colors :
colors <- brewer.pal(3,"Set2")
wordcloud(names(tweets.sorted[1:40]), tweets.sorted[1:40], random.color = TRUE, colors=colors)

