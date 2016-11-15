# Text-mining class #
# Margot Selosse #
# TwitteR #
# mclustDA #

# download twitter package
# install.packages("twitteR")
library(twitteR)

#### initialization ####

# Authorization Object :
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- " https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumer_key <- "h2r9l0MiW2FQdjvR19pht9dkM"
consumer_secret <- "iZhcxIe9hDAIImdBWtly6SGRVYoEyZDz7pP5wYAYRVvyqcgqN5"
access_token <- "795940717184962561-NASkS6kNSpOGjcZ4qn9mXPH83SPuttG"
access_secret <- "fCOEDK6RFNpqRdTCvYQ4eFK0pQd5xuZ7R8X95fnRWWnLk"
# Handshake Object"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweets <- searchTwitter('#jillstein', n=10000)
tweets.df <- do.call(rbind, lapply(tweets, as.data.frame))
write.csv(tweets.df, "D:/test.csv")


