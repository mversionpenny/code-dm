# Margot Selosse . text-mining : TP 1

# Ex. 5 Mon premier moteur de recherche (préparation)

#1.
#install.packages("SnowballC")
library(dplyr)
library(gutenbergr)
library(tm)
library(SnowballC)

#load "Moby Dick"
book <- readLines("http://www.gutenberg.org/cache/epub/2701/pg2701.txt")
#take only the first 5000 lines:
book <- book[1:5000]
#change it to a "corpus" object :
book_corpus <- VCorpus(VectorSource(book))

### changing corpus ###
# to lower case
book_corpus <- tm_map(book_corpus, content_transformer(tolower))
# get rid of punctuation
book_corpus <- tm_map(book_corpus, removePunctuation)
# remove numbers
book_corpus <- tm_map(book_corpus, removeNumbers)
# remove stop-words ("the" "are" ...)
book_corpus <- tm_map(book_corpus, removeWords,stopwords("SMART"))
# stemming words (eg evaluation ==> evaluat)
book_corpus <- tm_map(book_corpus, stemDocument)

### creating TermDocumentMatrix ###
tdm <- TermDocumentMatrix(book_corpus, control=list(weighting=weightTfIdf))
#/!\"Warning message:In weighting(x) :empty document(s): 2 7 8 10 ..."--> ask teacher 

#2.
# num of terms in tdm with stemming < without stemming
#wait for class, if not clear, ask teacher number

#3.
findFreqTerms(tdm,20)
# stemming cut words
