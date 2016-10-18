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
book.corpus <- VCorpus(VectorSource(book))

### changing corpus ###
# to lower case
book.corpus.processed <- tm_map(book.corpus, content_transformer(tolower))
# get rid of punctuation
book.corpus.processed <- tm_map(book.corpus.processed, removePunctuation)
# remove numbers
book.corpus.processed <- tm_map(book.corpus.processed, removeNumbers)
# remove stop-words ("the" "are" ...)
book.corpus.processed <- tm_map(book.corpus.processed, removeWords,stopwords("SMART"))
# stemming words (eg evaluation ==> evaluat)
book.corpus.processed <- tm_map(book.corpus.processed, stemDocument)

### creating TermDocumentMatrix ###
tdm <- TermDocumentMatrix(book.corpus.processed, control=list(weighting=weightTfIdf))
#/!\"Warning message:In weighting(x) :empty document(s): 2 7 8 10 ..."--> ask teacher 

#2.
# num of terms in tdm with stemming < without stemming
#wait for class, if not clear, ask teacher number

#3.
findFreqTerms(tdm,20)
# stemming cut words

#4.
printdoc <- function(tdm, doc_vec){
  terms <- inspect(tdm[,doc_vec]);
  indexes <- which(rowSums(terms) > 0)
  result <- inspect(tdm[indexes,doc_vec])
  return(result)
}

#5.
printdoc_raw <- function(original_corpus,doc_vec){
  #unlist does a vector
  unlist(lapply(original_corpus[doc_vec]$content, as.character))
}

# Ex. 6 Mon premier moteur de recherche (formuler une requête)





