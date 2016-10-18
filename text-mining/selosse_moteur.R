# Margot Selosse . text-mining : TP 1

# Ex. 5 Mon premier moteur de recherche (préparation) ####

#1.
#install.packages("SnowballC")
library(dplyr)
library(gutenbergr)
library(tm)
library(SnowballC)

#load "Moby Dick"
#book <- readLines("http://www.gutenberg.org/cache/epub/2701/pg2701.txt")
book <- book[!(is.na(book) | book=="")]
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
book.corpus.processed <- tm_map(book.corpus.processed, removeWords,stopwords("english"))
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
# ex : printdoc_raw(book.corpus, c(63,23,96))
printdoc_raw <- function(original_corpus,doc_vec){
  #unlist does a vector
  unlist(lapply(original_corpus[doc_vec]$content, as.character))
}

# Ex. 6 Mon premier moteur de recherche (formuler une requête) ####

#1.
cosine <- function(vec1,vec2){
  #we compute the scalar product of the two vectors
  scalar <- sum(vec1 * vec2)
  # then the product of the norms
  norm_product <- norm(vec1, type="2") * norm(vec2, type="2")
  if(norm_product == 0){
    return(0)
  }
  else{
    return(scalar/norm_product)
  }
}

#2.
m <- as.matrix(tdm)
# build a vector from the m matrix and the words q (given by user)
query2vector <- function(m,q){
  #initialisation
  result <- integer(dim(m)[1])
  # I think there is a vectorized way to do that : 
  for (qtext in q) {
    index <- which(rownames(m) == qtext)
    if(length(index)>0){
      result[index] = 1
    }
  }
  return(result)
}

#3.
# returns a sorted list of documents regarding their cosinus similarity ith the qury
# ex : test <- run_query(m,c("whale","captain"))
run_query <- function(m, query){
  query_vector <- query2vector(m, query)
  cosinus <- numeric(dim(m)[2])
  for (i in seq_along(1:dim(m)[2])) {
    cosinus[i] <- cosine(query_vector,m[,i])
  }
  cosinus <- sort(cosinus, decreasing = TRUE, index.return =TRUE)$ix
  return(cosinus)
}


#4.
words <- c("whale","captain") # can be whatever we want!
test <- run_query(m,words)
book[test[1:10]]


