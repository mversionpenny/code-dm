# Margot Selosse :  TD Text Mining 1.

# Ex 1. Premier pas avec la librairie tm ####
#this is something to do when the gutenbergr and tidyr packages are not on the machine :
#install.packages('gutenbergr')
#install.packages('tidyr')

#includes libraries we need
library(dplyr)
library(gutenbergr)
library(tm)

#book_data <- gutenberg_works(title == "Moby Dick")
#gutenberg_id = moby_dick_data$gutenberg_id
#book <- gutenberg_download(gutenberg_id)

#load "Moby Dick"
book <- readLines("http://www.gutenberg.org/cache/epub/2701/pg2701.txt")
#to know how many lines were loaded:
length(book)
#take only the first 5000 lines:
book <- book[1:5000]
#change it to a "corpus" object :
book_corpus <- VCorpus(VectorSource(book))
#inspect first three texts :
inspect(book_corpus[1:3])
#write the first text :
writeLines(as.character(book_corpus[[20]]))
#write the first three texts :
lapply(book_corpus[1:3], as.character)
#use of tm_map to lower case and remove punctuation
book_corpus <- tm_map(book_corpus, content_transformer(tolower))
book_corpus <- tm_map(book_corpus, removePunctuation)

# Ex 2. Du corpus à la matrice de données ####
#1.
tdm <- TermDocumentMatrix(book_corpus)
inspect(tdm[202:205, 1:5])
inspect(tdm[c("whale"), 1:50])

#2. sparsity, non sparse entries --> a lot of zeros!!

#3. 
terms <- inspect(tdm[,20])
which(terms == 1, arr.ind = TRUE)

#4.
findFreqTerms(tdm,50)
# lot of them are "clueless" -> are, your, these
# ocean semantic --> whale, whaling, captain, jonah...

#5.
findAssocs(tdm, "love", 0.4)
findAssocs(tdm, c("love","whale"), 0.1)

# Ex 3. Premières manipulation de la matrice ####

#1. loading tdm as a standard R matrix
std_tdm <- as.matrix(tdm)

#2.
terms <- std_tdm[,20]
which(terms == 1, arr.ind = TRUE)

#3.
rownames(std_tdm)

#4.
sums <- rowSums(std_tdm)
#my way :
sorted <- sort(-sums)
#better way :
sorted<- sort(sums,decreasing = TRUE)

#5.
plot(sorted[1:50], type = 'h') #"fast way" : can be improved
barplot(sorted[1:50], type = 'h', las=2)

# Ex4. Prétraitements supplémentaires
#1.
#removing numbers
book_corpus <- tm_map(book_corpus, removeNumbers)
#mot outil == stopwords??
#display stopwords :
stopwords("english")
#stopwords("fr")
#stopwords("sp")
#stopwords("SMART") #--> more than "english"
book_corpus <- tm_map(book_corpus, removeWords,stopwords("SMART"))

#ex3 redo all:
tdm <- TermDocumentMatrix(book_corpus)
std_tdm <- as.matrix(tdm)
sums <- rowSums(std_tdm)
sorted<- sort(sums,decreasing = TRUE)
barplot(sorted[1:50], type = 'h', las=2)

#2.not sure about this part --> ask teacher
tdm2 <- removeSparseTerms(tdm, 0.99)
std_tdm2 <- as.matrix(tdm2)
length(rownames(std_tdm2))
