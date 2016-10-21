# Margot Selosse :  TD Text Mining 1.

# Ex 1. Premier pas avec la librairie tm ####
#this is something to do when the gutenbergr and tidyr packages are not on the machine :
#install.packages('gutenbergr')
#install.packages('tidyr')

#includes libraries we need
library(dplyr)
library(gutenbergr)
library(NLP)
library(tm)

#book_data <- gutenberg_works(title == "Moby Dick")
#gutenberg_id = moby_dick_data$gutenberg_id
#book <- gutenberg_download(gutenberg_id)

#load "Moby Dick" /!\ if warning "incomplete final line" --> go to site an fill captcha
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
which(terms > 0, arr.ind = TRUE)

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

# Ex4. Prétraitements supplémentaires ####
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
tdm2 <- removeSparseTerms(tdm, 0.98)
std_tdm2 <- as.matrix(tdm2)
length(rownames(std_tdm2))

#3. Not sure in this context it is really helping :
# sparsity of tdm2 = 0.98 while sparsity of tdm = 100%

#4.
#install.packages("wordcloud")
#install.packages("RColorBrewer")
library(wordcloud)
library(RColorBrewer)
#getting only 100 most freqent words (at beginning, repeating ex3) :
tdm <- TermDocumentMatrix(book_corpus)
std_tdm <- as.matrix(tdm)
sums <- rowSums(std_tdm)
sorted <-sort(sums, decreasing =TRUE)
wordcloud(names(sorted[1:10]), sorted[1:10])



# meanwhile : 
temp <- inspect(tdm)
indexes <- which(rowSums(temp)>9)
FreqMat <- data.frame(terms = rownames(temp)[indexes], Freq = rowSums(temp)[indexes])
wordcloud(FreqMat$terms,FreqMat$Freq)
#if error : "queequeg could not be fit on page. It will not be plotted." --> increase size of plot
# with colors :
colors <- brewer.pal(4,"Paired")
wordcloud(FreqMat$terms,FreqMat$Freq, random.color = TRUE, colors=colors)


