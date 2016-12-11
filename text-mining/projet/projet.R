#------------------- Margot Selosse -------------------
#--------------- Text-mining : projet -----------------
#------------------ 2 nov. 2016 ----------------------

#### Setting environnement ####

setwd("D:/master-DM/cours/text-mining/projet/")

# includes libraries we need
library(dplyr)
library(NLP)
library(tm)
library(wordcloud)
library(RColorBrewer)

#### Creating corpus / cleaning data / tdm####

book <- readLines(".//les_rois_maudits//txt//[Rois Maudits-1] Le Roi de fer - Druon,Maurice.txt", encoding = "UTF-8")
book <- book[!(is.na(book) | book=="")]

f <-  function(x) return(gsub("\u2019"," ",x))
  
book <- sapply(book,f,USE.NAMES = FALSE)
  
book.corpus <- VCorpus(VectorSource(book))
lapply(book.corpus[1:30], as.character)

# get rid of punctuation
book.corpus.processed <- tm_map(book.corpus, removePunctuation)
# remove numbers
book.corpus.processed <- tm_map(book.corpus.processed, removeNumbers)
# remove stop-words ("le"...)
book.corpus.processed <- tm_map(book.corpus.processed, removeWords,stopwords("french"))

# create tdm
tdm <- TermDocumentMatrix(book.corpus.processed)

# fin frequent terms
findFreqTerms(tdm,50)
tdm.matrix <- as.matrix(tdm)
sums <- rowSums(tdm.matrix)
sorted <- sort(sums,decreasing = TRUE)
barplot(sorted[1:50], type = 'h', las=2)

#### Wordcloud ####
colors <- brewer.pal(4,"Paired")
wordcloud(names(sorted[1:20]), sorted[1:20], colors=colors)
