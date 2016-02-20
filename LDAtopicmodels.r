
library(tm)
library(RTextTools)
library(topicmodels)
library(lasso2)



setwd("/Users/thorosm2002/Dropbox/Rcode/LDA")
Afile<-'holresid.csv'
# load data into a matrix
data <- read.csv(Afile, stringsAsFactors=FALSE)

descriptions <- data$description
descriptions <- gsub("'", "", descriptions)  # remove apostrophes
descriptions <- gsub("[[:punct:]]", " ", descriptions)  # replace punctuation with space
descriptions <- gsub("[[:cntrl:]]", " ", descriptions)  # replace control characters with space
#descriptions <- gsub("^[[:space:]]+", "", descriptions) # remove whitespace at beginning of documents
#descriptions <- gsub("[[:space:]]+$", "", descriptions) # remove whitespace at end of documents
descriptions <- tolower(descriptions)  #




dtm <- create_matrix(as.vector(descriptions), 
                     language="english", removeNumbers=TRUE, stemWords=TRUE,
                     minWordLength = 3, 
                     removePunctuation = TRUE,
                     weighting=weightTf)


myctm <- CTM(dtm, k=10 , method = "VEM")
gr <- build_graph(myctm,lambda = 0.9, and = TRUE)


term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * 
  log2(nDocs(dtm)/col_sums(dtm > 0))
dtm <- dtm[ , term_tfidf >= 0.1]
dtm <- dtm[row_sums(dtm) > 0, ]


lda <- LDA(dtm, 3)




terms(lda,30)

topics(lda)