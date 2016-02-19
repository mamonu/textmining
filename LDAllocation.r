library(tm)
library(RTextTools)
library(topicmodels)
library(dplyr)
library(stringi)
library(LDAvis)
library(slam)
library(lda)



setwd("/Users/thorosm2002/Dropbox/Rcode/LDA")
Afile<-'holresid.csv'
# load data into a matrix
data <- read.csv(Afile, stringsAsFactors=FALSE)

reviews <- data$description
reviews <- gsub("'", "", reviews)  # remove apostrophes
reviews <- gsub("[[:punct:]]", " ", reviews)  # replace punctuation with space
reviews <- gsub("[[:cntrl:]]", " ", reviews)  # replace control characters with space
#reviews <- gsub("^[[:space:]]+", "", reviews) # remove whitespace at beginning of documents
#reviews <- gsub("[[:space:]]+$", "", reviews) # remove whitespace at end of documents
reviews <- tolower(reviews)  #



doc.list <- strsplit(reviews, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
stop_words <- stopwords("SMART")
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)


get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents  
W <- length(vocab)  # number of terms in the vocab  
doc.length <- sapply(documents, function(x) sum(x[2, ])) 
N <- sum(doc.length)  
term.frequency <- as.integer(term.table)


# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:

set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop




# dtm <- create_matrix(as.vector(reviews), 
#                         language="english", removeNumbers=TRUE, stemWords=TRUE,
#                         minWordLength = 3, 
#                         removePunctuation = TRUE,
#                         weighting=weightTf)
# 
# term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * 
#   log2(nDocs(dtm)/col_sums(dtm > 0))
# dtm <- dtm[ , term_tfidf >= 0.1]
# dtm <- dtm[row_sums(dtm) > 0, ]
# 
# 
# lda <- LDA(dtm, 3)
# 
# terms(lda,30)
# 
# topics(lda)






