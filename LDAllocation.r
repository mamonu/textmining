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

desc <- data$description
desc <- gsub("'", "", desc)  # remove apostrophes
desc <- gsub("[[:punct:]]", " ", desc)  # replace punctuation with space
desc <- gsub("[[:cntrl:]]", " ", desc)  # replace control characters with space
desc <- gsub("[[:digit:]]+", "", desc) # remove numbers

#desc <- gsub("^[[:space:]]+", "", desc) # remove whitespace at beginning of documents
#desc <- gsub("[[:space:]]+$", "", desc) # remove whitespace at end of documents
desc <- tolower(desc)  #



doc.list <- strsplit(desc, "[[:space:]]+")

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
K <- 3
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


# fit <-  slda.em(documents, K, vocab, num.e.iterations, num.m.iterations, alpha,
#         eta, annotations, params, variance, logistic = FALSE, lambda = 10,
#         regularise = FALSE, method = "sLDA", trace = 0L, MaxNWts=3000)


t2 <- Sys.time()
t2 - t1  # about 2 minutes on laptop



theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

lda_description <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)


json <- createJSON(phi = lda_description$phi, 
                   theta = lda_description$theta, 
                   doc.length = lda_description$doc.length, 
                   vocab = lda_description$vocab, 
                   term.frequency = lda_description$term.frequency)


serVis(json, out.dir = 'vis', open.browser = TRUE)







