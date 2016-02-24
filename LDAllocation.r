library(tm)
library(RTextTools)
library(topicmodels)
library(dplyr)
library(stringi)
library(LDAvis)
library(slam)
library(lda)
library(RJSONIO)
library(ddR)

nInst = 4 # Change level of parallelism
useBackend(parallel,executors = nInst)





#clean up the r environment.
rm(list = ls())
#@home working directory
#setwd("/Users/thorosm2002/Dropbox/Rcode/LDA")


#@work working directory
setwd("/home/bigdata/LDA/textmining/")

#Afile<-'holresid.csv'
Afile<-'caravanelse.csv'
# load data into a matrix
data <- read.csv(Afile, stringsAsFactors=FALSE)

desc <- data$description



desc <- gsub("'", "", desc)  # remove apostrophes
desc <- gsub("[[:punct:]]", "", desc)  # replace punctuation with space
desc <- gsub("[[:cntrl:]]", "", desc)  # replace control characters with space
desc <- gsub("[[:digit:]]+", "", desc) # remove numbers

desc <- gsub("^[[:space:]]+", "", desc) # remove whitespace at beginning of documents
desc <- gsub("[[:space:]]+$", "", desc) # remove whitespace at end of documents
desc <- tolower(desc)  #




t1 <- Sys.time()
doc.list <- strsplit(desc, "[[:space:]]+")
stem.list <- dlapply (doc.list, wordStem )
stem.list <- collect (stem.list)
t2 <- Sys.time()
t2 - t1  


doc.list <- stem.list
# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
stop_words <- stopwords("SMART")
del <- names(term.table) %in% stop_words | term.table < 5 

term.table <- term.table[!del]


####STEMMING GOES HERE


##########################



vocab <-  (names(term.table))






get.terms <- function(x,vocab) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

###### simple laplly  

# t1 <- Sys.time()
# documents <- lapply(doc.list, get.terms,vocab)
# t2 <- Sys.time()
# t2 - t1  

###### parallel laplly instead

t1 <- Sys.time()
documents <- dlapply(doc.list, get.terms,vocab)
documents <-collect (documents)
t2 <- Sys.time()
t2 - t1  

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents  
W <- length(vocab)  # number of terms in the vocab  
doc.length <- sapply(documents, function(x) sum(x[2, ])) 
N <- sum(doc.length)  
term.frequency <- as.integer(term.table)


# MCMC and model tuning parameters:
K <- 10
G <- 9000
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
t2 - t1  


##################################################################################################
#CVB0 produces a matrix of topic counts for
#each document, N td and counts of topics assigned to the different words, N wt . These count
#matrices are converted to conditional probabilities of interest, the distributions of words
#given topics, p(w|t), and the distributions of topics given documents, p(t|d).


# 
# fitcvb0 <-lda.cvb0(documents = documents, K = K, vocab = vocab, 
#                   num.iterations = G, alpha = alpha, 
#                   eta = eta, trace = 0L)


########################################################################################################3
# fit <-  slda.em(documents, K, vocab, num.e.iterations, num.m.iterations, alpha,
#         eta, annotations, params, variance, logistic = FALSE, lambda = 10,
#         regularise = FALSE, method = "sLDA", trace = 0L, MaxNWts=3000)

#fit <- mmsb.collapsed.gibbs.sampler(network, K, num.iterations, alpha,
#                            beta.prior, initial = NULL, burnin = NULL, trace = 0L)






# about 2 minutes on laptop
# 1.5 hours for the big file.

#save fit object (a list) into rdata so we use at another time without doing all the calcs again!!
#save(fit,file="12clusterfit.RData")


#to access a vocab word : lda_description$vocab[1233] 
#to access a vocab word frequency : lda_description$term.frequency[1233]



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



#export the json file! 
write(json, "exported.json")

serVis(json, out.dir = 'vis2', open.browser = TRUE)







