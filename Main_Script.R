# load libraries
library(stringr)
library(tidyverse)
library(tm)
library(openNLP)
library(ggplot2)
library(RWeka)
library(quanteda)
library(readtext)

rm(list=ls())
gc(full = TRUE)

setwd("~/Imparare_R/Capstone/Capstone_proj")
con <- file("./en_US.twitter.txt", open = "r") 
con2 <- file("./en_US.news.txt",open= "r") 
con3 <- file("./en_US.blogs.txt", open="r") 

twitter <- readLines(con,200000, encoding = "UTF-8", skipNul = TRUE)
news <- readLines(con2, encoding = "UTF-8", skipNul = TRUE)
blogs <- readLines(con3, 100000, encoding = "UTF-8", skipNul = TRUE)

twitter
close(con) #### It's important to close the connection when you are done
close(con2)
close(con3)
rm(con, con2, con3)


corpus <- rbind(blogs,news,twitter)
corpus <- corpus(corpus)
corpus
rm(blogs,news,twitter)
gc()

start.time <- Sys.time()
# TOKENIZE THE TXT
traintoken <- tokens(corpus, what = "word",
                     remove_punct = TRUE, remove_symbols = TRUE,
                     remove_numbers = T,remove_separators = T,remove_url = T)
traintoken[[10]]


# Lowercase everything
traintoken <- tokens_tolower(traintoken)
traintoken[[10]]


# Remove Stopwords
traintoken <- tokens_select(traintoken, stopwords(), selection = "remove")
traintoken[[10]]

# Stemming 
traintoken <- tokens_wordstem(traintoken, language = "english")
traintoken[[10]]


tottime <- Sys.time() - start.time
tottime
gc(full = TRUE)

# n-grams
traintoken_2 <- tokens_ngrams(traintoken, n = 2)
traintoken_3 <- tokens_ngrams(traintoken, n = 3)
traintoken_4 <- tokens_ngrams(traintoken, n = 4)
gc(full=TRUE)

# Document-Feature Matrix (DFM)
traintoken_dfm <- dfm(traintoken)
# do the same thing for every n-gram

# transform tokens in a Document Feature Matrix, then convert 'em in Document Term Matrix (DTM)
tdm_1<- quanteda::as.DocumentTermMatrix(traintoken_dfm)

traintoken_dfm2 <- dfm(traintoken_2)
tdm_2<- quanteda::as.DocumentTermMatrix(traintoken_dfm2)

traintoken_dfm3 <- dfm(traintoken_3)
tdm_3<- quanteda::as.DocumentTermMatrix(traintoken_dfm3)

traintoken_dfm4 <- dfm(traintoken_4)
tdm_4<- quanteda::as.DocumentTermMatrix(traintoken_dfm4)

# and finally transpose the matrix to build a Term Document Matrix (TDM)
a<- t(tdm1gram)
b<- t(tdm2gram)
c<- t(tdm3gram)
d<- t(tdm4gram)

# function to extract n-grams and sort by freq
reqNgram <- function(tdm, freq) {
    tdm.freq <- findFreqTerms(tdm, lowfreq = freq)
    tdm.df <- rowSums(as.matrix(tdm[tdm.freq,]))
    tdm.df <- data.frame(word = names(tdm.df), frequency = tdm.df)
    tdm.df <- tdm.df[order(-tdm.df$frequency),]
    tdm.df
}

#remove sparse terms
a <- removeSparseTerms(a, 0.99)

b <- removeSparseTerms(b, 0.999)

c <- removeSparseTerms(c,0.9999)

d <- removeSparseTerms(d,0.99999)


# count freq of n-grams----
freq1gram <- freqNgram(a, 2)
freq2gram <- freqNgram(b, 2)
freq3gram <- freqNgram(c, 2)
freq4gram <- freqNgram(d, 10)



# split n-grams into individual words----
unigram <- data.frame(rows = rownames(freq1gram), count = freq1gram$frequency)
unigram$rows <- as.character(unigram$rows)
uni.split <- strsplit(unigram$rows, split = " ")
unigram <- transform(unigram, first = sapply(uni.split,"[[",1))
unigram <- data.frame(unigram = unigram$first,
                      freq = unigram$count,
                      stringsAsFactors = FALSE)
write.csv(unigram[unigram$freq>1,], "unigram.csv",
          row.names = FALSE)
unigram <- read.csv("unigram.csv",
                    stringsAsFactors = FALSE)
saveRDS(unigram, "unigram.RData")

bigram <- data.frame(rows = rownames(freq2gram), count = freq2gram$frequency)
bigram$rows <- as.character(bigram$rows)
bi.split <- strsplit(bigram$rows, split = "_")
bigram <- transform(bigram, 
                    first = sapply(bi.split,"[[", 1),
                    second = sapply(bi.split,"[[", 2)
                    )
bigram <- data.frame(unigram = bigram$first,
                     bigram = bigram$second,
                      freq = bigram$count,
                      stringsAsFactors = FALSE)
write.csv(bigram[bigram$freq>1,], "bigram.csv",
          row.names = FALSE)
bigram <- read.csv("bigram.csv",
                    stringsAsFactors = FALSE)
saveRDS(bigram, "bigram.RData")

trigram <- data.frame(rows = rownames(freq3gram), count = freq3gram$frequency)
trigram$rows <- as.character(trigram$rows)
tri.split <- strsplit(trigram$rows, split = "_")
trigram <- transform(trigram, 
                    first = sapply(tri.split,"[[",1),
                    second = sapply(tri.split,"[[",2),
                    third = sapply(tri.split,"[[",3))
trigram <- data.frame(unigram = trigram$first,
                     bigram = trigram$second,
                     trigram = trigram$third,
                     freq = trigram$count,
                     stringsAsFactors = FALSE)
write.csv(trigram[trigram$freq>1,], "trigram.csv",
          row.names = FALSE)
trigram <- read.csv("trigram.csv",
                   stringsAsFactors = FALSE)
saveRDS(trigram, "trigram.RData")

quadgram <- data.frame(rows = rownames(freq4gram), count = freq4gram$frequency)
quadgram$rows <- as.character(quadgram$rows)
quad.split <- strsplit(quadgram$rows, split = "_")
quadgram <- transform(quadgram, 
                     first = sapply(quad.split,"[[",1),
                     second = sapply(quad.split,"[[",2),
                     third = sapply(quad.split,"[[",3),
                     fourth = sapply(quad.split,"[[",4))
quadgram <- data.frame(unigram = quadgram$first,
                      bigram = quadgram$second,
                      trigram = quadgram$third,
                      quadgram = quadgram$fourth,
                      freq = quadgram$count,
                      stringsAsFactors = FALSE)
write.csv(quadgram[quadgram$freq>1,], "quadgram.csv",
          row.names = FALSE)
quadgram <- read.csv("quadgram.csv",
                    stringsAsFactors = FALSE)
saveRDS(quadgram, "quadgram.RData")

