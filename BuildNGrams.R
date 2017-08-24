library(tm)
library(dplyr)
library(stringi)
library(stringr)
library(knitr)
library(scales)
library(wordcloud)
library(RWeka)
library(data.table)
library(ggplot2)
library(ggthemes)
library(caret)
library(kernlab)

setwd("~/Data Science Course/Capstone Project")

contwitter <- file("./en_US/en_US.twitter.txt", "r")
twitter <- readLines(contwitter, encoding = "UTF-8", skipNul = TRUE)
close(contwitter)

conblogs <- file("./en_US/en_US.blogs.txt", "r")
blogs <- readLines(conblogs, encoding = "UTF-8", skipNul = TRUE)
close(conblogs)

connews <- file("./en_US/en_US.news.txt", "r")
news <- readLines(connews, encoding = "UTF-8", skipNul = TRUE)
close(connews)

## Sample and Cleaning of the Data
set.seed(10)
combined <- c(twitter, blogs, news)
sam <- sample(combined, length(combined) * 0.5) %>%
        iconv("latin1", "ASCII", sub = "")

# rm(contwitter, twitter, conblogs, blogs, connews, news, combined)             # Cleanup

# Divide data into small chunks
lensam <- length(sam)
size <- 10000
nDiv <- as.integer(lensam/size)

# Empty data frames
wf1 <- data.frame(phrase=NA, freq=NA)[numeric(0), ]
wf2 <- data.frame(phrase=NA, freq=NA)[numeric(0), ]
wf3 <- data.frame(phrase=NA, freq=NA)[numeric(0), ]
wf4 <- data.frame(phrase=NA, freq=NA)[numeric(0), ]

unigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
quadgram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

system.time({
        for (i in 1:nDiv ) {
                j <- (i - 1) * size + 1
                k <- i * size
                subsam <- sam[j:k]
                
                # Create a Corpus from the data subsam
                corpus <- VectorSource(subsam) %>%
                        VCorpus()
                
                # Clean and manage the data 
                corpus_clean <- tm_map(corpus, tolower) %>%                             # All letters to lower case
                        tm_map(removePunctuation) %>%                                   # Remove punctuation
                        tm_map(removeNumbers) %>%                                       # Remove numbers
                        tm_map(stripWhitespace) %>%                                     # Remove unnecessary white space
                        tm_map(PlainTextDocument)                                       # Preprocessed text converted to plain text
                
                unigram_matrix <- TermDocumentMatrix(corpus_clean, control = list(tokenize = unigram_tokenizer))
                bigram_matrix <- TermDocumentMatrix(corpus_clean, control = list(tokenize = bigram_tokenizer))
                trigram_matrix <- TermDocumentMatrix(corpus_clean, control = list(tokenize = trigram_tokenizer))
                quadgram_matrix <- TermDocumentMatrix(corpus_clean, control = list(tokenize = quadgram_tokenizer))
                
                unigram_corpus <- findFreqTerms(unigram_matrix,lowfreq = 5)
                bigram_corpus <- findFreqTerms(bigram_matrix,lowfreq = 2)
                trigram_corpus <- findFreqTerms(trigram_matrix,lowfreq = 2)
                quadgram_corpus <- findFreqTerms(quadgram_matrix,lowfreq = 2)
                
                freq1 <- sort(rowSums(as.matrix(unigram_matrix[unigram_corpus, ])), decreasing=TRUE)
                freq2 <- sort(rowSums(as.matrix(bigram_matrix[bigram_corpus, ])), decreasing=TRUE)
                freq3 <- sort(rowSums(as.matrix(trigram_matrix[trigram_corpus, ])), decreasing=TRUE)
                freq4 <- sort(rowSums(as.matrix(quadgram_matrix[quadgram_corpus, ])), decreasing=TRUE)
                
                wf1 <- rbind(wf1, data.table(phrase=names(freq1), freq=freq1))
                wf2 <- rbind(wf2, data.table(phrase=names(freq2), freq=freq2))
                wf3 <- rbind(wf3, data.table(phrase=names(freq3), freq=freq3))
                wf4 <- rbind(wf4, data.table(phrase=names(freq4), freq=freq4))
                
                print(percent(round(i / nDiv, digits = 4)))
        }
})

 rm(i, j, k, subsam, corpus, corpus_clean)                                     # Cleanup
 rm(unigram_matrix, bigram_matrix, trigram_matrix, quadgram_matrix)            # Cleanup
 rm(unigram_corpus, bigram_corpus, trigram_corpus, quadgram_corpus)            # Cleanup
 rm(freq1, freq2, freq3, freq4)                                                # Cleanup

# setwd("~/Data Science Course/Capstone Project")
save(wf1, file = "wf1.RData")
save(wf2, file = "wf2.RData")
save(wf3, file = "wf3.RData")
save(wf4, file = "wf4.RData")

unigram <- wf1[, ':='(Freq = sum(freq)), by = .(phrase)] %>%
        distinct(phrase, Freq)
bigram <- wf2[, ':='(Freq = sum(freq)), by = .(phrase)] %>%
        distinct(phrase, Freq)
trigram <- wf3[, ':='(Freq = sum(freq)), by = .(phrase)] %>%
        distinct(phrase, Freq)
quadgram <- wf4[, ':='(Freq = sum(freq)), by = .(phrase)] %>%
        distinct(phrase, Freq)

 rm(wf1, wf2, wf3, wf4)                                                        # Cleanup

bigram <- bigram[,':='(FirstTerms = word(phrase, start = 1, end = 1))][
        , ':='(LastTerm = word(phrase, -1))]
trigram <- trigram[,':='(FirstTerms = word(phrase, start = 1, end = 2))][
        , ':='(LastTerm = word(phrase, -1))]
quadgram <- quadgram[,':='(FirstTerms = word(phrase, start = 1, end = 3))][
        , ':='(LastTerm = word(phrase, -1))]

save(unigram, file = "unigram.RData")
save(bigram, file = "bigram 4 cols.RData")
save(trigram, file = "trigram 4 cols.RData")
save(quadgram, file = "quadgram 4 cols.RData")

# rm(trigram, quadgram)

system.time({
        for (i in 1:nrow(bigram)){
                if (bigram$FirstTerms[i] %in% unigram$phrase){
                        bigram$FirstTermsCount[i] <- unigram$Freq[bigram$FirstTerms[i] == unigram$phrase]
                } else {
                        bigram$FirstTermsCount[i] <- sum(bigram$Freq[bigram$FirstTerms == bigram$FirstTerms[i]])
                }
                print(percent(round(i / nrow(bigram), digits = 4)))        
        }
        
        bigram$Probability <- bigram$Freq / bigram$FirstTermsCount
})

# setwd("~/Data Science Course/Capstone Project")
# load("trigram 4 cols.RData")

system.time({
        for (i in 1:nrow(trigram)){
                if (trigram$FirstTerms[i] %in% bigram$phrase){
                        trigram$FirstTermsCount[i] <- bigram$Freq[trigram$FirstTerms[i] == bigram$phrase]
                } else {
                        trigram$FirstTermsCount[i] <- sum(trigram$Freq[trigram$FirstTerms == trigram$FirstTerms[i]])
                }
                print(percent(round(i / nrow(trigram), digits = 4)))
        }
        
        trigram$Probability <- trigram$Freq / trigram$FirstTermsCount
})

# setwd("~/Data Science Course/Capstone Project")
# save(bigram, file = "bigram.RData")
# rm(bigram)
# load("quadgram 4 cols.RData")

system.time({
        for (i in 1:nrow(quadgram)){
                if (quadgram$FirstTerms[i] %in% trigram$phrase){
                        quadgram$FirstTermsCount[i] <- trigram$Freq[quadgram$FirstTerms[i] == trigram$phrase]
                } else {
                        quadgram$FirstTermsCount[i] <- sum(quadgram$Freq[quadgram$FirstTerms == quadgram$FirstTerms[i]])
                }
                print(percent(round(i / nrow(quadgram), digits = 4)))        
        }
        
        quadgram$Probability <- quadgram$Freq / quadgram$FirstTermsCount
})

# setwd("~/Data Science Course/Capstone Project")

# save(unigram, file = "unigram.RData")
# save(bigram, file = "bigram.RData")
save(trigram, file = "trigram.RData")
save(quadgram, file = "quadgram.RData")
