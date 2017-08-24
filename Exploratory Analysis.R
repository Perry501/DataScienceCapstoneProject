library(tm)
library(dplyr)
library(stringi)
library(knitr)
library(scales)
library(wordcloud)
library(RWeka)
library(data.table)
library(ggplot2)
library(ggthemes)

setwd("H:/Data_Science_Course/10 Capstone Project")

contwitter <- file("./en_US/en_US.twitter.txt", "r")
twitter <- readLines(contwitter, encoding = "UTF-8", skipNul = TRUE)
close(contwitter)

conblogs <- file("./en_US/en_US.blogs.txt", "r")
blogs <- readLines(conblogs, encoding = "UTF-8", skipNul = TRUE)
close(conblogs)

connews <- file("./en_US/en_US.news.txt", "r")
news <- readLines(connews, encoding = "UTF-8", skipNul = TRUE)
close(connews)

### Summary of Data
tsize <- round(file.info("./en_US/en_US.twitter.txt")$size / 1024^2, digits = 2)
bsize <- round(file.info("./en_US/en_US.blogs.txt")$size / 1024^2, digits = 2)
nsize <- round(file.info("./en_US/en_US.news.txt")$size / 1024^2, digits = 2)

tlinecount <- length(twitter)
blinecount <- length(blogs)
nlinecount <- length(news)

twordcountperline <- stri_count_words(twitter)
bwordcountperline <- stri_count_words(blogs)
nwordcountperline <- stri_count_words(news)

twordcount <- sum(twordcountperline)
bwordcount <- sum(bwordcountperline)
nwordcount <- sum(nwordcountperline)

tlongestline <- max(twordcountperline)
blongestline <- max(bwordcountperline)
nlongestline <- max(nwordcountperline)

taveragerwords <- round(mean(twordcountperline), digits = 2)
baveragerwords <- round(mean(bwordcountperline), digits = 2)
naveragerwords <- round(mean(nwordcountperline), digits = 2)

ExploratoryDT <- data.frame(File = c("Twitter", "Blogs", "News"),
                            `Size (MB)` = c(tsize, bsize, nsize),
                            `Line Count` = comma(c(tlinecount, blinecount, nlinecount)),
                            `Word Count` = comma(c(twordcount, bwordcount, nwordcount)),
                            `Longest Line` = comma(c(tlongestline, blongestline, nlongestline)),
                            `Mean Words per Line` = c(taveragerwords, baveragerwords, naveragerwords))
# save(ExploratoryDT, file = "ExploratoryDT.RData")
kable(ExploratoryDT)

## Sample and Cleaning of the Data
set.seed(100)
combined <- c(twitter, blogs, news)
sam <- sample(combined, length(combined) * 0.01) %>%
        iconv("latin1", "ASCII", sub = "")

corpus <- VectorSource(sam) %>%
        VCorpus()

corpus_clean <- #iconv(corpus, "latin1", "ASCII", sub = "") %>%                 # Removes non-English characters
        tm_map(corpus, tolower) %>%                                             # All letters to lower case
        tm_map(removePunctuation) %>%                                           # Remove punctuation
        tm_map(removeNumbers) %>%                                               # Remove numbers
        tm_map(stripWhitespace) %>%                                             # Remove unnecessary white space
        tm_map(PlainTextDocument)                                               # Preprocessed text converted to plain text

## Word Cloud

wordcloud(corpus_clean, max.words=100, random.order=FALSE, colors=brewer.pal(8,"Dark2"))
# ggsave("wordcloud.png", height = 12, width = 12)

## Frequency Distributions by N-Gram

unigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

unigram_matrix <- TermDocumentMatrix(corpus_clean, control = list(tokenize = unigram_tokenizer))
bigram_matrix <- TermDocumentMatrix(corpus_clean, control = list(tokenize = bigram_tokenizer))
trigram_matrix <- TermDocumentMatrix(corpus_clean, control = list(tokenize = trigram_tokenizer))

unigram_corpus <- findFreqTerms(unigram_matrix,lowfreq = 50)
bigram_corpus <- findFreqTerms(bigram_matrix,lowfreq = 50)
trigram_corpus <- findFreqTerms(trigram_matrix,lowfreq = 50)

unigram_corpus_count <- rowSums(as.matrix(unigram_matrix[unigram_corpus, ]))
unigram_plot_data <- data.table(Word = names(unigram_corpus_count), Count = unigram_corpus_count)

bigram_corpus_count <- rowSums(as.matrix(bigram_matrix[bigram_corpus, ]))
bigram_plot_data <- data.table(Word = names(bigram_corpus_count), Count = bigram_corpus_count)

trigram_corpus_count <- rowSums(as.matrix(trigram_matrix[trigram_corpus, ]))
trigram_plot_data <- data.table(Word = names(trigram_corpus_count), Count = trigram_corpus_count)

plot_ngram <- function(data, title, num = 20) {
        plotdata <- data[order(-data$Count),][1:num,]
        plotdata$Word <- factor(plotdata$Word, levels = plotdata$Word)
        ggplot(plotdata, aes(x = Word, y = Count)) +
                geom_bar(stat = "identity") +
                theme_economist() +
                labs(title = paste0(title,"\n"), x = "\nWord", y = "Count\n") +
                scale_y_continuous(breaks = pretty_breaks(), label = comma) +
                theme(legend.position = "none",
                      axis.text = element_text( size = 12),
                      axis.text.x = element_text(angle = 90, hjust = 1),
                      axis.title = element_text(size = 20),
                      plot.title = element_text(size = 25),
                      legend.title =  element_text( size = 16) ,
                      legend.text = element_text( size = 14),
                      strip.text = element_text( size = 16),
                      axis.line = element_blank())
}

plot_ngram(unigram_plot_data, "Most Common Unigrams", 20)
plot_ngram(bigram_plot_data, "Most Common Bigrams", 20)
plot_ngram(trigram_plot_data, "Most Common Trigrams", 20)
        