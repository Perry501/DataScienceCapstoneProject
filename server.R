
suppressWarnings(library(shiny))
suppressWarnings(library(tm))
suppressWarnings(library(data.table))
suppressWarnings(library(dplyr))

load("unigram.RData")
load("bigram.RData")
load("trigram.RData")
load("quadgram.RData")

shinyServer(function(input, output) {
        
        output$OriginalStatement <- renderText({
                OriginalSentence <- input$input
                return(OriginalSentence)
        })
        
        output$PredictedNextWord <- renderText({
                OriginalSentence <- input$input
                PredictedNextWord <- "The predicted next word will be here."
                
                CleanInput <- function(input) {
                        
                        input <- iconv(input, "latin1", "ASCII", sub = "") %>%
                                VectorSource() %>%
                                VCorpus()
                        
                        input_corpus <- tm_map(input, tolower) %>%                              # All letters to lower case
                                tm_map(removePunctuation) %>%                                   # Remove punctuation
                                tm_map(removeNumbers) %>%                                       # Remove numbers
                                tm_map(stripWhitespace) %>%                                     # Remove unnecessary white space
                                tm_map(PlainTextDocument)                                       # Preprocessed text converted to plain text
                        input_clean <- as.character(input_corpus[[1]])
                        input_clean <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", input_clean)
                        
                        if (nchar(input_clean) > 0) {
                                return(input_clean) 
                        } else {
                                return("")
                        }
                }
                
                PredictNextWordFunc <- function(input)
                {
                        # Clean up the input string and extract only the words with no leading and trailing white spaces
                        input <- CleanInput(input)
                        
                        # Split the input string across white spaces and then extract the length
                        input <- unlist(strsplit(input, split=" "))
                        inputLen <- length(input)
                        
                        nextTermFound <- FALSE
                        predictedNextTerm <- as.character(NULL)
                        
                        # 1. First test the Four Gram using the four gram data frame
                        if (inputLen >= 3 & !nextTermFound)
                        {
                                # Assemble the terms of the input string separated by one white space each
                                subinput3 <- paste(input[(inputLen-2):inputLen], collapse=" ")
                                subinput2 <- paste(input[(inputLen-1):inputLen], collapse=" ")
                                subinput1 <- paste(input[(inputLen):inputLen], collapse=" ")
                                
                                allmatched <- rbind(quadgram[quadgram$FirstTerms == subinput3,],
                                                    trigram[trigram$FirstTerms == subinput2,],
                                                    bigram[bigram$FirstTerms == subinput1,])
                                lengthallmatched <- nrow(allmatched)
                                
                                if (lengthallmatched < 1) {
                                        predictedNextTerm <- unigram$phrase[unigram$Freq == max(unigram$Freq)]
                                } else {
                                        bestmatched <- allmatched$LastTerm[allmatched$Probability == max(allmatched$Probability)]
                                        
                                        if (length(bestmatched) > 1){
                                                bestmatchedDT <- data.table(phrase=NA, Freq=NA)[numeric(0), ]
                                                for (i in 1:length(bestmatched)){
                                                        bestmatchedDT <- rbind(bestmatchedDT, unigram[phrase == bestmatched[i], ])
                                                }
                                                predictedNextTerm <- bestmatchedDT$phrase[bestmatchedDT$Freq == max(bestmatchedDT$Freq)]
                                        } else {
                                                predictedNextTerm <- bestmatched
                                        }
                                }
                                
                                nextTermFound <- TRUE
                                
                        }
                        
                        # 2. Next test the Three Gram using the three gram data frame
                        if (inputLen >= 2 & !nextTermFound)
                        {
                                # Assemble the terms of the input string separated by one white space each
                                subinput2 <- paste(input[(inputLen-1):inputLen], collapse=" ")
                                subinput1 <- paste(input[(inputLen):inputLen], collapse=" ")
                                
                                allmatched <- rbind(trigram[trigram$FirstTerms == subinput2,],
                                                    bigram[bigram$FirstTerms == subinput1,])
                                lengthallmatched <- nrow(allmatched)
                                
                                if (lengthallmatched < 1) {
                                        predictedNextTerm <- unigram$phrase[unigram$Freq == max(unigram$Freq)]
                                } else {
                                        bestmatched <- allmatched$LastTerm[allmatched$Probability == max(allmatched$Probability)]
                                        
                                        if (length(bestmatched) > 1){
                                                bestmatchedDT <- data.table(phrase=NA, Freq=NA)[numeric(0), ]
                                                for (i in 1:length(bestmatched)){
                                                        bestmatchedDT <- rbind(bestmatchedDT, unigram[phrase == bestmatched[i], ])
                                                }
                                                predictedNextTerm <- bestmatchedDT$phrase[bestmatchedDT$Freq == max(bestmatchedDT$Freq)]
                                        } else {
                                                predictedNextTerm <- bestmatched
                                        }
                                }
                                
                                nextTermFound <- TRUE
                        }
                        
                        # 3. Next test the Two Gram using the three gram data frame
                        if (inputLen >= 1 & !nextTermFound)
                        {
                                # Assemble the terms of the input string separated by one white space each
                                subinput1 <- paste(input[(inputLen):inputLen], collapse=" ")
                                
                                allmatched <- bigram[bigram$FirstTerms == subinput1,]
                                lengthallmatched <- nrow(allmatched)
                                
                                if (lengthallmatched < 1) {
                                        predictedNextTerm <- unigram$phrase[unigram$Freq == max(unigram$Freq)]
                                } else {
                                        bestmatched <- allmatched$LastTerm[allmatched$Probability == max(allmatched$Probability)]
                                        
                                        if (length(bestmatched) > 1){
                                                bestmatchedDT <- data.table(phrase=NA, Freq=NA)[numeric(0), ]
                                                for (i in 1:length(bestmatched)){
                                                        bestmatchedDT <- rbind(bestmatchedDT, unigram[phrase == bestmatched[i], ])
                                                }
                                                predictedNextTerm <- bestmatchedDT$phrase[bestmatchedDT$Freq == max(bestmatchedDT$Freq)]
                                        } else {
                                                predictedNextTerm <- bestmatched
                                        }
                                }
                                
                                nextTermFound <- TRUE
                        }
                        return(predictedNextTerm)
                        
                }
                PredictedNextWord <- PredictNextWordFunc(OriginalSentence)
                
                return(PredictedNextWord)
        })
        
        output$About <- renderText({
                About <- "For more information about the model behind this application, go to my Github Repo below"
                return(About)
        })
        
        output$Link <- renderUI({
                a("Github: Perry501", 
                  href = "https://github.com/Perry501/DataScienceCapstoneProject",
                  target = "_blank")
        })
        
})
