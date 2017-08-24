CleanInput <- function(input) {
        # Test sentence
        #input <- "Hello all , . . % , i'll be on my"
        
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

