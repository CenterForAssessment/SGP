`capwords` <- 
function(
    x,
    special.words = c("ELA", "I", "II", "III", "IV", "CCSD", "CUSD", "CUD", "USD", "PSD", "UD", "ESD", "DCYF", "EMH", "HS", "MS", "ES", "SES", "IEP", "ELL", "MAD", "PARCC", "SBAC", "SD", "SWD", "US", "SGP", "SIMEX", "SS", "SAT", "PSAT", "WIDA", "ACCESS", "WIDA-ACCESS")
) {
    if (is.null(x)) return(NULL)
    if (is.na(x)) return(NA)
    if (identical(x, " ")) return(" ")
    
    # Basic cleaning: replace underscores, periods, and extra spaces
    x <- gsub("[_.]", " ", x)
    x <- gsub("\\s+", " ", trimws(x))  # Trim and reduce multiple spaces

    # Helper function to capitalize while respecting special words and numbers
    capitalize_words <- function(word) {
        # Handle special words
        if (word %in% special.words) {
            return(word)
        } else if (grepl("^[0-9]+(\\.[0-9]+)?$", word)) {  # Check for numbers
            return(word)
        } else {
            # Capitalize the first letter only
            return(paste0(toupper(substring(word, 1, 1)), tolower(substring(word, 2))))
        }
    }

    # Split words and process each, handling punctuation (hyphens, apostrophes)
    words <- unlist(strsplit(x, "(?=[\\s'-])|(?<=[\\s'-])", perl = TRUE))
    result <- sapply(words, capitalize_words)

    # Combine processed words and fix spacing around punctuation
    s.new <- paste(result, collapse = "")
    s.new <- gsub("\\s+([)-])", "\\1", gsub("([(])\\s+", "\\1", s.new))
    
    return(s.new)
} ### END capwords