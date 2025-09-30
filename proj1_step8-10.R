##############################################
# Step 8. Select a single word token at random (no punctuation)
##############################################
punct_chars <- c(",", ".", ";", "!", ":", "?", "-", "—", "_")  
# Define punctuation characters not used as start words.

non_punct_indices <- which(!(b %in% punct_chars) & grepl("^[a-z]+$", b))  
# Candidate start words are determined as:
#   - not in the punctuation set
#   - must be alphabetic only (regex "^[a-z]+$")

start_token <- sample(non_punct_indices, 1)  # Randomly select a valid index
start_word <- b[start_token]                 # Convert index to actual word

cat("Step 8: randomly selected start word =", start_word, "\n\n")


##############################################
# Step 9. Function to simulate a sentence
##############################################
simulate_sentence <- function(M, M1, b, start_word, 
                              min_clause_len=5, max_len=30, debug=FALSE) {
  ## Simulates a sentence given a Markov model context.
  ##   b              - vocabulary of common tokens
  ##   min_clause_len - minimum number of words of a sentence before punctuation
  ##   max_len        - maximum length of th whole sentence
  ##   debug          - if TRUE, prints intermediate tokens for debugging
  
  # Convert the starting word into its index
  start_token <- match(start_word, b)
  if (is.na(start_token)) {
    # Fallback: if the word is not found, pick a random token
    start_token <- sample(M1[!is.na(M1)], 1)
  }
  sentence <- c(start_token)  # Initialize sentence with start token
  
  words_since_punct <- 1  # Counter since last punctuation mark
  
  repeat {
    # Predict the next word using the Markov model
    next_token <- next.word(sentence, M, M1, 
                            w=rep(1/(ncol(M)-1), ncol(M)-1))
    
    # Rule: block punctuation if there are too few words since the last one
    if (words_since_punct < min_clause_len && 
        b[next_token] %in% c(",", ".", ";", "!", ":", "?")) {
      next
    }
    
    # Add the predicted token
    sentence <- c(sentence, next_token)
    
    # Update the word counter
    if (b[next_token] %in% c(",", ".", ";", "!", ":", "?")) {
      words_since_punct <- 0
    } else {
      words_since_punct <- words_since_punct + 1
    }
    
    # Stop if:
    if (b[next_token] == "." && length(sentence) >= min_clause_len) {
      break  # reached period with enough words
    }
    if (length(sentence) >= max_len) {
      break  # reached maximum sentence length
    }
  }
  
  # --- Post-processing: enforce ending with a period ---
  words <- b[sentence]
  if (!("." %in% words)) {
    # If no ".", backtrack to last punctuation
    last_punct <- max(which(words %in% c(",", ";", "!", ":", "?")), na.rm=TRUE)
    if (is.finite(last_punct)) {
      words <- words[1:last_punct]
      words[length(words)] <- "."  # Replace last punctuation with "."
    } else {
      words <- c(words, ".")       # Append "." if no punctuation exists
    }
  } else {
    # If multiple ".", cut at the last one
    last_dot <- max(which(words == "."))
    words <- words[1:last_dot]
  }
  
  return(paste(words, collapse=" "))  # Return the final sentence as a string
}


##############################################
# Step 10. Run the model for m = 3, 4, 5
##############################################
run_models <- function(M1, b, start_word, m_values=c(3,4,5)) {
  # Prints the chosen starting word and simulates sentences for each m.
  #   m_values   - vector of Markov orders (default: 3,4,5)
  
  cat("Starting word:", start_word, "\n\n")  # Report the starting word once
  
  for (m in m_values) {
    cat("=========== m =", m, "===========\n")
    
    # Build context matrix for the given m
    n <- length(M1)
    nrows <- n - m
    M <- matrix(NA, nrow = nrows, ncol = m + 1)
    for (j in 1:(m + 1)) {
      M[, j] <- M1[j:(j + nrows - 1)]
    }
    M <- M[!is.na(M[, m + 1]), , drop = FALSE]
    
    # Generate one sentence using simulate_sentence()
    sentence <- simulate_sentence(M, M1, b, start_word=start_word,
                                  min_clause_len=5, max_len=30, debug=FALSE)
    cat(sentence, "\n\n")
  }
}

# Run the model for m = 3, 4, 5
run_models(M1, b, start_word, m_values=c(3,4,5))

