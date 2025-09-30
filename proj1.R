##############################################################################################
#### proj1 - Group 33 - Extended Statistical Programming #####################################
#### Group members as below ##################################################################
#### Shuo Li (s2795688), Zhe Zhu (s2841606), Antrea Filippou (s2766374) ######################
#### Contributions as below ##################################################################
#### Shuo Li: single word token selection at random, function to simulate a sentence (33%) ###
#### Zhe Zhu: text pre-processing, create common words vector (34%) ##########################
#### Antrea Filippou: make the matrices of common word token sequences, next.word function (33%) ###
##############################################################################################

# setwd("/Users/apple/Desktop") # comment out of submitted
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")
cat("number of tokens:", length(a), "\n")

########## Step 4 Pre-processing #########################################
## (a) To remove the stage directions within "[]"
### Note: some unmatched brackets exist
left_brackets <- grep("\\[", a) # Find the position of the token containing the left bracket "["
cat("token numbers including '[':", length(left_brackets), "\n")
right_brackets <- grep("\\]", a) # Find the position of the token containing the right bracket "]"
cat("token numbers including ']':", length(right_brackets), "\n")

to_remove <- integer(0) # The token index that needs to be removed

for (i in left_brackets) {
  end <- min(i+100, length(a))
  close_brackets <- grep("\\]", a[(i+1):end]) # Search for the first token with ']' in the range from i+1 to end
  
  if (length(close_brackets)>0) {
    j <- i + close_brackets[1] # Find the index of the first right bracket ']'
    to_remove <- c(to_remove, i:j)
  } else {
    to_remove <- c(to_remove, i) # If the right bracket cannot be found, at least delete the '[' itself
  }
}

to_remove <- sort(unique(to_remove)) 
cat("token to delete:", length(to_remove), "\n")

a <- a[-to_remove] # Remove the content of the close brackets
cat("token numbers after removing stage directions:", length(a), "\n")

cat(" remaining number of tokens including'[':", sum(grepl("\\[", a)), "\n")
cat("remaining number of tokens including ']':", sum(grepl("\\]", a)), "\n")

## (b) To remove fully upper case and Arabic numerals
fully_uppercase <- a[(a == toupper(a) & !(a %in% c("I", "A")))] # Compare a with toupper(a) to find words that are fully upper case, ie. character names
head(fully_uppercase)                                           # and "I", "A"  are exceptions.
a <- a[! a %in% fully_uppercase] # Arabic numerals will also be removed
cat("After removing uppercase and numbers, number of remaining tokens:", length(a), "\n" )

## (c) To remove “_” and “-”
a <- gsub("_|-", "", a)
cat("After removing “_” and “-”, the remaining tokens:", length(a), "\n" )


## (d) Define split_punct function, Convert punctuation marks into independent tokens
split_punct <- function(words,
                             punct = c(",", "\\.", ";", "!", ":", "\\?")) {
  punct_regular <- paste(punct, collapse = "|") # regular expression
  regular <- paste0("(", punct_regular, ")")   
  
  index <- grep(punct_regular, words) # Find the index of the element in words that contains punctuation marks
  
  if (length(index) > 0) {
    words[index] <- gsub(regular, " \\1 ", words[index])
  }
  
  parts <- strsplit(words, "\\s+") # use blank characters to split words.
  out <- unlist(parts, use.names = FALSE)
  out <- out[out != ""]   
  
  punct_check <- c(",", ".", ";", "!", ":", "?")
  for (p in punct_check) {
    cat("", p, " number in txt =", sum(out == p), "\n")
  }
  
  return(out)
}

## (e) To separate the punctuation marks）
a <- split_punct(a)                  ## Implement the tokenization process: separate punctuation from words to ensure symbols (e.g., ".", ",", "!") are regarded as distinct tokens. 
cat("Step 4d: number of tokens:", length(a)) 

## (f) convert the cleaned word vector a to lower case
a <- tolower(a)                                    ## All tokens are converted to lowercase so that the Markov model processes the text in a case-insensitive manner and treats semantically identical forms (e.g. "Romeo" and "romeo") as a single token.
cat("Step 4e: head 20 words: ", head(a, 20), "\n") 

############### Step 5 #################################################
## (a) To find the vector of unique words in the cleaned text a.
b <- unique(a)                   ## The vocabulary of the text is defined by extracting the vector of unique tokens via the unique() function
length(b)                        ## The number of unique tokens

## (b) To find the vector of indices indicating which element in the unique word vector each element in the text corresponds to
indices <- match(a, b)           ## Generates a numerical representation of the text by matching each token to its location in the vocabulary b. Crucial first step in developing the Markov model.    
indices       

## (c) To count up how many times each unique word occurs in the text
word_counts <- tabulate(indices) ## The tabulate(indices) command calculates the frequency distribution of the vocabulary,counting how many times each unique token appears in the text.The result is stored in the vector word_counts, which is the empirical basis for identifying the most common tokens. 
word_counts                    

## (d) To find top 1000 common words
top_1000_indices <- order(word_counts, decreasing = TRUE)[1:1000]## Identifies the indices of the 1000 most frequent tokens by sorting the frequencies in descending order.
b <- b[top_1000_indices]            ## Keeps only the 1000 most common tokens, defining the basic vocabulary for the Markov model.
print(b)                              

###### Step 6 To make the matrices of common word token sequences ######
## (a) If a word is not in b, then match gives an NA for that word.
M1 <- match(a, b)   ## Each token of the text is mapped to its position in the vocabulary b.
                    ## Tokens that are not in the vocabulary (except top-1000) are given the value NA.
                    ## This creates a numeric vector that represents the entire text.
length(M1)          ## Checks that the length of vector M1 equals the number of tokens in the text.
head(M1, 100)      

## (b) Create an (n - mlag) × (mlag + 1) matrix, M
### Firstly, we use mlag equals to 4.
mlag <- 4         ## The maximum lag considered
n <- length(M1)   
print(n)         
nrows <- n - mlag 

M <- matrix(NA, nrow = nrows, ncol = mlag + 1)  ## Initialize a matrix with dimensions (n ​​- mlag) × (mlag + 1).
for (j in 1:(mlag + 1)) {
  M[, j] <- M1[j:(j + nrows - 1)]              ## Each column corresponds to a shifted version of M1.Each line contains a sequence of mlag+1 tokens.
}
print(M[1:10,])                                ## Display the first 10 lines for review

M <- M[!is.na(M[, mlag + 1]), , drop = FALSE]  ## Remove lines with NA in the last token
dim(M)                                         

######## Step 7 Write a function ############################
next.word <- function(key, M, M1, w = rep(1, ncol(M) - 1)) {
  m <- ncol(M) - 1                         ## Defines the maximum Markov order from the structure of M.
  
  if (length(key) > m) key <- tail(key, m) ## Limits the context to the last m tokens if it is larger.
  
  alternative <- integer(0)               ## Gathers the candidate "next" tokens.
  prob <- numeric(0)                      ## And the corresponding sampling weights from the mixture.
  
  L <- length(key)                        ## Current length of the available context.
  
  for (s in seq_len(L)) {
    sub <- key[s:L]                      ## Defines the subsequence (context) that will be compared to the columns of matrix M.
    r <- length(sub)
    mc <- m - r + 1                      ## Calculates the starting column in M ​​so that the sub is correctly aligned with the last r columns.
    
    ii <- colSums(!(t(M[, mc:m, drop = FALSE]) == sub))
    match <- which(ii == 0 & is.finite(ii))
    
    if (length(match) > 0) {
      u <- M[match, m + 1]
      u <- u[!is.na(u)]   ### Filters out any NA
      nu <- length(u)
      if (nu > 0) {
        weight <- w[mc]            
        alternative <- c(alternative, u)
        prob <- c(prob, rep(weight / nu, nu))
      }
    }
  }
  
  if (length(alternative) == 0) {
    m1 <- M1[!is.na(M1)] # non na
    frequence_b <- tabulate(m1, nbins = max(M1, na.rm = TRUE))
    return(sample(seq_along(frequence_b), 1, prob = frequence_b))
  }
  
  select_position <- sample(seq_along(alternative), 1, prob = prob)
  return(alternative[select_position])
}

####################################################################
### Step 8. Select a single word token at random (no punctuation) ##
####################################################################
punct_chars <- c(",", ".", ";", "!", ":", "?", "-", "—", "_")  
# Define punctuation characters not used as start words.

non_punct_indices <- which(!(b %in% punct_chars) & grepl("^[a-z]+$", b))  
# Candidate start words are determined as:
#  not in the punctuation set
#  must be alphabetic only (regex "^[a-z]+$")

start_token <- sample(non_punct_indices, 1)  # Randomly select a valid index
start_word <- b[start_token]                 # Convert index to actual word

cat("Step 8: randomly selected start word =", start_word, "\n\n")


##################################################################
###### Step 9. Function to simulate a sentence ###################
##################################################################
simulate_sentence <- function(M, M1, b, start_word, 
                              min_clause_len=5, max_len=30, debug=FALSE) {
  ## Simulates a sentence given a Markov model context.
  ##   b               vocabulary of common tokens
  ##   min_clause_len  minimum number of words of a sentence before punctuation
  ##   max_len         maximum length of th whole sentence
  ##   debug           if TRUE, prints intermediate tokens for debugging
  
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
      words <- c(words, ".")       # Append "." if no punctuation exists when the sentence produced is too long.
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

# Run the model for m = 3, 4, 5 respectively
run_models(M1, b, start_word, m_values=c(3,4,5))







