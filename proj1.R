###########################################################
####proj1 - Group 33 - Extended Statistical Programming ###
#### Group members as below ################################
#### Shuo Li (s2795688), Zhe Zhu (s2841606), Antrea Filippou (s2766374)
#### Contributions as below ################################
#### Shuo Li: xx (xx%) ###
#### Zhe Zhu: xx (xx%) ###
#### Antrea Filippou: xx (xx%) ###
############################################################

# setwd("/Users/apple/Desktop") # comment out of submitted
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")
cat("token numbers:", length(a), "\n")

########## Step 4 Pre-processing #############################
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
  punct_regular <- paste(punct, collapse = "|")
  regular <- paste0("(", punct_regular, ")")   # 
  
  index <- grep(punct_regular, words)
  
  if (length(index) > 0) {
    words[index] <- gsub(regular, " \\1 ", words[index])
  }
  
  parts <- strsplit(words, "\\s+")
  out <- unlist(parts, use.names = FALSE)
  out <- out[out != ""]   # 
  
  punct_check <- c(",", ".", ";", "!", ":", "?")
  for (p in punct_check) {
    cat("", p, " number in txt =", sum(out == p), "\n")
  }
  
  return(out)
}

## (e) To separate the punctuation marks）
a <- split_punct(a)                  ##Implement the tokenization process: separate punctuation from words to ensure symbols (e.g., ".", ",", "!") are regarded as distinct tokens. 
cat("Step 4d:  token  =", length(a)) ##Diagnostic validation of the token count following punctuation separation

## (f) convert the cleaned word vector a to lower case
a <- tolower(a)                                    ##All tokens are converted to lowercase so that the Markov model processes the text in a case-insensitive manner and treats semantically identical forms (e.g. "Romeo" and "romeo") as a single token.
cat("Step 4e: head 20 words =", head(a, 20), "\n") ##The command prints the first 20 tokens to the Console after conversion to lowercase, acting as a diagnostic check to verify correct normalization before starting Step 5.

############### Step 5 ########################################
## (a) To find the vector of unique words in the cleaned text a.
b <- unique(a)                   ##The vocabulary of the text is defined by extracting the vector of unique tokens via the unique() function
length(b)                        ##The number of unique tokens is determined by the length(b) command, which gives a numerical indication of the size of the vocabulary that will  be used in subsequent analysis.

## (b) To find the vector of indices indicating which element in the unique word vector each element in the text corresponds to
indices <- match(a, b)           ##Generates a numerical representation of the text by matching each token to its location in the vocabulary b. Crucial first step in developing the Markov model.    
indices       

## (c) To count up how many times each unique word occurs in the text
word_counts <- tabulate(indices) ## The tabulate(indices) command calculates the frequency distribution of the vocabulary,counting how many times each unique token appears in the text.The result is stored in the vector word_counts, which is the empirical basis for identifying the most common tokens. 
word_counts                      ##Printing word_counts provides an immediate visualization of the distribution and acts as a diagnostic check before moving on to the next step.

## (d) To find top 1000 common words
top_1000_indices <- order(word_counts, decreasing = TRUE)[1:1000]## Identifies the indices of the 1000 most frequent tokens by sorting the frequencies in descending order.
b <- b[top_1000_indices]                                         ## Keeps only the 1000 most common tokens, defining the basic vocabulary for the Markov model.
print(b)                                                         ## Displays the final set of most frequent tokens.

###### Step 6 To make the matrices of common word token sequences ###
## (a) If a word is not in b, then match gives an NA for that word.
M1 <- match(a, b)   ## Each token of the text is mapped to its position in the vocabulary b.
                    ##Tokens that are not in the vocabulary (except top-1000) are given the value NA.
                    ## This creates a numeric vector that represents the entire text.
length(M1)          ##Checks that the length of vector M1 equals the number of tokens in the text.
head(M1, 100)       ##Preview the first 100 elements to see if the mapping was done correctly (indicators + NA).

## (b) Create an (n - mlag) × (mlag + 1) matrix, M
### Firstly, we use mlag equals to 4.
mlag <- 4
n <- length(M1)
print(n)
nrows <- n - mlag

M <- matrix(NA, nrow = nrows, ncol = mlag + 1)
for (j in 1:(mlag + 1)) {
  M[, j] <- M1[j:(j + nrows - 1)]
}
print(M[1:10,])

M <- M[!is.na(M[, mlag + 1]), , drop = FALSE]
dim(M)

######## Step 7 Write a function ############################
##
next.word <- function(key, M, M1, w = rep(1, ncol(M) - 1)) {
  m <- ncol(M) - 1     # 
  
  if (length(key) > m) key <- tail(key, m)
  
  alternative <- integer(0)
  prob <- numeric(0)
  
  L <- length(key)
  
  for (s in seq_len(L)) {
    sub <- key[s:L]        
    r <- length(sub)
    mc <- m - r + 1           
    
    ii <- colSums(!(t(M[, mc:m, drop = FALSE]) == sub))
    match <- which(ii == 0 & is.finite(ii))
    
    if (length(match) > 0) {
      u <- M[match, m + 1]
      u <- u[!is.na(u)]
      nu <- length(u)
      if (nu > 0) {
        weight <- w[mc]            
        alternative <- c(alternative, u)
        prob <- c(prob, rep(weight / nu, nu))
      }
    }
  }
  
  if (length(alternative) == 0) {
    m1 <- M1[!is.na(M1)]#non na
    frequence_b <- tabulate(m1, nbins = max(M1, na.rm = TRUE))
    return(sample(seq_along(frequence_b), 1, prob = frequence_b))
  }
  
  select_position <- sample(seq_along(alternative), 1, prob = prob)
  return(alternative[select_position])
}

######## Step 8 select a single word token at random #################
punct_chars <- c(",", ".", ";", "!", ":", "?")

non_punct_indices <- which(!(common %in% punct_chars))

## select a start word at random
start_token <- sample(non_punct_indices, 1)
start_word <- common[start_token]
cat("Step 8: select a start word at random =", start_word, "\n")

######## Step 9 simulate a sentence ####################################
simulate_sentence <- function(M, M1, b, start_word, max_len=100, debug=FALSE) {
  start_token <- match(start_word, b)
  if (is.na(start_token)) {
    start_token <- sample(M1[!is.na(M1)], 1)
  }
  sentence <- c(start_token)
  if (debug) cat("start word:", b[start_token], "\n")
  
  repeat {
    next_token <- next.word(sentence, M, M1, w=rep(1/(ncol(M)-1), ncol(M)-1))
    sentence <- c(sentence, next_token)
    if (debug) cat("generate:", b[next_token], "\n")
    
    if (b[next_token] == "." || length(sentence) >= max_len) break
  }
  
  return(paste(b[sentence], collapse=" "))
}

######## Generate a sentence ############################################
cat("Step 9: simulate from the model →\n")
cat(simulate_sentence(M, tokens, common, start_word=start_word, debug=TRUE), "\n")









