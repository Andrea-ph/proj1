#proj1
# Group members:
# Shuo Li (s2795688), Zhe Zhu (s2841606), Antrea Filippou (s2766374)
#
# Contributions:
# Name1: xx% 
# Name2: xx% 
# Name3: xx% 
#############################
#############################
#setwd("/Users/apple/Desktop") ## comment out of submitted
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")
cat("token numbers:", length(a), "\n")

# Pre-processing
## (a) To remove the stage directions
left_brackets <- grep("\\[", a)
cat("token numbers including '[':", length(left_brackets), "\n")
right_brackets <- grep("\\]", a)
cat("token numbers including ']':", length(right_brackets), "\n")

to_remove <- integer(0)

for (i in left_brackets) {
  end <- min(i+100, length(a))
  close_brackets <- grep("\\]", a[(i+1):end])
  
  if (length(close_brackets)>0) {
    j <- i + close_brackets[1] 
    to_remove <- c(to_remove, i:j)
  } else {
    to_remove <- c(to_remove, i) 
  }
}

to_remove <- sort(unique(to_remove)) 
cat("token to delete:", length(to_remove), "\n")

a <- a[-to_remove]
cat("token numbers after deletion:", length(a), "\n")

cat(" remaining number of tokens including'[':", sum(grepl("\\[", a)), "\n")
cat("remaining number of tokens including ']':", sum(grepl("\\]", a)), "\n")

## (b) To remove fully upper case and Arabic numerals
fully_uppercase <- a[(a == toupper(a) & !(a %in% c("I", "A")))]
head(fully_uppercase)
a <- a[! a %in% fully_uppercase]
cat("After removing uppercase and numbers, the remaining tokens:", length(a), "\n" )

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
a <- split_punct(a)
cat("Step 4d:  token  =", length(a))


## (f) convert the cleaned word vector a to lower case
a <- tolower(a)
cat("Step 4e: head 20 words =", head(a, 20), "\n")

#Step5
## (a) To find the vector of unique words in the cleaned text a.
b <- unique(a)
length(b) 

## (b) To find the vector of indices indicating which element in the unique word vector each element in the text corresponds to
indices <- match(a, b)     
indices       

## (c) To count up how many times each unique word occurs in the text
word_counts <- tabulate(indices)   
word_counts

## (d) To find top 1000 common words
top_1000_indices <- order(word_counts, decreasing = TRUE)[1:1000]
b <- b[top_1000_indices]
print(b)

#Step6 To make the matrices of common word token sequences
## (a) If a word is not in b, then match gives an NA for that word.
M1 <- match(a, b)
length(M1)
head(M1, 100)

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

#Step7 Write a function
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

#Step 8 select a single word token at random
punct_chars <- c(",", ".", ";", "!", ":", "?")

non_punct_indices <- which(!(common %in% punct_chars))

## select a start word at random
start_token <- sample(non_punct_indices, 1)
start_word <- common[start_token]
cat("Step 8: select a start word at random =", start_word, "\n")

#Step 9 simulate a sentence
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

# Generate a sentence
cat("Step 9: simulate from the model →\n")
cat(simulate_sentence(M, tokens, common, start_word=start_word, debug=TRUE), "\n")



