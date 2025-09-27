a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")
# Step 4a: Remove stage directions in Shakespeare text
# Process left and right halves separately

# Split text into left and right halves
mid <- ceiling(length(a)/2)
left <- a[1:mid]
right <- a[(mid+1):length(a)]

# Function to remove stage directions from a vector of words
remove_stage_directions <- function(words) {
  
  # Find positions of words containing '['
  start_idx <- grep("\\[", words)
  
  # Vector to store positions of words to remove
  remove_idx <- c()
  
  # Loop over each starting '['
  for (i in start_idx) {
    # Look at the next 100 words to find the matching ']'
    end_search <- i:min(i+100, length(words))
    
    # Find the first word containing ']'
    end_idx <- grep("\\]", words[end_search])
    
    if (length(end_idx) > 0) {
      # Get actual index in the original vector
      end_idx <- end_search[end_idx[1]]
      
      # Add all indices from start to end to remove_idx
      remove_idx <- c(remove_idx, i:end_idx)
    } else {
      # If no matching ']', just remove the '[' word
      remove_idx <- c(remove_idx, i)
    }
  }
  
  # Remove all stage direction words
  words_clean <- words[-remove_idx]
  return(words_clean)
}

# Apply function to left and right halves separately
left_clean <- remove_stage_directions(left)
right_clean <- remove_stage_directions(right)

# Combine the cleaned halves
a_clean <- c(left_clean, right_clean)
# Step 4b: Remove fully uppercase words (names, headings) and numbers
# Keep "I" and "A" as exceptions

# Identify uppercase words (excluding "I" and "A")
is_upper <- a_clean == toupper(a_clean) & !(a_clean %in% c("I","A"))

# Identify words that are numbers (arabic numerals)
is_number <- grepl("^[0-9]+$", a_clean)

# Remove both from the text
a_clean <- a_clean[!(is_upper | is_number)]
# Step 4c: Remove quotes and optionally hyphens from words
# Using gsub for regex replacement

# Remove double quotes
a_clean <- gsub('"', '', a_clean)

# Remove single quotes
a_clean <- gsub("'", '', a_clean)

# Remove hyphens (optional)
a_clean <- gsub("-", '', a_clean)
# Step 4d: split_punct function
# Splits punctuation from words and inserts it as separate entries
split_punct <- function(words, punct=c(",", ".", ";", "!", ":", "?")) {
  new_words <- c()
  
  for (w in words) {
    matched <- FALSE
    for (p in punct) {
      # Check if punctuation is in the word
      if (grepl(paste0("\\",p), w)) {
        # Split the word by punctuation
        parts <- strsplit(w, paste0("\\",p))[[1]]
        
        # Recombine parts with punctuation as separate entries
        combined <- c(rbind(parts, rep(p, length(parts)-1)))
        new_words <- c(new_words, combined)
        matched <- TRUE
        break
      }
    }
    if (!matched) new_words <- c(new_words, w)
  }
  
  return(new_words)
}
# Step 4e: Apply split_punct to entire text
a_clean <- unlist(lapply(a_clean, function(x) split_punct(x)))
# Step 4f: Convert all words to lowercase for model consistency
a_clean <- tolower(a_clean)
# Step 5a: Find unique words
b <- unique(a_clean)

# Step 5b: Match each word in text to index in unique word vector
index_vector <- match(a_clean, b)

# Step 5c: Count frequencies of each unique word
word_counts <- tabulate(index_vector)

# Step 5d: Keep ~1000 most common words
top_k <- 1000
top_indices <- order(word_counts, decreasing=TRUE)[1:top_k]

# Vector of most common words
b_common <- b[top_indices]

