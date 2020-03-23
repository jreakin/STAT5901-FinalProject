# Set working directory
setwd("~/Documents/School/STAT 5901/facebook/datasets2")

# Load files
load("fb_clean.RDa")

# Create a data frame of all the words in the cleaned posts.
# Note: This only includes words in posts that were not filtered out 
# after removing stop words and non-dictionary words.
posts <- fb_clean$message
postlist <- strsplit(tolower(posts), "\\s")
vocablist <- data.frame(word = unique(unlist(postlist)), stringsAsFactors = FALSE)

# Save file
save(vocablist, file = "vocablist.RDa")

# Define function provided on Assignment 4
get_terms <- function(x, vocablist) {
  index <- match(x, vocablist)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

# For each of the cleaned posts, apply the `get_terms` function 
documents <- lapply(postlist, get_terms, vocablist$word)

# Save file
save(documents, file = "documents.RDa")
