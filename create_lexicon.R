# Load libraries 
library(tidyverse)
library(tidytext)
library(qdapDictionaries)
library(dplyr)

# Load files
load("fb.RDa")

# Prepare data to be turned into a TF-IDF matrix
word_count <- fb %>%
  unnest_tokens(output = word, input = message) %>%
  anti_join(stop_words) %>%
  inner_join(data.frame(word = GradyAugmented)) %>%
  dplyr::count(status_num, word, sort = TRUE)

# Create TF-IDF matrix
total_words <- word_count %>%
  group_by(status_num) %>%
  summarise(total = sum(n))
word_count <- left_join(word_count, total_words)
tfidf <- word_count %>%
  bind_tf_idf(term = word, document = status_num, n = n)

# The following code would transform data to the desired format, but 
# is too inefficient to run on an object of this size.
# library(reshape2)
# tfidf_tidy <- dcast(tfidf, formula = word ~ status_num, value.var = "tf_idf")

# Save file 
save(tfidf, file = "tfidf.RDa")

# # Create status-by-emotion / status-by-reaction matrix
sum_reaction <- fb$like + fb$love + fb$haha + fb$wow + fb$sad + fb$angry
status_reaction <- data.frame(p_love = fb$love / sum_reaction,
                              p_haha = fb$haha / sum_reaction,
                              p_wow = fb$wow / sum_reaction,
                              p_sad = fb$sad / sum_reaction,
                              p_angry = fb$angry / sum_reaction,
                              row.names = fb$status_num)

# Create non-normalized lexicon
# Get the unique words in all of the posts
unique_words <- unique(tfidf$word)

# Initialize the lexicon: one row for each word, one column for each emotion
lexicon <- data.frame(matrix(NA, nrow = length(unique_words), ncol = 6))
colnames(lexicon) <- c("word", "love", "haha", "wow", "sad", "angry")

# Create the "right format" to be later filled in, in the loop
right_format <- data.frame(status_num = 1:nrow(fb))
for (i in 1:length(unique_words)) {
  # The following will create what `dcast` would have created, had our object been small enough
  # to use the `dcast` function.
  current_word <- unique_words[i]
  indices <- which(tfidf$word == current_word)
  temp <- data.frame(status_num = tfidf$status_num[indices],
                     tf_idf = tfidf$tf_idf[indices])
  temp <- full_join(right_format, temp)
  
  # Set NAs to zeores in order to take dot product
  temp[is.na(temp)] <- 0
  
  # Take dot product of `temp` and `status_reaction`, for each reaction
  lexicon$word[i] <- current_word
  lexicon$love[i] <- t(as.numeric(temp$tf_idf)) %*% as.numeric(status_reaction$p_love)
  lexicon$haha[i] <- t(as.numeric(temp$tf_idf)) %*% as.numeric(status_reaction$p_haha)
  lexicon$wow[i] <- t(as.numeric(temp$tf_idf)) %*% as.numeric(status_reaction$p_wow)
  lexicon$sad[i] <- t(as.numeric(temp$tf_idf)) %*% as.numeric(status_reaction$p_sad)
  lexicon$angry[i] <- t(as.numeric(temp$tf_idf)) %*% as.numeric(status_reaction$p_angry)
}

# Save file
save(lexicon, file = "lexicon.RDa")

# Normalize the lexicon so that each row sums to 1
sum_reaction <- lexicon$love + lexicon$haha + lexicon$wow + lexicon$sad + lexicon$angry
lexicon_norm <- data.frame(word = lexicon$word,
                           love = lexicon$love / sum_reaction,
                           haha = lexicon$haha / sum_reaction,
                           wow = lexicon$wow / sum_reaction,
                           sad = lexicon$sad / sum_reaction,
                           angry = lexicon$angry / sum_reaction)
# Save file
save(lexicon_norm, file = "lexicon_norm.RDa")
