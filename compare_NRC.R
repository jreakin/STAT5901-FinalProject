# Load libraries 
library(tidytext)
library(tidyverse)
library(data.table)
library(mltools)
library(reshape2)
library(lda)

# Set working directory
setwd("~/Documents/School/STAT 5901/facebook/datasets2")

# Load files 
load("lexicon_norm.RDa")
load("fb_clean.RDa")
load("documents.RDa")

# Get the NRC lexicon 
nrc <- get_sentiments("nrc")

# Define the sentiments in NRC that match with the ones in `lexicon_norm`
our_sentiments <- c("positive", "joy", "surprise", "sadness", "anger")

# Create a "threshold" lexicon, for varying thresholds 
# Initiaize values
thresholds <- seq(0.10, 0.90, by = 0.01)
prop_matched <- vector(mode = "list", length = length(thresholds))
prop_match_love <- vector(length = length(thresholds))
prop_match_haha <- vector(length = length(thresholds))
prop_match_wow <- vector(length = length(thresholds))
prop_match_sad <- vector(length = length(thresholds))
prop_match_angry <- vector(length = length(thresholds))
for (i in 1:length(thresholds)) {
  # For the specified threshold, set emotions to 1 if the proportion is greater than threshold
  lexicon_thresh <- lexicon_norm
  lexicon_thresh[, 2:6] <- ifelse(lexicon_thresh[, 2:6] > thresholds[i], 1, 0)
  
  # Find all words which are in both the NRC lexicon and `lexicon_norm` 
  words_comparable <- lexicon_thresh %>% 
    inner_join(nrc)
  
  # Only look at the sentiments that map to one of our five that are in `lexicon_norm`
  words_comparable <- words_comparable[words_comparable$sentiment %in% our_sentiments, ]
  
  # Convert to factor variable so one-hot encoding can be used
  words_comparable$sentiment <- as.factor(words_comparable$sentiment)
  
  # Compare the two matrices (`lexicon_norm` vs. NRC) element-wise
  # Create matrix containing 1s and 0s for each word, using `lexicon_norm`
  df1 <- words_comparable[, 1:6]
  # Create matrix containing 1s and 0s for each word, using the NRC lexicon
  df2 <- as.data.frame(cbind(words_comparable[, 1], 
                             one_hot(as.data.table(words_comparable[, 7]))))
  df2 <- df2 %>% 
    group_by(V1) %>% 
    mutate(V1_anger = sum(V1_anger), V1_joy = sum(V1_joy), V1_positive = sum(V1_positive),
           V1_sadness = sum(V1_sadness), V1_surprise = sum(V1_surprise)) %>% 
    ungroup()
  
  # Rearrange the order of the variables so the two matrices can be compared directly
  df2 <- data.frame(word = df2$V1, love = df2$V1_positive, haha = df2$V1_joy, 
                    wow = df2$V1_surprise, sad = df2$V1_sadness, angry = df2$V1_anger)
  
  # Only keep the unique rows
  df1 <- df1 %>% distinct()
  df2 <- df2 %>% distinct()
  
  # Calculate the proportion of correct matches for each of the words
  prop_matched[[i]] <- vector(length = nrow(df1))
  prop_matched[[i]] <- apply(df1[, 2:6] == df2[, 2:6], 1, FUN = function(x) {sum(x) / 5})
  
  # Calculate the proportion of correct matches across all words, for each emotion individually
  prop_match_love[i] <- mean(prop_matched[[i]][df2$love == 1])
  prop_match_haha[i] <- mean(prop_matched[[i]][df2$haha == 1])
  prop_match_wow[i] <- mean(prop_matched[[i]][df2$wow == 1])
  prop_match_sad[i] <- mean(prop_matched[[i]][df2$sad == 1])
  prop_match_angry[i] <- mean(prop_matched[[i]][df2$angry == 1])
}

# Visualize the results
# Create data.frame of results to be passed to ggplot 
mean_prop_matched <- unlist(lapply(prop_matched, mean))
prop_correct <- data.frame(mean_prop_matched, prop_match_love, 
                           prop_match_haha, prop_match_wow, 
                           prop_match_sad, prop_match_angry, 
                           x = thresholds)
prop_correct <- melt(prop_correct, id = "x")

# Create plot of correct matches, where colour represents emotion
ggplot(data = prop_correct, aes(y = value, x = x, color = variable)) +
  geom_line() + 
  labs(x = "Threshold", y = "Proportion matched with NRC lexicon") +
  ggtitle("Comparison of threshold lexicon to NRC lexicon")

# Calculate average sentiment per post, using `lexicon_norm`
# Note: only posts which had words remaining after removing stop words, 
# non-dictionary words, and words which were not in the NRC lexicon will remain. The 
# `status_num` variable can be used to map back to the original posts.
post_sentiments_norm <- fb_clean %>%
  unnest_tokens(output = word, input = message) %>% 
  inner_join(lexicon_norm, by = c("word" = "word")) %>% 
  group_by(status_num) %>% 
  summarise(sentiment_love = mean(love), sentiment_haha = mean(haha), 
            sentiment_wow = mean(wow), sentiment_sad = mean(sad), 
            sentiment_angry = mean(angry))

# Save file
save(post_sentiments_norm, file = "post_sentiments_norm.RDa")

# Perform SLDA to predict sentiment according to `lexicon_norm`
# The `slda.em` function takes the response variable via the argument `annotations` 
slda_annotations <- fb_clean %>% left_join(post_sentiments_norm)

# Create training and test set (80/20 split)
# Set seed for reproducibility 
set.seed(23)
possible_indices <- which(complete.cases(slda_annotations))
train_indices <- as.integer(sample(possible_indices, 
                                   size = round(0.8 * length(possible_indices), 0),
                                   replace = FALSE))
test_indices <- possible_indices[-which(possible_indices %in% train_indices)]
documents_train <- documents[train_indices]
documents_test <- documents[test_indices]

# Set the number of topics for the SLDA
K <- 10

# Perform SLDA for the love sentiment
slda_love <- slda.em(documents = documents_train,
                     K = K,
                     vocablist$word,
                     num.e.iterations = 10,
                     num.m.iterations = 5,
                     variance = 0.25,
                     params = rnorm(K),
                     alpha = 50 / K,
                     eta = 1 / K,
                     lambda = 1.0,
                     annotations = slda_annotations$sentiment_love[train_indices],
                     logistic = FALSE,
                     method = "sLDA")

# Save file
save(slda_love, file = "slda_love.RDa")

# Do predictions for the love sentiment
predictions_love <- slda.predict(documents = documents_test,
                                 topics = slda_love$topics,
                                 model = slda_love$model,
                                 alpha = 50 / K,
                                 eta = 1 / K)

# Save file
save(predictions_love, file = "predictions_love.RDa")

# Perform SLDA for the haha sentiment
slda_haha <- slda.em(documents = documents_train,
                     K = K,
                     vocablist$word,
                     num.e.iterations = 10,
                     num.m.iterations = 5,
                     variance = 0.25,
                     params = rnorm(K),
                     alpha = 50 / K,
                     eta = 1 / K,
                     lambda = 1.0,
                     annotations = slda_annotations$sentiment_haha[train_indices],
                     logistic = FALSE,
                     method = "sLDA")

# Save file
save(slda_haha, file = "slda_haha.RDa")

# Do predictions for the haha sentiment
predictions_haha <- slda.predict(documents = documents_test,
                                 topics = slda_haha$topics,
                                 model = slda_haha$model,
                                 alpha = 50 / K,
                                 eta = 1 / K)

# Save file
save(predictions_haha, file = "predictions_haha.RDa")

# Perform SLDA for the wow sentiment
slda_wow <- slda.em(documents = documents_train,
                    K = K,
                    vocablist$word,
                    num.e.iterations = 10,
                    num.m.iterations = 5,
                    variance = 0.25,
                    params = rnorm(K),
                    alpha = 50 / K,
                    eta = 1 / K,
                    lambda = 1.0,
                    annotations = slda_annotations$sentiment_wow[train_indices],
                    logistic = FALSE,
                    method = "sLDA")

# Save file
save(slda_wow, file = "slda_wow.RDa")

# Do predictions for the wow sentiment
predictions_wow <- slda.predict(documents = documents_test,
                                topics = slda_wow$topics,
                                model = slda_wow$model,
                                alpha = 50 / K,
                                eta = 1 / K)

# Save file
save(predictions_wow, file = "predictions_wow.RDa")

# Perform SLDA for the sad sentiment
slda_sad <- slda.em(documents = documents_train,
                    K = K,
                    vocablist$word,
                    num.e.iterations = 10,
                    num.m.iterations = 5,
                    variance = 0.25,
                    params = rnorm(K),
                    alpha = 50 / K,
                    eta = 1 / K,
                    lambda = 1.0,
                    annotations = slda_annotations$sentiment_sad[train_indices],
                    logistic = FALSE,
                    method = "sLDA")

# Save file
save(slda_sad, file = "slda_sad.RDa")

# Do predictions for the sad sentiment
predictions_sad <- slda.predict(documents = documents_test,
                                topics = slda_sad$topics,
                                model = slda_sad$model,
                                alpha = 50 / K,
                                eta = 1 / K)
save(predictions_sad, file = "predictions_sad.RDa")

# Perform SLDA for the angry sentiment
slda_angry <- slda.em(documents = documents_train,
                      K = K,
                      vocablist$word,
                      num.e.iterations = 10,
                      num.m.iterations = 5,
                      variance = 0.25,
                      params = rnorm(K),
                      alpha = 50 / K,
                      eta = 1 / K,
                      lambda = 1.0,
                      annotations = slda_annotations$sentiment_angry[train_indices],
                      logistic = FALSE,
                      method = "sLDA")

# Save file
save(slda_angry, file = "slda_angry.RDa")

# Do predictions for the angry sentiment
predictions_angry <- slda.predict(documents = documents_test,
                                  topics = slda_angry$topics,
                                  model = slda_angry$model,
                                  alpha = 50 / K,
                                  eta = 1 / K)

# Save file
save(predictions_angry, file = "predictions_angry.RDa")
