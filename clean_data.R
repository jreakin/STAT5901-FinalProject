# Load libraries
library(rjson)
library(tidyverse)
library(dplyr)
library(tidytext)
library(qdapDictionaries)

# Set working directory
setwd("~/Documents/School/STAT 5901/facebook/datasets2")

# Get list of filenames 
filenames <- list.files()

# Load each file, convert from JSON format to a list of data frames
posts <- vector(mode = "list", length = length(filenames))
for (i in 1:length(filenames)) {
  fb <- fromJSON(file = filenames[i])
  if (length(fb) > 0) {
    # Initialize data frame to correct size
    # Elements in the list are of different sizes, so this is the easiest way.
    temp <- data.frame(matrix(NA, nrow = length(fb), ncol = 10))
    colnames(temp) <- c("time", "message", "page", "id", "like",
                        "love", "haha", "wow", "sad", "angry")
    for (j in 1:length(fb)) {
      if (!is.null(fb[[j]]$reactions) & length(fb[[j]]$reactions > 0)) {
        temp$time[j] <- fb[[j]]$created_time
        temp$message[j] <- ifelse(!is.null(fb[[j]]$message), fb[[j]]$message, NA)
        temp$page[j] <- ifelse(!is.null(fb[[j]]$story), fb[[j]]$story, NA)
        temp$id[j] <- fb[[j]]$id
        temp$like[j] <- fb[[j]]$reactions$like
        temp$love[j] <- fb[[j]]$reactions$love
        temp$haha[j] <- fb[[j]]$reactions$haha
        temp$wow[j] <- fb[[j]]$reactions$wow
        temp$sad[j] <- fb[[j]]$reactions$sad
        temp$angry[j] <- fb[[j]]$reactions$angry
      }
    }
    posts[[i]] <- temp
  }
}

# Collapse the list of data frames into a single data frame
fb <- do.call(rbind.data.frame, posts)

# Fix date / times of posts to a standard format
fb$time <- str_replace(fb$time, pattern = "T", replacement = " ")
fb$time <- str_replace(fb$time, pattern = "\\+0000", replacement = "")
fb$time <- as.POSIXct(fb$time, tz = "GMT")

# Remove any duplicated posts, based on the post ID
fb <- fb %>% distinct(id, .keep_all = TRUE)

# Remove any posts without post content (e.g., just a picture with no message)
fb <- fb[-which(is.na(fb$message)), ]

# Remove any posts that have only "like" as the reactions
sum_not_like <- fb$love + fb$haha + fb$wow + fb$sad + fb$angry
fb <- fb[which(sum_not_like > 0), ]

# Add column for the status number
fb <- fb %>%
  mutate(status_num = row_number())

# Save file
save(fb, file = "fb.RDa")

# Create a second file which will contain "cleaned" Facebook posts
# (Remove punctuation, stop words, and non-dictionary words)
# Note: This will remove any statuses which had no words leftover after stop words and
# non-dictionary words were removed. The `status_num` variable can be used to match 
# back to the original posts.
fb_clean <- fb %>%
  group_by(status_num) %>%
  unnest_tokens(output = word, input = message) %>%
  anti_join(stop_words) %>%
  inner_join(data.frame(word = GradyAugmented)) %>%
  summarise(message = paste0(word, collapse = " "))

# Save file
save(fb_clean, file = "fb_clean.RDa")
