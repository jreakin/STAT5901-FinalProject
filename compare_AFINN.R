# Load libraries 
library(tidytext)
library(tidyverse)

# Load files 
load("lexicon_norm.RDa")
load("fb_clean.RDa")

# Compute pseudo-AFINN
afinn <- get_sentiments("afinn")

# Keep only the words which are in both lexicons 
afinn_sentiments <- lexicon_norm %>% 
  inner_join(afinn)

# Perform linear regression, predicting the AFINN value as a function of the reactions
# Exclude intercept term
afinn_lmod <- lm(value ~ love + haha + wow + sad + angry - 1, data = afinn_sentiments[, -1])

# Add a variable for the pseudo_AFINN and for the difference between the two lexicons
afinn_sentiments <- afinn_sentiments %>% 
  mutate(pseudo_afinn = predict(afinn_lmod, newdata = afinn_sentiments),
         afinn_diff = value - pseudo_afinn)

# Create boxplot of the difference between AFINN and pseudo-AFINN, word-level 
p <- ggplot(data = afinn_sentiments, aes(y = afinn_diff)) + 
  geom_boxplot(fill = "#8be3ff", color = "#1f1c51") +
  labs(y = "Difference between AFINN value and pseudo-AFINN value") + 
  ggtitle("Comparison of AFINN lexicon and new lexicon") + 
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

# Save file 
ggsave(p, file = "comapre_AFINN_boxplot.png")

# Compare at post level: compute average AFINN vs. average pseudo-afinn for the posts
post_sentiments_afinn <- fb_clean %>%
  unnest_tokens(output = word, input = message) %>% 
  inner_join(afinn_sentiments, by = c("word" = "word")) %>% 
  group_by(status_num) %>% 
  summarise(avg_afinn = mean(value), avg_pseudo = mean(pseudo_afinn))

# Save file 
save(post_sentiments_afinn, file = "post_sentiments_afinn.RDa")

# Examine results
# On average, AFINN labels posts as more positive than they actually are
summary(post_sentiments_afinn$avg_afinn - post_sentiments_afinn$avg_pseudo)
boxplot(post_sentiments_afinn$avg_pseudo - post_sentiments_afinn$avg_afinn)
