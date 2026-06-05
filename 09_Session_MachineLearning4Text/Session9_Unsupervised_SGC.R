## Code written by Sofia Gil-Clavel for "Session 5: R-Workshop Statistical Learning"
## April 25th, 2025.

# Cleaning the environment
rm(list = ls())
gc()

# Installing all the packages
# install.packages("tidytext")
# install.packages("textrecipes")
# install.packages("text2vec")
# install.packages("LDAvis")
# install.packages("tm")
# install.packages("ggwordcloud")

#### Introduction to lists ####





#### Topic Modeling ####
library(tm)
library(textrecipes)
library(text2vec)
library(ggwordcloud)
library(tidyverse)

#*** Preparing the data ***#

data("acq")

ACQ<-data.frame(text=unlist(sapply(acq, `[[`, "content")), 
                stringsAsFactors=FALSE)

#*** Creating a base text recipe ***#

# ACQ_rec <- recipe(~ ?, data = ACQ) %>%
#   
#   # 1. Break text into individual words (tokens)
#   step_?(text) %>%
#   
#   # 2. REMOVE STOP WORDS FIRST (while tokens are still single words)
#   step_?(text, keep = FALSE)%>%
#   
#   # 3. BUILD N-GRAMS SECOND (from the remaining meaningful words)
#   step_?(text, num_tokens = 2, min_num_tokens = 1) %>%
#   
#   # 4. Filter or convert to features
#   step_?(text, max_tokens = 1000)

#*** Checking the document-term matrix ***#

# What does this command return?
bake(prep(ACQ_rec%>%step_tf(text)), new_data = NULL)

#*** Adding LDA as a step & running the recipe ***#

# ACQ_obj<-ACQ_rec%>%
#   # 5. lda
#   step_?(text, num_topics = 4)%>%
#   prep()  

# Extracting the LDA element 

# ACQ_LDA<-ACQ_obj$steps[[?]]$lda_models$text

# Using the LDA components 

# ACQ_LDA$?(n=?)

ACQ_LDA$plot()

#*** 3.3 Latent Dirichlet allocation ***#

# There is also the option to add other LDA models:

tokens <- word_tokenizer(tolower(ACQ$text))
it <- itoken(tokens, ids = seq_along(ACQ$text))
v <- create_vocabulary(it)
v = prune_vocabulary(v, term_count_min = 10, doc_proportion_max = 0.1)
dtm <- create_dtm(it, vocab_vectorizer(v))
lda_model <- LDA$new(n_topics = 3)

ACQ_obj2 <- ACQ_rec%>%
  # 5. lda
  step_lda(text, lda_models = lda_model)%>%
  prep()  

# Extracting the LDA element 

ACQ_LDA2<-ACQ_obj2$steps[[5]]$lda_models$text

# Using the LDA components 

ACQ_LDA2$get_top_words(n=10)

ACQ_LDA2$plot()

#*** Phi contains the probabilities ***#

# Pull raw matrix and vocabulary names from the private environment
phi_matrix_raw <- ACQ_LDA$.__enclos_env__$private$components_
vocab_names    <- ACQ_LDA$.__enclos_env__$private$vocabulary

# Format into a tidy data frame of probabilities
rownames(phi_matrix_raw) <- paste0("Topic ", 1:nrow(phi_matrix_raw))
colnames(phi_matrix_raw) <- vocab_names

phi_matrix_probs <- t(phi_matrix_raw / rowSums(phi_matrix_raw))

# 5. Filter for the top words per topic to avoid cluttering the cloud
top_words_per_topic <- as.data.frame(phi_matrix_probs) %>%
  tibble::rownames_to_column(var = "words") %>%
  pivot_longer(
    cols = -words, 
    names_to = "topic", 
    values_to = "probability"
  ) %>%
  group_by(topic) %>%
  slice_max(probability, n = 30) %>% # Keep the top 30 words per topic
  ungroup()

# 6. Plot the faceted word clouds
ggplot(top_words_per_topic, aes(label = words, size = probability, color = topic)) +
  geom_text_wordcloud_area(seed = 123) +
  scale_size_area(max_size = 15) + # Controls maximum word size
  facet_wrap(~ topic, scales = "free") +
  theme_minimal() +
  labs(title = "LDA Topic Word Clouds", subtitle = "Word sizes reflect topic-word probability (Phi)") +
  theme(strip.text = element_text(face = "bold", size = 12))

# Use ggsave to save the graph:
# ggsave("<link to your folder>/topics_cloud.png",
#        plot=cloud,width = 30,height = 20,units = "cm")






