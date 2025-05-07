## Code written by Sofia Gil-Clavel for "Session 5: R-Workshop Statistical Learning"
## April 25th, 2025.

# Cleaning the environment
rm(list = ls())
gc()

# Installing all the packages
# installed.packages(tidytext)
# installed.packages(textmineR)
# installed.packages(ggwordcloud)

#### Topic Modeling ####
library(tidytext)
library(textmineR)
library(ggwordcloud)
library(tm)

#*** Preparing the data ***#
data("acq")

# As this data comes from news articles, it contains some weird characters, such
# as "<", ">", etc... Those characers can interfeer with the analysis. So, we 
# will remove them.
# Using the functions "tm_map", "content_transformer", and "gsub":
# acq_clean <- ?(acq, ?(?), 
#                pattern = "([^A-Za-z0-9 ])+", 
#                replacement = "")

#*** Document-Term Matrix ***#
# In our data, there are some terms that appear too often or too little. This can
# cause trouble when performing the analysis. So, we will remove them.

# Create a variable "n_documents" where you store the number of documents in the 
# corpus:
# ?=?

# Now, create a variable "minDocFreq" where you will store the minimum percentage  
# of sparse terms you want to ignore: n_document*percentage
# minDocFreq <- ?*?

# Now, create a variable "maxDocFreq" where you will store the maximum percentage 
# of sparse terms you want to ignore: n_document*percentage
# maxDocFreq <- ?*?

# Finally, we will use the function "DocumentTermMatrix" to create the 
# Term-Document Matrix. For this, you need to pass the parameter "control" with 
# the variables:
# stopwords = FALSE
# bounds = list(global = c(minDocFreq, maxDocFreq))
# acq_dtm=DocumentTermMatrix(acq_clean, control = ?)

# Finally, save the data as a sparse matrix:
dtm_lda <- Matrix::Matrix(as.matrix(acq_dtm), sparse = T)

#*** 3.3 Latent Dirichlet allocation ***#
# The burn-in iteration means that we only collecting samples starting from 
# iteration of 4000, since the earlier iteration is still unstable and may not 
# reflect the actual distribution of the data.

# Create the variable "n_topics" to store the number of topics (clusters) you 
# want to explore:
# ?=?

# Use the function "FitLdaModel" to fit the LDA topic model:
# lda_news <-  ?(dtm = dtm_lda, 
#                          k = n_topics, 
#                          iterations = 5000,
#                          burnin = 4000, 
#                          calc_coherence = T)

# What does each parameter do?

#*** Checking theta ***#
# If a term has a high value of theta, it has a high probability of that term 
# being generated from that topic. This also indicates that the term has a high 
# association toward a certain topic.
lda_news$theta %>% 
  head() %>% 
  as.data.frame() %>% 
  set_names(paste("Topic", 1:n_topics)) %>% 
  rownames_to_column("document")  

#*** Checking phi ***#
# Remember that LDA assumes that a topic is a mixture of words. The posterior 
# probability for per-topic-per-word assignment is represented by the phi value. 
# The sum of all phi for a topic is 1.
lda_news$phi %>% 
  rowSums()

# To get the top 10 terms for each topic, we can use the GetTopTerms function.
# news_word_topic <- ?(lda_news$phi, ?) %>%
#   as.data.frame() %>% 
#   set_names(paste("Topic", 1:n_topics))

cloud=news_word_topic %>% 
  rownames_to_column("id") %>%
  mutate(id = as.numeric(id)) %>% 
  pivot_longer(-id, names_to = "topic", values_to = "term") %>% 
  ggplot(aes(label = term, size = rev(id), color = topic, alpha = rev(id))) +
  geom_text_wordcloud(seed = 123) +
  facet_wrap(~topic, scales = "free") +
  scale_alpha_continuous(range = c(0.4, 1)) +
  # scale_color_manual(values = c( "dodgerblue4", "firebrick4", "darkgreen")) +
  theme_minimal() +
  theme(strip.text.x = element_text(colour = "white"))

# Use ggsave to save the graph:
# ggsave("<link to your folder>/topics_cloud.png",
#        plot=cloud,width = 30,height = 20,units = "cm")
