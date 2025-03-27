## Code written by Sofia Gil-Clavel for "Session 4: R-Workshop Text as Data"
## March 25th, 2025.

# Cleaning the environment
rm(list = ls())
gc()

# Opening the packages
library(tidyverse)
library(tidytext)

#### 2. Text Mining & DataViz ####
# installed.packages("janeaustenr")

# Opening Jane Austen books
JA_BOOKS_org=janeaustenr::austen_books()

summary(JA_BOOKS_org)

# What is the structure of the data?
# How would you initialy process the data?

JA_BOOKS=JA_BOOKS_org%>%
  filter(!text=="") %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

# Would it make sense to remove some words based on their frequency?

#### 2.2 Word and document frequency ####

# Now let's calculate the tf-idf
JA_BOOKS=JA_BOOKS%>%
  bind_tf_idf(word, book, n)

# Let's visualize the results

JA_BOOKS%>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)


#### 2.3 Relationships between words ####

JA_BOOKS_2gram <- JA_BOOKS_org %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

# Now let's count their frequency
bigrams_separated <- JA_BOOKS_2gram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Removing bigrams where there are stop_words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  group_by(book)%>%
  count(word1, word2, sort = TRUE)

# Exercise 2.2.A: Redo the analysis with 3-grams


# tf-idf for n-grams

# First, you need to unite the terms into a single word
bigram_counts=bigram_counts%>%
  unite(bigram, word1, word2, sep = " ")

# Second, you can calculate the tf-idf
bigram_counts=bigram_counts%>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_counts%>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# Words correlations

# install.packages("widyr")

# Consider the book “Pride and Prejudice” divided into 10-line sections, 
# as we did (with larger sections) for sentiment analysis in Chapter 2. 
# We may be interested in what words tend to appear within the same section.

austen_section_words <- JA_BOOKS_org %>%
  filter(book=="Pride & Prejudice")%>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

# Pairwise correlation
# To decrease the amount of computational resources used,
# we need to filter for at least relatively common words first.
word_pairs <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  widyr::pairwise_cor(word, section, sort = TRUE)

# Visualizing a network of bigrams with ggraph

# install.packages("igraph")
# install.packages("ggraph")

library(ggraph)

bigram_graph <- bigram_counts %>%
  filter(book=="Pride & Prejudice") %>%
  ungroup()%>%
  slice_max(tf_idf,prop = 0.01)%>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  relocate(word1,word2,tf_idf)%>%
  igraph::graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name),vjust = 1, hjust = 1)+
  theme_minimal()

# More complicated graph

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


# Let's plot the correlations

correlations_graph <- word_pairs %>%
  filter(correlation > .15) %>%
  igraph::graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(correlations_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

