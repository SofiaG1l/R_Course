## Code written by Sofia Gil-Clavel for "Session 8: R-Workshop Text as Data"
## First version: March 25th, 2025.

# Cleaning the environment
rm(list = ls())
gc()

# First, we will install all the packages that we will use during this class:
# install.packages("janeaustenr")
# install.packages("widyr")
# install.packages("igraph")
# install.packages("ggraph")

# We will open the packages when we need them:
library(tidyverse)
library(tidytext)

#### 2. Text Mining & DataViz ####

# Opening Jane Austen books
JA_BOOKS_org=janeaustenr::austen_books()

# Turn the data into a tibble:
# JA_BOOKS_org=?(JA_BOOKS_org)

# How would you examine the data?
# ?(JA_BOOKS_org)

# What is the structure of the data?
# Hint: Use the function unnest_token() and count()

# JA_BOOKS=JA_BOOKS_org%>%
#   filter(!text=="") %>%
#   ?(word, text) %>%
#   ?(book, word, sort = TRUE)

#### 2.2 Word and document frequency ####

# Save the number of book in the variable n_docs.
# Hint: Use the functions length() and unique()
# n_docs=?

# Now let's calculate the tf-idf using the function bind_tf_idf
# Which arguments do you need to pass?
# JA_BOOKS=JA_BOOKS%>%
#   bind_tf_idf(?,?,?)


# How would you visualize the results?
# Hint: The following is a code to visualize frequencies.
# Modify it to plot the tf-idf by book. Fill the bars by book.
# Hint 2: You can use the following functions to split and arrange the plots per book:
#       * fct_reorder(), this one goes inside the aes().
#       * Use facet_wrap()

JA_BOOKS%>%
  group_by(book) %>%
  slice_max(n, n = 15) %>%
  ungroup() %>%
  ggplot(aes(n, reorder(word, n))) +
  geom_col(show.legend = FALSE) +
  labs(x = "n", y = NULL)


#### 2.3 Relationships between words ####

#*** n-grams ***#

# To extract the bigrams (i.e. 2-grams), you need to use the function
# unnest_tokens, but with different parameters. Which parameters are those?

# JA_BOOKS_2gram <- JA_BOOKS_org %>%
#   unnest_tokens(?, ?, token = ?, n = ?) %>%
#   filter(!is.na(bigram))

# Are all the rows useful? What would you do to keep only the useful rows?
# 1) Hint: Stop words

# JA_BOOKS_2gram=JA_BOOKS_org%>%
#   filter(!text=="") %>%
#   mutate(id=row_number())%>%
#   ?(word, text)%>%
#   filter(!?%in%?)

# 2) Hint: Stemming

JA_BOOKS_2gram<-JA_BOOKS_2gram%>%
  mutate(stem_huns = hunspell::hunspell_stem(word))

# We need to unnest the vector values
JA_BOOKS_2gram<-JA_BOOKS_2gram%>%
  unnest_wider(stem_huns, simplify = FALSE,names_sep = "_")

# Replace the column "words" with the result you found most useful, if any.
# And delete the unused columns.

# JA_BOOKS_2gram<-JA_BOOKS_2gram%>%
#   mutate(stem_huns = ifelse(!is.na(?),?,?))%>%
#   # remove the other columns
#   select(-?)

# JA_BOOKS_2gram<-JA_BOOKS_2gram%>%
#   mutate(word = ifelse(!is.na(?),?,?))%>%
#   select(-?)

# Now, we can concatenate the words based on the id:
# Hint: Use group_by() and paste0(collapse = " ")
# JA_BOOKS_2gram<-JA_BOOKS_2gram%>%
#   ?(?,?)%>%
#   ?(text=paste0(?, collapse = " "))%>%
#   ungroup()%>%
#   select(-id)

# Now we can get the bigram, i.e., the 2-gram.
# JA_BOOKS_2gram <- JA_BOOKS_2gram %>%
#   unnest_tokens(?, ?, ? = "ngrams", ? = 2) %>%
#   filter(!is.na(bigram))

# Now let's count their frequency

# bigram_counts<-JA_BOOKS_2gram%>%
#   ?(book)%>%
#   ?(bigram,sort = TRUE)

#*** tf-idf for n-grams ***#

# Second, you can calculate the tf-idf
# bigram_counts=bigram_counts%>%
#   ?(?, ?, ?) %>%
#   arrange(desc(tf_idf))

bigram_counts%>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

#*** Pairwise correlation ***#

# To decrease the amount of resources used by the computer, we will only analyse
# the book "Pride & Prejudice", but we will do it by section.

# We will follow the book "Text Mining with R" and declare that a section is 
# made of 10 lines of text. So, we will use the integer division operator (%%) 
# to obtain the quotients of dividing by 10.

austen_section_words <- JA_BOOKS_org %>%
  filter(book=="Pride & Prejudice")%>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) 

# Now, we can extract the words 
# austen_section_words<-austen_section_words%>%
#   ?(word, text)

# And remove those that are Stop Words
# austen_section_words=austen_section_words%>%
#   ?(!? %in% ?)

# To decrease the amount of computational resources used,
# we still need to filter for at least relatively common words.
# What is the following code doing?
word_corr <- austen_section_words %>%
  group_by(word) %>%
  filter(n()>= 20)

# Now, we can calculate the correlations:
# Use the function pairwise_cor() from the package widyr to get the correlations
# word_corr=word_corr%>%
#   ?(word, section, sort = TRUE)

#*** Visualizing a network of bigrams with ggraph ***#

library(ggraph)

# Let's visualize the network with edges representing the bigram tf-idf.
# For this, we first turn the data frame into a network.

# To make the graph more readable. We will keep only 1% of the bigrams:
bigram_graph <- bigram_counts %>%
  filter(book=="Pride & Prejudice") %>%
  ungroup()%>%
  slice_max(tf_idf,prop = 0.005)

# Now we will create two columns with each bigram word :
bigram_graph=bigram_graph%>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Finally, we will format the dataframe in the way igraph::graph_from_data_frame
# requires it: first three columns represent source, target, edge.
bigram_graph=bigram_graph%>%
  relocate(word1,word2,tf_idf)%>%
  igraph::graph_from_data_frame()

# ggraph has a random component. Therefore, to make it reproducible, it is 
# necessary to use the function set.seed(#), where # is any integer. 
# What does set.seed(2017) do?
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


# Now, it is your turn to plot the correlations:
set.seed(2017)

# correlations_graph <- ?

ggraph(correlations_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

