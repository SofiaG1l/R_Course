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

# How would you examine the data?
# ?(JA_BOOKS_org)

# What is the structure of the data?

# How would you initially process the data?
# Hint: Extract and count the words per book:
# JA_BOOKS=JA_BOOKS_org%>%
#   ?(word, text) %>%
#   count(?)

#### 2.2 Word and document frequency ####

#*** tf-idf ***#

# Now let's calculate the tf-idf. For this, you will use the function
# bind_tf_idf:
# JA_BOOKS=JA_BOOKS%>%?

# How would you visualize the results?
# Hint: You can use the following functions to split and arrange the plots per book:
#       * fct_reorder
#       * facet_wrap

# JA_BOOKS%>%
#   group_by(book) %>%
#   slice_max(tf_idf, n = 15) %>%
#   ungroup() %>%
#   ggplot????


#### 2.3 Relationships between words ####

#*** n-grams ***#

# To extract the bigrams (i.e. 2-grams), you need to use the function
# unnest_tokens, but with different parameters. Which parameters are those?

# JA_BOOKS_2gram <- JA_BOOKS_org %>%
#   unnest_tokens(?) 

# Are all the rows useful? What would you do to keep only the useful rows?

# JA_BOOKS_2gram=JA_BOOKS_2gram%>%
#   filter(?)

# Now let's count their frequency

# First, let's remove those bigrams where there is a Stop_Word. For this,
# you need to use the function separate. What parameters would you use?
# bigrams_separated <- JA_BOOKS_2gram %>%
#   separate(?, sep = " ")

# Now, you can filter out the  bigrams where there are stop_words:
# bigrams_filtered <- bigrams_separated %>%
#   filter(?) 

# Now, you can count the bigrams:
# bigram_counts <- bigrams_filtered %>% 
#   group_by(book)%>%
#   count(?)


#*** tf-idf for n-grams ***#

# First, you need to unite the terms into a single word
# bigram_counts=bigram_counts%>%
#   unite(?, sep = " ")

# Second, you can calculate the tf-idf
# bigram_counts=bigram_counts%>%?

# Now we cant plot the frequencies:
# bigram_counts%>%
#   group_by(book) %>%
#   slice_max(tf_idf, n = 15) %>%
#   ungroup() %>%
#   ggplot???

#*** Pairwise correlation ***#

# install.packages("widyr")

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
# austen_section_words=austen_section_words%>%
#   ?(word, text)

# And remove those that are Stop Words
# austen_section_words=austen_section_words%>%
#   filter(?)

# To decrease the amount of computational resources used,
# we still need to filter for at least relatively common words.
word_pairs <- austen_section_words %>%
  group_by(word) %>%
  # What is the following line doing?
  filter(n() >= 20)

# Now, we can calculate the correlations:
# word_pairs=word_pairs%>%
#   widyr::pairwise_cor(?)

#*** Visualizing a network of bigrams with ggraph ***#

# install.packages("igraph")
# install.packages("ggraph")

library(ggraph)

# Let's visualize the network with edges representing the bigram tf-idf.
# For this, we first turn the data frame into a network.

# To make the graph more readable. We will keep only 1% of the bigrams:
# bigram_graph <- bigram_counts %>%
#   filter(book=="Pride & Prejudice") %>%
#   ungroup()%>%
#   slice_max(tf_idf,prop = ?)

# Now we will create two columns with each bigram word :
# bigram_graph=bigram_graph%>%
#   separate(?, sep = " ")

# Finally, we will format the dataframe in the way igraph::graph_from_data_frame
# requires it: first three columns represent source, target, edge.
# bigram_graph=bigram_graph%>%
#   relocate(?)%>%
#   igraph::graph_from_data_frame()

# ggraph has a random component. Therefore, to make it reproducible, it is 
# necessary to use the function set.seed(#), where # is any integer. 
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

# correlations_graph <- ??

ggraph(correlations_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

