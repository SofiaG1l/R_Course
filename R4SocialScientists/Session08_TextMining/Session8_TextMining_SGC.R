## Code written by Sofia Gil-Clavel for "Session 4: R-Workshop Text as Data"
## March 25th, 2025.

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
JA_BOOKS_org=?(JA_BOOKS_org)

# How would you examine the data?
# ?(JA_BOOKS_org)

# What is the structure of the data?

#### 2.2 Word and document frequency ####

#*** term frequency (tf) ***#

# What are the differences between TF1, TF2, and TF3?

# Raw frequencies
TF1=JA_BOOKS%>%
  spread(book,n,fill = 0)%>%
  mutate(tf1=rowSums(pick(2:7), na.rm = TRUE))

# Standardized frequencies by all words
TF2=JA_BOOKS%>%
  mutate(total= n())%>%
  mutate(n=n/total)%>%
  select(-total)%>%
  spread(book,n,fill = 0)%>%
  mutate(tf2=rowSums(pick(2:7), na.rm = TRUE))%>%
  select(word,tf2)

# Standardized frequencies by total words by book
TF3=JA_BOOKS%>%
  group_by(book)%>%
  mutate(total= n())%>%
  mutate(n=n/total)%>%
  ungroup()%>%
  select(-total)%>%
  spread(book,n,fill = 0)%>%
  mutate(tf3=rowSums(pick(2:7), na.rm = TRUE))%>%
  select(word,tf3)

#*** inverse document frequency (idf) ***#
# Now, we will calculate the idf.
# For this, you need the total number of documents. So, save that number in:

# n_docs=??

# Now, you need to calculate the idf=ln(n_docs/number of docs containing the word)
IDF=JA_BOOKS%>%
  # Use the function ifelse to replace n with 1 if it is bigger than 0 and 0 otherwise
  mutate(n=??)%>%
  # Use the function spread to turn each book into a column with the n values as elements
  spread(?,?,fill = 0)%>%
  # Use the function rowSums together with the function pick to add up the 
  # number of docs containing the word
  mutate(total=?(?, na.rm = TRUE))%>%
  # Finaly, calculate: idf=ln(n_docs/number of docs containing the word)
  mutate(?)

# How does the data look?

# Let's keep only the words and the idf:
IDF2=IDF%>%
  select(word,idf)

#*** tf-idf ***#

# tf-idf by hand:
TF_IDF=IDF2%>%
  # We will merge the data frames with the different TFs and the IDF:
  left_join(TF1)%>%
  left_join(TF2)%>%
  left_join(TF3)%>%
  # Now, we will calculate the tf-idf= tf*idf:
  mutate(tf_idf1=tf1*idf,
         tf_idf2=tf2*idf,
         tf_idf3=tf3*idf)%>%
  # Now, we will arrange the value in descendent order based on the tf-idf:
  arrange(-tf_idf1,-tf_idf2,-tf_idf3)

# Are the results similar? Which tf would you use to calculate the tf-idf?

# Now let's calculate the tf-idf using the function bind_tf_idf
# Which arguments do you need to pass?
# JA_BOOKS=JA_BOOKS%>%
#   ?(?)

# Are the results similar? Which tf would you use to calculate the tf-idf?

# How would you visualize the results?
# Hint: Check the previous session code to visualize frequencies.
# Hint 2: You can use the following functions to split and arrange the plots per book:
#       * fct_reorder
#       * facet_wrap

# ??%>%
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

