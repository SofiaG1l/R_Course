## Code written by Sofia Gil-Clavel for "Session 4: R-Workshop Text as Data"
## March 25th, 2025.

# Cleaning the environment
rm(list = ls())
gc()

# install.packages("spacyr")
# install.packages("textstem")

# Opening the packages
library(tidyverse)
library(spacyr)
library(rsyntax)

#### 3. Quick overview of advance topics ####

# In Spacy there are several model we can choose, for this exercise, we will use:
# en_core_web_sm
# spacy_install("en_core_web_sm")

# Now we can initialize the model:
spacy_initialize(model = "en_core_web_sm")

#*** Natural Language Processing ***#

# First, we need to turn the book into plain text, i.e. no dataframe.

austen_book1=janeaustenr::austen_books() %>%
  filter(book=="Pride & Prejudice")%>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  summarise(text=paste0(text, collapse = " "))

austen_book1=as.character(austen_book1[1,"text"])

# Now, we can use spacy_parse to extract the sentences and the POS:
austen_sentences=spacy_parse(austen_book1, dependency=T)

# Now, we can visualize the dependency tree using:
tokens=austen_sentences%>%
  filter(sentence_id==585)
  
plot_tree(tokens)


#*** Extracting who said what to whom ***#

# These examples are from:
# Welbers, Kasper, Wouter Van Atteveldt, and Jan Kleinnijenhuis. “Extracting 
# Semantic Relations Using Syntax: An R Package for Querying and Reshaping 
# Dependency Trees.” Computational Communication Research 3, no. 2 
# (October 1, 2021): 1–16. https://doi.org/10.5117/CCR2021.2.003.WELB.

speech_verbs = c("say", "state")

source_said = tquery(label = "quote", pos = "VERB", 
                     lemma =  speech_verbs,  
                     children(label = "source", relation = "nsubj"),  
                     children(label = "quote"))

according_to_source = tquery(label = "quote", pos = "VERB",  
                            children(label = "verb", lemma="accord",  
                            children(lemma = "to",children(label = "source"))))

chain = list(source_said, according_to_source)

# Example 1:

tokens = spacy_parse("Trump said that Biden is the dumbest of  all candidates", dependency=T)
tokens = annotate_tqueries(tokens, "quote", chain)
plot_tree(tokens, annotation='quote')

# Example 2:

tokens = spacy_parse("Trump has \"cloaked America in darkness for much too long \", Joe Biden said.", dependency=T)
tokens = annotate_tqueries(tokens, "quote", chain)
plot_tree(tokens, annotation='quote')

# Example 3:

tokens = spacy_parse("If Trump had acted earlier, then according to Biden, he would have saved thousands of lives.", dependency=T)
tokens = annotate_tqueries(tokens, "quote", chain)
plot_tree(tokens, annotation='quote')





