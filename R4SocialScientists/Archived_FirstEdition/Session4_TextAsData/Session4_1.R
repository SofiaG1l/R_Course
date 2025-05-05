## Code written by Sofia Gil-Clavel for "Session 4: R-Workshop Text as Data"
## March 25th, 2025.

# Cleaning the environment
rm(list = ls())
gc()

# Opening the packages
library(tidyverse)

#### 1.1 Basic Text Handling ####

## Opening the example text 

# Use the function read.delim() to open the data in data.frame format
# TXT=read.delim(?)

# Transform the data into a tibble
# TXT=?

# Replace the "V1" column name with "text"
colnames(TXT)="text"

# So, let's turn all the encoding to "utf-8". For this,
# you will use the function iconv and the parameters 
# from = "latin1", to = "UTF-8"

# TXT=TXT%>%
#   mutate(text_utf8=?)

# What if we want to replace NON-ASCII characters with ASCII?

# For this, will use the package "stringi":
# install.packages("stringi") 

# From stringi we only use the this function stri_trans_general
# TXT=TXT%>%
#   mutate(text_ASCII=stringi::?(str = text_utf8, id = "ASCII"))


#### 1.2 Basic text functions ####

# Install the packages

# install.packages("stringr") 

# Opens the packages
library(stringr) 

# Now that all the text is plain, we can start applying some functions.

# Exercise 1.2.A

# function 1: What does it do?

# function 2: What does it do?

# function 3: What does it do?

#### 1.3 Tidy Text ####

library(tidytext)

#*** Text Data ***#
# Let's add more information about the data file.
# Add the file name and the paragraph number (hint: row_number())

# TXT=TXT%>%
#   mutate(file=?,paragraph=?)

# Now turn text_ASCII into the default text column and delete the unused columns:

# TXT=TXT%>%
#   mutate(?)%>% 
#   select(?)

#*** Tidy Text ***#
# To extract the words from the text, you need use the function unnest_tokens
# What parameters do you need to pass? Where can you check that?

# TXT=TXT%>%
#   unnest_tokens(?)

#*** Summarized Text ***#
# The following lines will reduce the amount of data stored in RAM.
# This way, we will make an efficient use of the memory.

# What are they doing and why is that more efficient?
TXT=TXT%>%
  group_by(file,paragraph,words)%>%
  summarise(total=n())%>%
  spread(paragraph,total)

# Now, we will count the number of times the words appeared in the whole document:
TXT=TXT%>%
  ungroup()%>%
  # What is the following line doing?
  mutate(total=rowSums(pick(3:8), na.rm = TRUE))

# Some words are just conjugations of a verb.
# How can consider those as the same?
# One way is transforming them into their stems.

# install.packages("SnowballC")
TXT=TXT%>%
  mutate(stem_snow = SnowballC::wordStem(words))

# install.packages("hunspell")
TXT=TXT%>%
  mutate(stem_huns = hunspell::hunspell_stem(words))

# We need to unnest the vector values
TXT=TXT%>%
  unnest_wider(stem_huns, simplify = FALSE,names_sep = "_")

# Now, we keep only the stems
TXT=TXT%>%
  mutate(stem_huns = ifelse(!is.na(stem_huns_2),stem_huns_2,stem_huns_1))%>%
  # remove the other columns
  select(-stem_huns_1,-stem_huns_2)

# Which algorithm would you use?
# Is stemming useful?

# Replace the column "words" with the result you found most useful, if any.
# And delete the unused columns.

# TXT=TXT%>%
#   mutate(words=?)%>%
#   select(?)

# Count the words again:
TXT=TXT%>%
  group_by(file,words)%>%
  summarise(total=sum(total,na.rm = TRUE))%>%
  drop_na(words)

# Which words appear the most?
# Are those words useful? 

# Removing the Stop Words
# install.packages("stopwords") 

# There are several options
# * "SMART"    
# * "snowball" 
# * "onix" 
# * Use all of them

# Choose the option you find most convenient:
# STOPWORDS=stopwords::stop_words%>%
#   filter(?)

# Remove the words that are Stop Words
# TXT=TXT%>%
#   filter(?)


#*** Visualizations ***#
# Plotting the most frequent words
TXT%>%
  arrange(desc(total))%>%
  slice_head(n=10)%>%
  ggplot(aes(x=reorder(words,total)))+
  geom_bar(aes(weight=total))+
  coord_flip()

# Using word clouds

TXT%>%
  mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))%>%
  ggplot(aes(label = words, size = total, angle = angle)) +
  ggwordcloud::geom_text_wordcloud_area(rm_outside = TRUE) + # 
  scale_size_area(max_size = 10) +
  theme_minimal()

# We can also plot following certain shapes:

DIR_IMG="<replace with link>/BookIcon.png"
DIR2SAVE="<replace with link>/BookIcon_2.png"

TXT%>%
  mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, 
                             prob = c(1, 1, 4, 1, 1)))%>%
  ggplot(aes(label = words, size = total, angle = angle)) +
  ggwordcloud::geom_text_wordcloud_area(
    mask = png::readPNG(DIR_IMG),
    rm_outside = TRUE,
    area_corr = TRUE) + # 
  scale_size_area(max_size = 10) +
  theme_minimal()

ggsave(DIR2SAVE)
