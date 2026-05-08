## Code written by Sofia Gil-Clavel for "Session 4: R-Workshop Text as Data"
## March 25th, 2025.

# Cleaning the environment
rm(list = ls())
gc()

# install.packages("tidytext") 
# install.packages("stringi") 
# install.packages("SnowballC")
# install.packages("hunspell")
# install.packages("ggwordcloud")

# Opening the packages
library(tidyverse)
library(stringi) 
library(tidytext) 

#### 1. Basic text functions ####

TXT="The weather this afternoon feels like one of those endlessly shifting spring days where soft gray clouds drift lazily across the sky between bursts of pale sunlight, a cool breeze carries the faint scent of rain through the streets and trees, and the air hovers in that uncertain balance between warmth and chill that makes people alternately reach for sunglasses, jackets, and umbrellas within the span of a single afternoon."

#### Exercise 1.1 ####

# 1. How many characters are in the text?
# 2. Is the space also considered a character?
# 3. How would you get the number of characters without considering the spaces?
# How would you turn all the characters into lowercase?

#### Exercise 1.2 ####
# 1. How would you save each word in a vector?
# 2. Save the vector in a tibble, remember that the result comes as a list:
# 3. Change the name of the variable to "word".
# 4. Use the tidyverse packages to count the number of times each word appeared in the text
# 5. Arrange the tibble by word frequency in descending order

#### 2. Basic Text Handling ####

## Opening the example text 

# Use the function read.delim() to open the data in data.frame format
# TXT=read.delim(?,header = FALSE)

# Save the data as a tibble
# TXT=?

# Replace the "V1" column name with "text"
colnames(TXT)="text"

# So, let's turn all the encoding to "utf-8". For this,
# you will use the function iconv() and the parameters 
# from = "latin1", to = "UTF-8"

# TXT=TXT%>%
#   mutate(text_utf8=?)

# What if we want to replace NON-ASCII characters with ASCII?

# From stringi we only use the this function stri_trans_general()
# TXT=TXT%>%
#   mutate(text_ASCII=stringi::?(str = text_utf8, id = "ASCII"))


#### 3. Tidy Text ####

#*** Text Data ***#
# Let's add more information about the data file.
# Add the file name and the paragraph number (hint: row_number())

# TXT=TXT%>%
#   mutate(file=?,paragraph=?)

# Now turn text_ASCII into the default text column and delete the unused columns:
# The default column needs to be called "text".

# TXT=TXT%>%
#   mutate(?)%>% 
#   select(?)

#*** Tidy Text ***#
# To extract the words from the text, you need use the function unnest_tokens()
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

TXT=TXT%>%
  mutate(stem_snow = SnowballC::wordStem(words))

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
#   ?(words=?)%>%
#   ?(?)

# Count the words again:
TXT=TXT%>%
  group_by(file,words)%>%
  summarise(total=sum(total,na.rm = TRUE))%>%
  drop_na(words)

# Which words appear the most?
# Are those words useful? 

# Removing the Stop Words.
# The package tidytext comes with a data frame of stopwords
tidytext::stop_words

# There are several options
# * "SMART"    
# * "snowball" 
# * "onix" 
# * Use all of them

# How can you use the stop_words data frame to remove 
# the words that are Stop Words:
# TXT=TXT%>%
#   filter(?)


#### 4. DataViz ####
# Plotting the most frequent words
# 
TXT%>%
  # arrange the data frame in descending order
  # ?(?(total))%>%
  # keep only the first 10 rows
  # slice_head(n=?)%>%
  ggplot(aes(x=reorder(words,total)))+
  # Which geometry would you use to plot the bars
  # ?(aes(weight=total))+
  coord_flip()

# Using word clouds

TXT%>%
  ggplot(aes(label = words, size = total)) +
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
