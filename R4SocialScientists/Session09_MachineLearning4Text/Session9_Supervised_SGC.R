## Code written by Sofia Gil-Clavel for "Session 5: R-Workshop Statistical Learning"
## May 2nd, 2025.

# Cleaning the environment
rm(list = ls())
gc()

# Installing the packages
# installed.packages(tidymodels)
# installed.packages(textrecipes)
# installed.packages(discrim)
# installed.packages(naivebayes)

#### 4. Text Classification ####

# Cleaning the environment
rm(list = ls())
gc()

library(tidymodels)
library(textrecipes)
library(discrim)
library(naivebayes)

#*** Preprocessing the Data ***#

# Opening the data
DIR="<Path to where you stored the data>"
complaints <- read_csv(paste0(DIR,"/complaints.csv.gz"))

# For our first model, let’s build a binary classification model to predict 
# whether a submitted complaint is about “Credit reporting, credit repair 
# services, or other personal consumer reports” or not.

unique(complaints$product)

set.seed(1234)
complaints2class <- complaints %>%
  mutate(product = factor(if_else(
    product == "Credit reporting, credit repair services, or other personal consumer reports",
    "Credit", "Other")))

# Use the function "initial_split" to split the data into train and test data.
# Use the variable product as strata:
# complaints_split <- ?(complaints2class,?)

# Why did we use that function?

# Save the data into the variables complaints_train and complaints_test, using
# the functions training and testing respectively.
complaints_train <- training(complaints_split)
complaints_test <- testing(complaints_split)

# Next we need to preprocess this data to prepare it for modeling; we have text 
# data, and we need to build numeric features for machine learning from that text.

# We will use the function "recipe" to specify the model.
# In this case, we will only use the independent variable consumer_complaint_narrative
# to predict the dependent variable product:
# complaints_rec <-recipe(? ~ ?, data = complaints_train)

# Now, we will add steps to process the text of the complaints. For this, we 
# will use textrecipes to handle the consumer_complaint_narrative variable.

# First we tokenize the text to words with step_tokenize().

# complaints_rec <- complaints_rec %>%
#   ?(consumer_complaint_narrative)

# Use step_tokenfilter() to only keep the 1000 most frequent tokens, to avoid 
# creating too many variables in our first model. 
# complaints_rec <- complaints_rec%>%
#   ?(consumer_complaint_narrative, ?)

# To finish, we use step_tfidf() to compute tf-idf.
# complaints_rec <- complaints_rec%>%
#   ?(consumer_complaint_narrative)

# Now that we have a full specification of the preprocessing recipe, we can build 
# up a tidymodels workflow() to bundle together our modeling components.

complaint_wf <- workflow() %>%
  add_recipe(complaints_rec)

#*** Classifying: Naive Model ***#

### Training the model

# Let’s start with a naive Bayes model, which is available in the tidymodels 
# package discrim. One of the main advantages of a naive Bayes model is its ability 
# to handle a large number of features, such as those we deal with when using word 
# count methods.

nb_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes")

# Now we have everything we need to fit our first classification model. We can 
# add the naive Bayes model to our workflow, and then we can fit this workflow 
# to our training data.

# nb_fit <- complaint_wf %>%
#   # Add the model using "add_model"
#   ?(nb_spec) %>%
#   # Train the model using "fit". Which data should you use?
#   ?(data = ?)

### Evaluating the model

# Using Cross Validation
set.seed(234)

# Use the function "vfold_cv" to set up the cross validate. Use 10 groups.
# complaints_folds <- ?(complaints_train, ?)

# Each of these splits contains information about how to create cross-validation 
# folds from the original training data. In this example, 90% of the training data 
# is included in each fold, and the other 10% is held out for evaluation.
complaints_folds

# Now, let's initialize the workflow:
nb_wf <- complaint_wf %>%
  # Now, we just add the model to the previously set-up text processing:
  add_model(nb_spec)

# In the last section, we fit one time to the training data as a whole. Now, to 
# estimate how well that model performs, let’s fit the model many times, once to 
# each of these resampled folds, and then evaluate on the heldout part of each 
# resampled fold.

# Now we use the function "fit_resamples" to start the engine:
# nb_rs <- fit_resamples(
#   object=?,
#   resamples=?,
#   control = control_resamples(save_pred = TRUE)
# )

# We can extract the relevant information using collect_metrics() and 
# collect_predictions()

nb_rs_metrics <- collect_metrics(nb_rs)
nb_rs_predictions <- collect_predictions(nb_rs)

# To evaluate our model, we check the confusion matrix. 
# The function conf_mat_resampled() computes a separate confusion matrix for 
# each resample and takes the average of the cell counts. This allows us to 
# visualize an overall confusion matrix rather than needing to examine each 
# resample individually.

# What would you have to compare?
# table(?,?)

# Does the matrix make sense?

# Let's visualize this:
conf_mat_resampled(nb_rs, tidy = FALSE) %>%
  autoplot(type = "heatmap")

# Let's calculate the accuracy by group:
nb_rs_predictions%>%
  group_by(id)%>%
  summarise(MSE=mean(.pred_class==product))%>%
  ggplot(aes(MSE))+
  geom_boxplot()+
  theme_minimal()+
  coord_flip()




