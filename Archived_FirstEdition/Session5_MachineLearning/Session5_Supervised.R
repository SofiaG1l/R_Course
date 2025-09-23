## Code written by Sofia Gil-Clavel for "Session 5: R-Workshop Statistical Learning"
## May 2nd, 2025.

# Cleaning the environment
rm(list = ls())
gc()

# Installing the packages
# For section 1 to 2:
# installed.packages("tidyverse")
# installed.packages("corrplot")
# installed.packages(MASS)
# For section 4:
# installed.packages(tidymodels)
# installed.packages(textrecipes)
# installed.packages(discrim)
# installed.packages(naivebayes)

# Opening the packages
library(tidyverse)

# Turn the data into a Tibble
# OJ=?(OJ)

# Use the functions levels and contrast to check the reference category:
# ?(OJ?Purchase)
# ?(OJ?Purchase)

# We know that the following values correspond to the probability of the individual 
# purchasing MM, rather than CH, because the contrasts() function indicates that 
# R has created a dummy variable with a 1 for MM.

# We have some variables that depend on each other
# So, we need to remove them:
OJ=OJ%>%
  select(-PriceDiff,-ListPriceDiff,-STORE,-SalePriceCH,-SalePriceMM,-Store7)


#### 2. Classification ####

#*** Logistic regression ***#

# Train the model with the data
# glm.fits <- glm( ? ~ ?, data = ?, family = binomial)

# You can use the function "summary" to check the different stats:
?(glm.fits)

# The function "coef" returns the coefficients:
?(glm.fits)

# So, if we want to calculate the predicted value of y, then we multiply the 
# coefficients with the values of X.
# Let's do it for the first row:
# We add a 1 at the beginning to account for the intercept.
X=c(1,as.vector(t(OJ[1,-1])))
Y=OJ[1,1]

# Then we add them up:
param=sum(coef(glm.fits)*X)

# And turn them into probabilities
# prob=?

# The predict() function can be used to predict the probability that people 
# would buy MM, given values of the predictors. The type = "response" option tells 
# R to output probabilities of the form P (Y = 1|X), as opposed to other 
# information such as the logit.

# We use the function "predict" to predict the variable purchase for the whole data
# glm.probs <- ?(glm.fits,OJ, type = "response")

# In order to make a prediction as to whether someone would purchase CH or MM on a 
# particular week, we must convert these predicted probabilities into class 
# labels, CH or MM. The following two commands create a vector of class 
# predictions based on whether the predicted probability of a person is greater 
# than or less than 0.5.

glm.pred <- rep("CH", nrow(OJ)) 
# glm.pred[glm.probs > .?] = ?

# Given these predictions, the table() function table()  can be used to produce 
# a confusion matrix in order to determine how many observations were correctly 
# or incorrectly classified.

table(glm.pred, OJ$Purchase)

# The mean() function can be used to compute the fraction of days for which the 
# prediction was correct.
mean(glm.pred==OJ$Purchase)

#*** Linear discriminant analysis ***#

library(MASS)

# Use the function "lda" to perform linear discriminant analysis:
# lda.fit <- ?(formula=?, data = ?)

# Obtain the predictions:
# lda.pred <- ?(lda.fit, OJ)

# This model returns more values:
names(lda.pred)

# class, contains LDA’s predictions about the people's purchase
# lda.class <- lda.pred$?

# Obtain the confusion matrix:
# table(?, ?)

# Obtain the accuracy:
# mean(?)

#### 3.2 Assessing the results ####

#*** Train and Test Data ***#
# Creating a row ID
# OJ=OJ%>%
#   # Create a new column called "ID" with the row_number
#   mutate(?)

# Save 30% of the data in a new variable called "OJ_test".
# Hint: Use the function "sample_frac"
# ?

# Save the other 70% of the data in a new variable called "OJ_train".
# Hint: Use the functions "filter", "!", and "%in%"
# ?

# How would you check if the splitting worked?
# ?+?
nrow(OJ)

# Removing the ID
OJ_test=OJ_test%>%
  dplyr::select(-ID)

OJ_train=OJ_train%>%
  dplyr::select(-ID)

#*** Accuracy ***#

# First, let's build a function to calculate the accuracy:
# In each "#?" describe what the next line of code is doing:
Accuracy=function(model,data_train,data_test,type="lda",...){
  # ?
  fits <- model(Purchase ~ ., data=data_train,...)
  # ?
  probs <- predict(fits,data_test)
  # ?
  if(type=="lda"){
    # ?
    return(mean(probs$class==data_test$Purchase))
  }else{
    # ?
    pred <- rep("CH", nrow(data_test)) 
    pred[probs > .5] = "MM"
    # ?
    return(mean(pred==data_test$Purchase))
  }
}

#*** Cross Validation ***#

# Now, let's write a function to Cross Validate:
# In each "#?" describe what the next line of code is doing:
CroosValidation=function(model,data,k,type="lda",...){
  # ?
  data=data%>%
    mutate(ID=row_number())
  # ?
  k_groups=split(data$ID, cut(seq_along(data$ID),
                              k,labels = FALSE))
  # ?
  MSE=rep(0,k)
  # ?
  for (kk in 1:k) {
    # ?
    data_val=data%>%
      filter(ID%in%k_groups[[kk]])%>%
      dplyr::select(-ID)
    # ?
    data_train=data%>%
      filter(!ID%in%k_groups[[kk]])%>%
      dplyr::select(-ID)
    # ?
    MSE[kk]=Accuracy(model,data_train,data_val,type,...)
  }  
  # ?
  return(MSE)
}

# Use the CrossValidation function to cross validate the train data with 20 groups:
# glm.MSE=CroosValidation(?,?,?,type = "glm",family = binomial)
# lda.MSE=CroosValidation(?,?,?,type = "lda")

# So, which model works better?
library(ggplot2)

# Let's plot them to see their variability:
# Save the results as a dataframe with two columns: value and model.
# glm.MSE=?
# lda.MSE=?

# Bind the data by row:
# MSE=?(glm.MSE,lda.MSE)

# Plot the results using ggplot:
MSE%>%
  ggplot(aes(value,model,fill = model))+
  geom_boxplot()+
  theme_minimal()+
  coord_flip()

#### Let's check the accuracy of each model

# Use the "Accuracy" to test the results:
# Accuracy(?,?,?,type = "glm",family = binomial)
# Accuracy(?,?,?,type = "lda")

# Which model would you use?

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




