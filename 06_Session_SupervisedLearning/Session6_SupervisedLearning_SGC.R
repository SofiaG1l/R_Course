## Code written by Sofia Gil-Clavel for "Session 5: R-Workshop Statistical Learning"
## May 2nd, 2025.

# Cleaning the environment
rm(list = ls())
gc()

# Installing the packages
# For section 1 to 2:
install.packages("tidyverse")
install.packages("corrplot")
install.packages("MASS")
install.packages("ISLR2")

# Opening the packages
library(tidyverse)
library(ISLR2)

# Turn the data into a Tibble
OJ=tibble(ISLR2::OJ)

# Use the functions levels and contrast to check the reference category:
levels(OJ$Purchase)
contrasts(OJ$Purchase)

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
glm.fits <- glm( Purchase ~ ., data = OJ, family = binomial)

# You can use the function "summary" to check the different stats:
summary(glm.fits)

# The function "coef" returns the coefficients:
coef(glm.fits)

# So, if we want to calculate the predicted value of y, then we multiply the 
# coefficients with the values of X.
# Let's do it for the first row:
# We add a 1 at the beginning to account for the intercept.
X=c(1,as.vector(t(OJ[1,-1])))
Y=OJ[1,1]

# Then we add them up:
param=sum(coef(glm.fits)*X)

# And turn them into probabilities
prob=exp(param)/(1+exp(param)) # CH

# The predict() function can be used to predict the probability that people 
# would buy MM, given values of the predictors. The type = "response" option tells 
# R to output probabilities of the form P (Y = 1|X), as opposed to other 
# information such as the logit.

# We use the function "predict" to predict the variable purchase for the whole data
glm.probs <- predict(glm.fits,OJ, type = "response")

glm.probs[1]

# In order to make a prediction as to whether someone would purchase CH or MM on a 
# particular week, we must convert these predicted probabilities into class 
# labels, CH or MM. The following two commands create a vector of class 
# predictions based on whether the predicted probability of a person is greater 
# than or less than 0.5.

# Lazy option:
glm.pred <- rep("CH", nrow(OJ)) 
glm.pred[glm.probs > 0.5] = "MM"

# Opt. 1:
glm.pred <- if_else(glm.probs > 0.5, "MM", "CH")

# Opt. 2:
OJ<-OJ%>%
  mutate(glm.pred=if_else(glm.probs > 0.5, "MM", "CH"))

# Given these predictions, the table() function table()  can be used to produce 
# a confusion matrix in order to determine how many observations were correctly 
# or incorrectly classified.

table(glm.pred, OJ$Purchase)

# The mean() function can be used to compute the fraction for which the 
# prediction was correct.
mean(glm.pred==OJ$Purchase)

# Other way to calculate the accuracy:
sum(glm.pred==OJ$Purchase)/nrow(OJ)

#*** Linear discriminant analysis ***#

library(MASS)

# Use the function "lda" to perform linear discriminant analysis:
lda.fit <- lda(formula=Purchase~., data = OJ)

# Obtain the predictions:
lda.pred <- predict(lda.fit, OJ)

# This model returns more values:
names(lda.pred)

# class, contains LDA’s predictions about the people's purchase
lda.class <- lda.pred$class

# Obtain the confusion matrix:
table(lda.class, OJ$Purchase)

# Obtain the accuracy:
mean(lda.class==OJ$Purchase)
mean(glm.pred==OJ$Purchase)

#### 3.2 Assessing the results ####


#*** Train and Test Data ***#
# Creating a row ID
OJ=OJ%>%
  # Create a new column called "ID" with the row_number
  mutate(ID=row_number())

# Save 30% of the data in a new variable called "OJ_test".
# Hint: Use the function "sample_frac"
set.seed(751)
OJ_test=OJ%>%
  sample_frac(0.3)

# Save the other 70% of the data in a new variable called "OJ_train".
# Hint: Use the functions "filter", "!", and "%in%"
OJ_train<-OJ%>%
  filter(!ID%in%OJ_test$ID)

# How would you check if the splitting worked?
nrow(OJ_test)+nrow(OJ_train)
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
  # Getting the model coefficients for either glm or lda.
  # Here we use the training data
  fits <- model(Purchase ~ ., data=data_train,...)
  # Use the model from the previous model to get the predictions.
  # We use the test data.
  probs <- predict(fits,data_test)
  # In case "type" is "lda" then perform the following, Otherwise, use "glm"
  # specification. 
  if(type=="lda"){
    # 
    return(mean(probs$class==data_test$Purchase))
  }else{
    # ?
    pred <- rep("CH", nrow(data_test)) 
    pred[probs > .5] = "MM"
    # ?
    return(mean(pred==data_test$Purchase))
  }
}

Accuracy(model=lda,
         data_train=OJ_train,
         data_test=OJ_test,
         type="lda")

Accuracy(model=glm,
         data_train=OJ_train,
         data_test=OJ_test,
         type="glm",
         family=binomial)

#*** Cross Validation ***#

# Now, let's write a function to Cross Validate:
# In each "#?" describe what the next line of code is doing:
CrossValidation=function(model,data,k,type="lda",...){
  # Create a unique ID for each row.
  data=data%>%
    mutate(ID=row_number())
  # It is spliting the data into k different groups for the 
  # Cross Validation
  k_groups=split(data$ID, cut(seq_along(data$ID),
                              k,labels = FALSE))
  # Creating a vector of size k full of 0
  MSE=rep(0,k)
  # For loop to use the different groups in the data to use as validation data
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
glm.MSE=CrossValidation(glm,OJ_train,20,type = "glm",family = binomial)
lda.MSE=CrossValidation(lda,OJ_train,20,type = "lda")

# So, which model works better?
library(ggplot2)

# Let's plot them to see their variability:
# Save the results as a dataframe with two columns: value and model.
glm.MSE=data.frame(value=glm.MSE,model="glm")
lda.MSE=data.frame(value=lda.MSE,model="lda")

# Bind the data by row:
MSE=rbind(glm.MSE,lda.MSE)

# Plot the results using ggplot:
MSE%>%
  ggplot(aes(value,model,fill = model))+
  geom_boxplot()+
  theme_minimal()+
  coord_flip()

#### Let's check the accuracy of each model

# Use the "Accuracy" to test the results:
Accuracy(glm,OJ_train,OJ_test,type = "glm",family = binomial)
Accuracy(lda,OJ_train,OJ_test,type = "lda")

# Which model would you use?

