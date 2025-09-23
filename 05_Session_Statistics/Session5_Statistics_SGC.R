
## Code written by Sofia Gil-Clavel for "Session 3: R-Workshop"
## February 21st, 2025.


# Cleaning the environment
rm(list = ls())
gc()

# Opening the packages
library(tidyverse)

# Let's turn our data into a tibble:
# ChickWeight<- # Save here the tibble of the data

# Visualizing the data
ChickWeight%>%
  ggplot(aes(Time,weight,color=Diet))+
  geom_point()+
  # geom_smooth(method = lm, alpha = 0.5) +
  facet_wrap(Diet~.)+
  theme_bw()

#### 3. Linear Models ####
#### Basic linear model ####
# First, let's analyse the association between Time & Weight in general:
lm() # How do you need to write the parameters?

# Now, let's save the model in an object
# LM1<-# Save here the first model

  #### Detecting outliers ####
# As the initial graph shows, the outliers are the biggest and smallest numbers
# in Diet 2. So, let's look for them:
ChickWeight%>%
  filter(Diet==2,Time==21)%>%
  filter(weight==max(weight) | weight==min(weight))

# The Chicks' IDs are 21 and 24. So, let's add a color row to highlight it in our plots:
# Outliers= # Save here the vector of outliers

# Now let's create a new column with the specific colors
ChickWeight=ChickWeight%>%
  mutate(color=ifelse(Chick%in%Outliers,999,Diet))

ChickWeight$color

# Visualizing the data
ChickWeight%>%
  ggplot(aes(Time,weight,group = Diet,color=color))+
  geom_point()+
  facet_wrap(Diet~.)+
  theme_bw()+
  theme(legend.position = "none")

# Perfect, let's remove the outliers from the data:
ChickWeight_wO=ChickWeight%>%
  filter(color!=999)

# To check the linear model assumptions:
# par(mfrow = c(2, 2)) # You can activate this to visualize everything 
#                       # in the same plot.
plot() # What do you need to write as a parameter?

  
# Check the coefficients, standard errors, and p-values
# How do you check them?

# To check the confidence intervals:
# How do you check them?

#### Now, let's see the effect that the diets had: ####
# Let's check the reference category:
levels(ChickWeight$Diet)

#### Adding time ####
# The model formula weight~Time+Diet creates a model with different 
# y-intercepts for each species: 

# Let's check the association:
LM2<-lm(weight~Time+Diet,data = ChickWeight_wO)

#### Adding interactions ####
# We can add an interaction, so that the slopes of the regression 
# lines are different for each diet.
LM3<-lm(weight~Time*Diet,data = ChickWeight_wO)

#### ANOVA ####
# So, which model is the best?
# Compare the two:
# anova() # What do you need to write here?

# This statistical test generates a p-value of 0. 
# This implies that there is evidence that the null 
# hypothesis that the interaction term is needed by 
# the model. For this reason, we will conduct further 
# analysis on the model with the interaction term.

#### Predicting ####
# If we needed to estimate the chicks weight at a time that was not observed 
# in the experiment, we could use the predict() method. It takes the model object 
# and a data frame of new values for prediction. For example, the model estimates 
# for times beyond 21 can be computed via:

new_values <- data.frame(Diet=rep(1:4,each=10),
                         Time = 21:30)

new_values$Diet=factor(new_values$Diet)
LM2pr=predict(LM3, new_values)

## But what about the prediction intervals?
LM2pr=predict() # What do you need to write here?

# Can we plot the prediction?
LM2pred=LM2pr$fit%>%
  bind_cols(new_values)%>%
  mutate(weight=fit)%>%
  select(-fit)

ChickWeight_wO%>%
  mutate(lwr=NA,upr=NA)%>%
  select(-Chick,-color)%>%
  ggplot(aes(Time,weight,fill=Diet,color=Diet))+
  geom_smooth(method = lm, alpha = 0.2,se = FALSE) +
  geom_ribbon(data=LM2pred,aes(ymin = lwr, ymax = upr),alpha = 0.2) +
  geom_line(data=LM2pred,aes(Time,weight))+
  facet_wrap(Diet~.)+
  theme_bw()

#### generalized linear models ####
# First let us turn the Titanic data into a tibble
Titanic2=tibble() # What do you need to write here?

# How could you analyse the variable Survived?
# GLM1= #?

