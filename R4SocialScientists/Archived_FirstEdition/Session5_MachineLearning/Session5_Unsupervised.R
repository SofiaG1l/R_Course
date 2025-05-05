## Code written by Sofia Gil-Clavel for "Session 5: R-Workshop Statistical Learning"
## April 25th, 2025.

# Cleaning the environment
rm(list = ls())
gc()

# Installing all the packages
# For section 1 and 2:
# installed.packages("tidyverse")
# installed.packages("ISLR2")
# installed.packages("corrplot")
# installed.packages("scatterplot3d")
# For section 3:
# installed.packages(tidytext)
# installed.packages(textmineR)
# installed.packages(ggwordcloud)

# Opening the packages
library(tidyverse)
library(ISLR2)

# Bikeshare$mnth=as.integer(Bikeshare$mnth)
# Bikeshare$weathersit=as.integer(Bikeshare$weathersit)
# Bikeshare$hr=as.integer(Bikeshare$hr)

#### 2.2 Principal Components Analysis (PCA) ####

#*** The "Bikeshare" data ***#

# Turn the Bikeshare data into a tibble
# Bikeshare=?

# How would you check what variables are categorical/factors?
# Hint: Use the functions class and names
# ?

# Now, remove the categorical (factors) variables
# Bikeshare=Bikeshare%>%
#   select(?)

#*** Checking the correlations ***#
# Open the package "corrplot"
# ?(corrplot)

# Use the gunction cor to calculate the correlations
# Bikeshare_cor<-?(Bikeshare)

# To visualize the correlations, use the function "corrplot" with the 
# parameter method="circle"
# ?(Bikeshare_cor, ?)

#*** Before applying PCA... ***#
# To get good results using PCA, first we need to now whether we need to 
# standardize the variables. For this, we will check the variables' means 
# and variance.
# Analyzing the means
apply(Bikeshare, 2, mean)

# Analyzing the variance
apply(Bikeshare, 2, var)

#*** Performing PCA ***#
# As we see, the variables have very different means and variances. This means
# that the variables are not comparable. So, we need to standardize them, i.e.,
# to have mean zero and standard deviation one. For this, we can either do it 
# by hand or use the parameter scale=TRUE in the "prcomp" function.

# Use the function "prcomp" to calculate the principal components. Remember to 
# standardize the variables.
# pr.out=?(Bikeshare , ?)

# By default, the prcomp() function centers the variables to have mean zero. 
# By using the option scale = TRUE, we scale the variables to have standard 
# deviation one.

# The output from prcomp() contains a number of useful quantities:
names(pr.out)

# The center and scale components correspond to the means and standard deviations 
# of the variables that were used for scaling prior to implementing PCA.
pr.out$center
pr.out$scale

# The rotation matrix provides the principal component loadings; each column of 
# pr.out$rotation contains the corresponding principal component loading vector.
View(pr.out$rotation)

# Using the prcomp() function, we do not need to explicitly multiply the data by 
# the principal component loading vectors in order to obtain the principal 
# component score vectors. Rather the matrix "x" has as its columns the 
# principal component score vectors. That is, the kth column is the kth principal 
# component score vector:
View(pr.out$x)

# As you can see, the dimensions of the original data "Bikeshare" and the 
# principal components are the same.
dim(Bikeshare)
dim(pr.out$x)

#*** How much variability is explained by each principal component? ***#
# The prcomp() function also outputs the standard deviation of each principal 
# component.
pr.out$sdev

# The variance explained by each principal component is obtained by squaring 
# these:
pr.var <- pr.out$sdev^2

# To compute the proportion of variance explained by each principal component, 
# we simply divide the variance explained by each principal component by the 
# total variance explained by all four principal components:
pve <- pr.var / sum(pr.var)

round(pve*100)

#*** Visualizing the Principal Components ***#
# Use the function "biplot" to visualize the first two principal components.
# ?(pr.out)

# The scale = 0 argument to biplot() ensures that the arrows are scaled to 
# biplot()  represent the loadings; other values for scale give slightly 
# different biplots with different interpretations.

# ?(pr.out, ?)

# Add the parameter xlabs=rep("", nrow(Bikeshare)) to keep only the arrows:
# ?(pr.out, ?, ?)

# How would you explain the arrows?

#*** What variable belongs to each component? ***#

Bikeshare_PCA_cor<-cor(pr.out$x,Bikeshare)
corrplot(Bikeshare_PCA_cor, method="circle")


#### K-Means ####
# For this section, we will use those principal components that explain much of 
# the data variability. Substitute the "?" with that number:
# PC=tibble(data.frame(pr.out$x[1:?]))

# Visualize different pairs of PC. Can you find something interesting?
PC%>%
  ggplot(aes(PC1,PC3))+
  geom_point()+
  theme_minimal()

#*** Applying K-means ***#
# To run the kmeans() function in R with multiple initial cluster assignments, 
# we use the nstart argument. If a value of nstart greater than one is used, 
# then K-means clustering will be performed using multiple random assignments 
# in Step 1 of Algorithm 12.2, and the kmeans() function will report only the 
# best results.
# PC <- PC%>%
#   # Choose the principal components you want to use!
#   select(?,?,?)

# Use the function "kmeans" together with the number of clusters you want to check:
# km.out <- ?(PC,?)

#*** Visualizing Clustering ***#
# To visualize the clusters as categories, we need to save that variable in our 
# data and turn it into a category (or factor).
# How would you do that?
# PC?KM=?(km.out$cluster)

# Now, we can visualize the data using ggplot:
PC%>%
  ggplot(aes(PC1,PC3,color=KM))+
  geom_point()+
  theme_minimal()

# What is happening? Are the clusters wrong?

# Now let us visualize the 3D plot:
scatterplot3d::scatterplot3d(x = PC$PC1, y = PC$PC2, z = PC$PC3,
              color =  PC$KM, angle=60)

# Could there be more groups?
# Play around with the number of clusters:
# km.out <- kmeans(PC,?, nstart = 100)
scatterplot3d::scatterplot3d(x = PC$PC1, y = PC$PC2, z = PC$PC3,
              color =  km.out$cluster, angle=60)

#*** Let's check the original data ***#
# Add the variable cluster to the original data:
# Bikeshare?KM=?

# Are there some patterns?
# Sample=Bikeshare%>%
#   group_by(KM)%>%
#   # Sample 5 rows from each cluster, use the function "sample_n"
#   ?(?)%>%
#   # Arrange the data by cluster, use the function "arrange"
#   ?(KM)

View(Sample)

# What makes each group similar?

#*** Creating a nicer plot ***#
# The following function has as parameters the data and the cluster to filter 
# the data. Then it calculates the correlations only using that sub sample.
Data_Cor_Group=function(DATA,CLUSTER){
  Bike_cor<-data.frame(cor(DATA%>%filter(KM==CLUSTER)%>%select(-KM)))
  Bike_cor$KM=CLUSTER
  Bike_cor$ROWS=row.names(Bike_cor)
  Bike_cor=Bike_cor%>%
    gather("COLUMNS","cor",-KM,-ROWS)
  return(Bike_cor)
}

# Use the "Data_Cor_Group" function on your data and each cluster:
# Bike_cor1=?(DATA=?,CLUSTER=?)
# Bike_cor2=?(DATA=?,CLUSTER=?)
# Repeat as many time as necesary 

# Bind the data by row with the function "rbind"
# Bike_corAll=?(Bike_cor1,Bike_cor2,?,?)

# Now, you can reproduce the final graph:
Bike_corAll%>%
  # mutate(cor=ifelse(COLUMNS==ROWS,0,cor))%>%
  mutate(circle=abs(cor))%>%
  ggplot(aes(ROWS,COLUMNS,fill=cor,size=circle))+
  geom_point(shape = 21, stroke = 0) +
  scale_radius(range = c(0,5)) +
  scale_fill_distiller(palette = "RdYlBu",direction = 1,limits=c(-1,1))+
  theme_light() +
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text.x = element_text(angle=90,hjust = 1),
        axis.title = element_blank()) +
  labs(fill = "Correlation", x = NULL, y = NULL)+
  guides(size = "none")+
  scale_x_discrete(limits = sort(unique(Bike_corAll$ROWS),decreasing = TRUE))+
  # Remove extra whitespace from y-axis so lines are against the axis
  scale_y_discrete(expand = c(0.05,0.05)) +
  # Add straight lines at each factor level, shifted left/down so they're between values
  geom_hline(yintercept = 2:length(unique(Bike_corAll$ROWS)) - 0.5) +
  geom_vline(xintercept = 2:length(unique(Bike_corAll$COLUMNS)) - 0.5)+
  facet_wrap(KM~.)

# To save the graph in your computer, you can use the function "ggsave":
# ggsave("<REPLACE WITH YOU FOLDER LOCATION>/Correlations.png",
#        units = "cm",width = 20,height = 15)

#### Topic Modeling ####
library(tidytext)
library(textmineR)
library(ggwordcloud)
library(tm)

#*** Preparing the data ***#
data("acq")

# As this data comes from news articles, it contains some weird characters, such
# as "<", ">", etc... Those characers can interfeer with the analysis. So, we 
# will remove them.
# Using the functions "tm_map", "content_transformer", and "gsub":
# acq_clean <- ?(acq, ?(?), 
#                pattern = "([^A-Za-z0-9 ])+", 
#                replacement = "")

#*** Document-Term Matrix ***#
# In our data, there are some terms that appear too often or too little. This can
# cause trouble when performing the analysis. So, we will remove them.

# Create a variable "n_documents" where you store the number of documents in the 
# corpus:
# ?=?

# Now, create a variable "minDocFreq" where you will store the minimum percentage  
# of sparse terms you want to ignore: n_document*percentage
# minDocFreq <- ?*?

# Now, create a variable "maxDocFreq" where you will store the maximum percentage 
# of sparse terms you want to ignore: n_document*percentage
# maxDocFreq <- ?*?

# Finally, we will use the function "DocumentTermMatrix" to create the 
# Term-Document Matrix. For this, you need to pass the parameter "control" with 
# the variables:
# stopwords = FALSE
# bounds = list(global = c(minDocFreq, maxDocFreq))
# acq_dtm=DocumentTermMatrix(acq_clean, control = ?)

# Finally, save the data as a sparse matrix:
dtm_lda <- Matrix::Matrix(as.matrix(acq_dtm), sparse = T)

#*** 3.3 Latent Dirichlet allocation ***#
# The burn-in iteration means that we only collecting samples starting from 
# iteration of 4000, since the earlier iteration is still unstable and may not 
# reflect the actual distribution of the data.

# Create the variable "n_topics" to store the number of topics (clusters) you 
# want to explore:
# ?=?

# Use the function "FitLdaModel" to fit the LDA topic model:
# lda_news <-  ?(dtm = dtm_lda, 
#                          k = n_topics, 
#                          iterations = 5000,
#                          burnin = 4000, 
#                          calc_coherence = T)

# What does each parameter do?

#*** Checking theta ***#
# If a term has a high value of theta, it has a high probability of that term 
# being generated from that topic. This also indicates that the term has a high 
# association toward a certain topic.
lda_news$theta %>% 
  head() %>% 
  as.data.frame() %>% 
  set_names(paste("Topic", 1:n_topics)) %>% 
  rownames_to_column("document")  

#*** Checking phi ***#
# Remember that LDA assumes that a topic is a mixture of words. The posterior 
# probability for per-topic-per-word assignment is represented by the phi value. 
# The sum of all phi for a topic is 1.
lda_news$phi %>% 
  rowSums()

# To get the top 10 terms for each topic, we can use the GetTopTerms function.
# news_word_topic <- ?(lda_news$phi, ?) %>%
#   as.data.frame() %>% 
#   set_names(paste("Topic", 1:n_topics))

cloud=news_word_topic %>% 
  rownames_to_column("id") %>%
  mutate(id = as.numeric(id)) %>% 
  pivot_longer(-id, names_to = "topic", values_to = "term") %>% 
  ggplot(aes(label = term, size = rev(id), color = topic, alpha = rev(id))) +
  geom_text_wordcloud(seed = 123) +
  facet_wrap(~topic, scales = "free") +
  scale_alpha_continuous(range = c(0.4, 1)) +
  # scale_color_manual(values = c( "dodgerblue4", "firebrick4", "darkgreen")) +
  theme_minimal() +
  theme(strip.text.x = element_text(colour = "white"))

# Use ggsave to save the graph:
# ggsave("<link to your folder>/topics_cloud.png",
#        plot=cloud,width = 30,height = 20,units = "cm")
