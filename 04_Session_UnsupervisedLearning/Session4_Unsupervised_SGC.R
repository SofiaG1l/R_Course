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

# Opening the packages
library(tidyverse)
library(ISLR2)

Bikeshare2<-tibble(Bikeshare)

# Bikeshare2$mnth=as.integer(Bikeshare2$mnth)
# Bikeshare2$weathersit=as.integer(Bikeshare2$weathersit)
# Bikeshare2$hr=as.integer(Bikeshare2$hr)

#### 2.2 Principal Components Analysis (PCA) ####

#*** The "Bikeshare2" data ***#

# Turn the Bikeshare2 data into a tibble
# Bikeshare2=?

# How would you check what variables are categorical/factors?
# Hint: Use the functions class and names
# ?

# Now, remove the categorical (factors) variables
# Bikeshare2=Bikeshare2%>%
#   select(?)

#*** Checking the correlations ***#
# Open the package "corrplot"
# ?(corrplot)

# Use the gunction cor to calculate the correlations
# Bikeshare2_cor<-?(Bikeshare2)

# To visualize the correlations, use the function "corrplot" with the 
# parameter method="circle"
# ?(Bikeshare2_cor, ?)

#*** Before applying PCA... ***#
# Because it is undesirable for the principal components obtained to depend on 
# an arbitrary choice of scaling, we typically scale each variable to have standard
# deviation one before we perform PCA.

Bikeshare22_STND%>%
  summarise(across(everything(),mean))

Bikeshare22_STND%>%
  summarise(across(everything(),var))

Bikeshare22_STND<-Bikeshare22%>%
  mutate(across(everything(),
                ~ (.x -mean(.x))/sqrt(var(.x))))

#*** Performing PCA ***#
# As we see, the variables have very different means and variances. This means
# that the variables are not comparable. So, we need to standardize them, i.e.,
# to have mean zero and standard deviation one. For this, we can either do it 
# by hand or use the parameter scale=TRUE in the "prcomp" function.

# Use the function "prcomp" to calculate the principal components. Remember to 
# standardize the variables.
# pr.out=?(Bikeshare2 , ?)

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

# As you can see, the dimensions of the original data "Bikeshare2" and the 
# principal components are the same.
dim(Bikeshare2)
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

# Add the parameter xlabs=rep("", nrow(Bikeshare2)) to keep only the arrows:
# ?(pr.out, ?, ?)

# How would you explain the arrows?

#*** What variable belongs to each component? ***#

Bikeshare2_PCA_cor<-cor(pr.out$x,Bikeshare2)
corrplot(Bikeshare2_PCA_cor, method="circle")


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
# Bikeshare2?KM=?

# Are there some patterns?
# Sample=Bikeshare2%>%
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
