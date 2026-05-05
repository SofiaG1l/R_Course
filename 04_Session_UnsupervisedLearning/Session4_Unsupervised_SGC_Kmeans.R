## Code written by Sofia Gil-Clavel for "Session 4: R-Workshop Unsupervised Learning"
## First: April 25th, 2025.
## Last Update: March 24th, 2026.

# Cleaning the environment
rm(list = ls())
gc()

# Opening the packages
library(tidyverse)
library(ISLR2)

# Turning the data into a Tibble
Bikeshare2=tibble(Bikeshare)

# Bikeshare2$mnth=as.integer(Bikeshare2$mnth)
# Bikeshare2$weathersit=as.integer(Bikeshare2$weathersit)
# Bikeshare2$hr=as.integer(Bikeshare2$hr)

# Removing categorical data
Bikeshare2=Bikeshare2%>%
  select(-mnth,-weathersit,-hr)

#### Principal Components Analysis (PCA) ####

library(corrplot)
Bikeshare2_cor<-cor(Bikeshare2)
corrplot(Bikeshare2_cor, method="circle")

# Checking the data:
# Analyzing the means
Bikeshare2%>%
  summarise(across(everything(),mean))

# Analyzing the variance
Bikeshare2%>%
  summarise(across(everything(),var))

# Because it is undesirable for the principal components obtained to depend on 
# an arbitrary choice of scaling, we typically scale each variable to have standard
# deviation one before we perform PCA.
Bikeshare2_STND<-Bikeshare2%>%
  mutate(across(everything(),
                ~ (.x -mean(.x))/sqrt(var(.x))))

Bikeshare2_STND%>%
  summarise(across(everything(),mean))

Bikeshare2_STND%>%
  summarise(across(everything(),var))

# As we see, the variables have very different means and variances. This means
# that the variables are not comparable. So, we need to standardize them, i.e.,
# to have mean zero and standard deviation one.

#### Performing PCA ####
# By default, the prcomp() function centers the variables to have mean zero. 
# By using the option scale = TRUE, we scale the variables to have standard 
# deviation one.
pr.out=prcomp(Bikeshare2_STND )

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
# component score vectors. Rather the 50 × 4 matrix x has as its columns the 
# principal component score vectors. That is, the kth column is the kth principal 
# component score vector.
View((pr.out$x))

dim(Bikeshare2_STND)
dim(pr.out$x)

#### Visualizing the Principal Components ####
# The scale = 0 argument to biplot() ensures that the arrows are scaled to 
# biplot()  represent the loadings; other values for scale give slightly 
# different biplots with different interpretations.

biplot(pr.out, scale = 0, xlabs=rep("", nrow(Bikeshare2_STND)))

# How would you explain the arrows?

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

# Or you can just do:
summary(pr.out)

#### What variable belongs to each component? ####

Bikeshare2_PCA_cor<-cor(pr.out$x,Bikeshare2_STND)
corrplot(Bikeshare2_PCA_cor, method="circle")


#### K-Means ####

PC=tibble(data.frame(pr.out$x))

# Visualize different pairs of PC. Can you find something interesting?
PC%>%
  ggplot(aes(PC1,PC3))+
  geom_point()+
  theme_minimal()

# We are going to be playing around with PC, so let's save the original
# data into PC2
PC_2=PC

#### Applying K-means ####
# To run the kmeans() function in R with multiple initial cluster assignments, 
# we use the nstart argument. If a value of nstart greater than one is used, 
# then K-means clustering will be performed using multiple random assignments 
# at the beginning, and the kmeans() function will report only the 
# best results.
km.out <- PC%>%
  # Choose the principal components you want to use!
  select(PC1,PC3)%>%
  # Choose the number of clusters you want to check:
  kmeans(3, nstart = 100)

#### Visualizing Clustering ####

# Save the factor of the k-means' cluster as a factor variable:
PC$KM=factor(km.out$cluster)

# Now let's visualize the PCs and color them
PC%>%
  ggplot(aes(PC1,PC3,color=KM))+
  geom_point()+
  theme_minimal()

# What is happening? Are the clusters wrong?

# Now let us visualize the 3D plot:
library(scatterplot3d)
# As scatterplot3d is not a ggplot2 function, then we need to pass
# each column in the dataframe as a vector:
scatterplot3d(x = PC$PC1, y = PC$PC2, z = PC$PC3,
              color =  PC$KM, angle=60)

# Could there be more groups?
km.out <- kmeans(PC, 4, nstart = 100)
scatterplot3d(x = PC$PC1, y = PC$PC2, z = PC$PC3,
              color =  km.out$cluster, angle=60)

# Now let's replace KM with the new clusters
PC$KM=as.factor(km.out$cluster)

#### Let's check the original data ####

Bikeshare2$KM=km.out$cluster

View(Bikeshare2%>%
  group_by(KM)%>%
  sample_n(5)%>%
  arrange(KM))

# What makes each group similar?

# Creating a nicer plot: 
Bike_cor1<-data.frame(cor(Bikeshare2%>%filter(KM==1)%>%select(-KM)))
Bike_cor1$KM=1
Bike_cor1$ROWS=row.names(Bike_cor1)
Bike_cor1=Bike_cor1%>%
  gather("COLUMNS","cor",-KM,-ROWS)

Bike_cor2<-data.frame(cor(Bikeshare2%>%filter(KM==2)%>%select(-KM)))
Bike_cor2$KM=2
Bike_cor2$ROWS=row.names(Bike_cor2)
Bike_cor2=Bike_cor2%>%
  gather("COLUMNS","cor",-KM,-ROWS)

Bike_cor3<-data.frame(cor(Bikeshare2%>%filter(KM==3)%>%select(-KM)))
Bike_cor3$KM=3
Bike_cor3$ROWS=row.names(Bike_cor3)
Bike_cor3=Bike_cor3%>%
  gather("COLUMNS","cor",-KM,-ROWS)

Bike_cor4<-data.frame(cor(Bikeshare2%>%filter(KM==4)%>%select(-KM)))
Bike_cor4$KM=4
Bike_cor4$ROWS=row.names(Bike_cor4)
Bike_cor4=Bike_cor4%>%
  gather("COLUMNS","cor",-KM,-ROWS)

# How could you turn the previous code chunks into a function?

Bike_corAll=rbind(Bike_cor1,Bike_cor2,Bike_cor3,Bike_cor4)

# library(viridis)
Bike_corAll%>%
  ggplot(aes(ROWS,COLUMNS,fill=cor))+
  geom_tile()+
  facet_wrap(KM~.)+
  scale_fill_distiller(palette = "RdYlBu",direction = 1,limits=c(-1,1))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90,hjust = 1),
        axis.title = element_blank())

Bike_corAll%>%
  # filter(COLUMNS!=ROWS)%>%
  # mutate(cor=ifelse(COLUMNS==ROWS,0,cor))%>%
  mutate(circle=abs(cor))%>%
  filter(abs(circle)>0.1,abs(circle)<1)%>%
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
  facet_wrap(KM~.,scales = "free")



# ggsave("Session5/Correlations.png",units = "cm",width = 20,height = 15)


