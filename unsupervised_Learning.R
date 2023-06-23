#setting the working directory
getwd()
setwd("C:\\Users\\SaiSpandana\\OneDrive\\Desktop\\Unsupervised_Learning_Project")

#Loding packages
library(readr)  #Loading the data
library(dplyr) # library for data manipulation
library(tidyr)
library(GGally)
library(gridExtra)
library(factoextra)
library(FactoMineR)
library(plotly)
library(stringr)
library(corrplot)
library(RColorBrewer)
library(knitr)
library(NbClust)
library(FeatureImpCluster)
library(flexclust)
#devtools::install_github("o1iv3r/FeatureImpCluster")

#Loding the Data
wine_clustering <- read.csv("wine_clustering.csv")
View(wine_clustering)

#Dataset Insights
colnames(wine_clustering)
#Feature of data
head(wine_clustering)

#Dimension of data
dim(wine_clustering)

#Structure of data
str(wine_clustering)

#Summary of data
summary(wine_clustering)

#Checking for the null values
apply(wine_clustering, 2, function(x)sum(is.na(x)))

#EDA (Exploratory Data Analysis) - Univariate
# mean of the features
apply(wine_clustering,2,mean) %>% as.data.frame()
# variance of the features
apply(wine_clustering,2,var)%>%as.data.frame()
#Boxplot of the data
# boxplot of features
boxplot(wine_clustering, xlab="features",ylab="values", 
        main ="Boxplot of features", col = "green",border = "blue",cex.axis=.5)

###Standardizing the Variables (Scaling)
wine_standardized <- select(wine_clustering, c(1:13))
wine_scaled <- as.data.frame(scale(wine_standardized))
head(wine_scaled)
str(wine_scaled)
# mean of the scaled data set
apply(wine_scaled,2,mean)%>%as.data.frame()
# variance of the scaled dataset
apply(wine_scaled,2,var)%>%as.data.frame()
# boxplot of scaled features
boxplot(wine_scaled, xlab="features",ylab="values", main ="Boxplot of features", 
        col = "green",border = "blue",cex.axis=.5)

# function to create histogram and density plot
histf<-function(z){
  feature=str_replace_all(deparse(substitute(z)),"[wine_clustering$]","")
  ggplot(wine_clustering) +
    aes(x = z) +
    geom_histogram(aes(y=..density..), position="identity", 
                   alpha=0.5,bins = 14L, fill = "#497AD2", colour = "blue") +
    geom_density(alpha=0.2, fill = "#4411D2", colour = "#4411D2")+
    labs(x =  paste("Feature: ",feature),y = "No Of Obeservation",
         title = paste("Histogram Plot Of ",feature),
         subtitle = paste("Distribution Of Feature ",feature),
         caption = "wine dataset") +
    theme_grey()
}

# Create a list to store the plots
plots <- list()

# calling function for different features
plots[[1]] <-histf(wine_clustering$Alcohol)
plots[[2]] <-histf(wine_clustering$Malic_Acid)
plots[[3]] <-histf(wine_clustering$Ash)
plots[[4]] <-histf(wine_clustering$Ash_Alcanity)
plots[[5]] <-histf(wine_clustering$Magnesium)
plots[[6]] <-histf(wine_clustering$Total_Phenols)
plots[[7]] <-histf(wine_clustering$Flavanoids)
plots[[8]] <-histf(wine_clustering$Nonflavanoid_Phenols)
plots[[9]] <-histf(wine_clustering$Proanthocyanins)
plots[[10]] <-histf(wine_clustering$Color_Intensity)
plots[[11]] <-histf(wine_clustering$Hue)
plots[[12]] <-histf(wine_clustering$OD280)
plots[[13]] <-histf(wine_clustering$Proline)

# Arrange and display all the plots in a single plot
grid.arrange(grobs = plots, ncol = 4)

### Deeper Exploration - Relationships between the Variables
#Multivariate EDA
#Correlation matrix
ggcorr(wine_clustering, low = "navy", high = "darkred")
Correlation <- cor(wine_clustering)
corrplot(Correlation,order ="hclust", col = brewer.pal(n=8, name = "RdBu"))
corrplot(Correlation, method = "number")
cor(wine_clustering) %>% kable()

#Scatterplot for all features
ggpairs(wine_clustering)

###PCA AND FEATURE EXPLORATION
#principle component
pca.out<-prcomp(wine_scaled)
summary(pca.out)

#Proportion of Variance Explained (PVE) by each principle components
pr_var <- pca.out$sdev^2
pve <- pr_var/sum(pr_var)
summary(pve)
# plot of PVE explained by each principle component
plot(pve, xlab="Principle Component", ylab = "Proportion of variance explained",
     ylim=c(0,1), type="b")

# Cumulative proportion of variance explained
plot(cumsum(pve), xlab="principle component",
     ylab="Cumulative proportion of variance explained", ylim=c(0,1), type="b")

# Biplot
fviz_pca_biplot(pca.out)

###Kmeans clustering
# deciding optimul number of cluster
# Elbow method
fviz_nbclust(wine_scaled, kmeans, method = "wss")+
  geom_vline(xintercept = 3, linetype = 2)+labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(wine_scaled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(wine_scaled, kmeans, nstart = 25, method = "gap_stat", 
             nboot = 50)+labs(subtitle = "Gap statistic method")

#Optimum number of cluster by using NbClust() method
nb_clust <- NbClust(wine_scaled, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")
fviz_nbclust(nb_clust)

# The best number of clusters is 3 
#Now fit the kmeans with 3 clusters
set.seed(123)
km <- kmeans(wine_scaled, 3, nstart = 25)

# visualising k-means result
fviz_cluster(km, data = wine_scaled,
             palette = c( "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())

#Aggregate table by the cluster
# making cluster as factor
km$cluster <- as.factor(km$cluster)
# assining cluster to the original wine data set
data.clust.kmeans <- cbind(wine_clustering, cluster = km$cluster)
# aggregating the feature by cluster
aggar.kmeans <-aggregate(data.clust.kmeans[,1:13], 
                         by=list(data.clust.kmeans$cluster), mean) %>% as.data.frame()
aggar.kmeans%>%kable()

#visuvalizing the clustered data
ggpairs(data.clust.kmeans,aes(color=cluster, alpha=0.5),
                          lower = list(combo = wrap("facethist", binwidth = 0.1)))
#Feature Reduction for better interpretability of the model
set.seed(10)
res <- kcca(wine_scaled,k=3)
FeatureImp_res <- FeatureImpCluster(res,as.data.table(wine_scaled))
plot(FeatureImp_res)

#Kmeans clustering based on reduced features
# making new data framed of reduced features
data.scaled <- as.data.frame(wine_scaled)
# data (with reduced features) containing unscaled values of feature
data.reduced <- wine_clustering[c("Alcohol","Proline","Color_Intensity")]
# data (with reduced feature) containing scaled fratures
data.scaled.reduced <- data.scaled[c("Alcohol","Proline","Color_Intensity")]
# Compute k-means for reduced features with k = 3
set.seed(123)
km_reduced <- kmeans(data.scaled.reduced, 3, nstart = 25)

#Visuvalizing the clusters of reduced features
# visualising k-means result
suppressWarnings(
  fviz_cluster(km_reduced, data = data.scaled.reduced,
               palette = c( "#00AFBB", "#E7B800", "#FC4E07"),
               ellipse.type = "euclid", # Concentration ellipse
               star.plot = TRUE, # Add segments from centroids to items
               repel = TRUE, # Avoid label overplotting (slow)
               ggtheme = theme_minimal())
)

# Aggrigratee table by cluster
# making cluster as factor
km_reduced$cluster <- as.factor(km_reduced$cluster)
# assining cluster to the original wine data set
data.clust.reduced.kmeans <- cbind(data.reduced, cluster = km_reduced$cluster)
# Aggregating the clustered data (reduced feature) by cluster
aggar.reduced.kmeans <-aggregate(data.clust.reduced.kmeans[,1:3], 
                                 by=list(data.clust.reduced.kmeans$cluster), mean) %>% as.data.frame()
aggar.reduced.kmeans %>% kable()

#Visuvalising the clustered data
suppressMessages( ggpairs(data.clust.reduced.kmeans,
                          aes(color=cluster, alpha=0.5),
                          lower = list(combo = wrap("facethist", binwidth = 0.1))))


Cluster <- c("Cluster 1","Cluster 2","Cluster 3")
Alcohol <- c("High","Highest","Lowest")
Proline <- c("Low","Highest","Lowest")
Colour.intensity <- c("Highest","Medium","Lowest")
df<-data.frame(Cluster,Alcohol,Proline,Colour.intensity)
df %>% kable()

