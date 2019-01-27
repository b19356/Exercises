# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))

install.packages(c("cluster", "rattle.data", "NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

View(wine)

glimpse(wine)

levels(wine$Type)

class(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

# When variables vary in range, you want to standardize before clustering

df <- scale(wine[-1]) 

View(df)

# Scaling converts the original wine data frame into a matrix

glimpse(df)

class(df)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(df)

# The plot shows a steep drop between 1-3, but then levels off after 4

# So 3 clusters makes sense

# Exercise 2:
#   * How many clusters does this method suggest?
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)

nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")

glimpse(nc)

nc$Best.nc

View(nc$Best.nc)

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

# contingency table shows that 3 clusters had 15 "votes"

table(nc$Best.n[1,])

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

# fit.km <- kmeans( ... )

fit.km <- kmeans(df, centers = 3, nstart = 25)

glimpse(fit.km)

fit.km$size

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

wine$Type

fit.km$cluster

table(wine$Type, fit.km$cluster)

# Yes - actual vs. model looks pretty good.
# confusion matrix shows that model only "misassigned" 6 cases


# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

#clusplot( ... )

clusplot(df, fit.km$cluster)