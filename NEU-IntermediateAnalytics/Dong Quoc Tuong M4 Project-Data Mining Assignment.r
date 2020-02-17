rm(list = ls())
install.packages('factoextra')
library(factoextra)
install.packages('NbClust')
library(NbClust)
install.packages(party)
install.packages('fpc')  


my_data <- read.delim("seeds_dataset.txt")
my_data
cbind(summary(my_data))

# Boxplot of MPG by Car Cylinders
par(mfrow=c(1,1))
counts <- table(my_data$Class)
pie(counts, main="Number of seeds divided by Class")


#CLASSIFICATION
#Seeds Data
str(my_data)
my_data$Class <-as.factor(my_data$Class) #Convert the dependent variable to factor

#Split into training and test datasets
set.seed(1234)
ind <- sample(2,nrow(my_data), replace= T, prob=c(0.7,0.3))
my_data.train <- my_data[ind==1,]
my_data.test <- my_data[ind==2,]


#Build a decision tree
library(party)
my_data.formula<- Class~Length.of.Kernelgroove + Asymmetry.coefficient + Width.of.Kernel + Length.of.Kernel + Compactness +Perimeter +Area
my_data.ctree <- ctree(my_data.formula, data=my_data.train)

#plot decision tree
plot(my_data.ctree)

#predict on test data 
pred<- predict (my_data.ctree, newdata=my_data.test)

#Check prediction result 
table(pred,my_data.test$Class)



-------


#CLUSTERING- KMEANS 
set.seed(8953)
my_data2 <- my_data

#remove class IDs
my_data2$Class <- NULL

#K-means clustering 
my_data.kmeans <-kmeans(my_data2,3)

#Check result
table(my_data$Class, my_data.kmeans$cluster)

#Plot clusters and their centers 
par(mfrow=c(2,2))
plot(my_data2[c("Length.of.Kernelgroove", "Asymmetry.coefficient")], col=my_data.kmeans$cluster)
points(my_data.kmeans$centers[, c("Length.of.Kernelgroove", "Asymmetry.coefficient")],col=1:3, pch="*", cex=5)
plot(my_data2[c("Width.of.Kernel", "Length.of.Kernel")], col=my_data.kmeans$cluster)
points(my_data.kmeans$centers[, c("Width.of.Kernel", "Length.of.Kernel")],col=1:3, pch="*", cex=5)
plot(my_data2[c("Compactness", "Perimeter")], col=my_data.kmeans$cluster)
points(my_data.kmeans$centers[, c("Compactness", "Perimeter")],col=1:3, pch="*", cex=5)
plot(my_data2[c("Perimeter","Area")], col=my_data.kmeans$cluster)
points(my_data.kmeans$centers[, c("Perimeter","Area")],col=1:3, pch="*", cex=5)





#CLUSTERING- DENSITYBASED
library(fpc)
my_data2$Class <- NULL      #remove class IDs 

# DBSCAN clustering
ds <- dbscan(my_data2, eps = 0.8, MinPts = 5)

# compare clusters with original class IDs
table(ds$cluster, my_data$Class)

# 1-3: clusters; 0: outliers or noise
par(mfrow=c(1,1))
plotcluster(my_data2, ds$cluster)

