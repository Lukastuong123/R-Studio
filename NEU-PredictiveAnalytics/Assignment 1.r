rm(list=ls())


#Importing the dataset
wbcd <- read.csv("E:/NorthEastern University/Predictive Analytics/M1/Project/wisc_bc_data.csv", stringsAsFactors = FALSE)
summary(wbcd)




#Step 2: DATA CLEANING 
#Drop the id freature 
wbcd <- wbcd[-1]

#Check the diagnosis and recod the diagnosis variable
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

#Closer look at 3 out of 30 freatures 
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#Normalization of all the freatures to only 0 and 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

#Data preperation- creating training and test datasets 
wbcd_train <- wbcd_n[1:469, ]     #Create train set 
wbcd_test <- wbcd_n[470:569, ]    #Create the test set 
wbcd_train_labels <- wbcd[1:469, 1]     #Exclude the the Diagnosis  
wbcd_test_labels <- wbcd[470:569, 1] 



#Step 3: TRAINING THE KNN MODEL 
#install.packages("class")
library(class)

#Use knn function to classify the test data
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)




#Step 4: EVALUATING MODEL PERFORMANCE
#install.packages("gmodels")
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

#Applying Mettrics to check the accuracy of our model 
#install.packages("caret")
library(caret)
confusionMatrix(wbcd_test_pred,wbcd_test_labels)






#Step 5: IMPROVING MODEL PERFORMANCE 
#1- Transformation- z-score standardization 
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                        cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
             prop.chisq=FALSE)

#2- Testing alternative values of k
#k=1
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)
#k=5
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)
#k=11
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)
#k=15
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)
#k=21
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)
#k=27
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)










#------------------------------------------------REPEAT EVERYTHING WITHOUT THE FEATURE SCALING----------------------------------
#Importing the dataset
wbcd <- read.csv("E:/NorthEastern University/Predictive Analytics/M1/Project/wisc_bc_data.csv", stringsAsFactors = FALSE)
summary(wbcd)

#Step 2: DATA CLEANING 
#Drop the id freature 
wbcd <- wbcd[-1]

#Check the diagnosis and recod the diagnosis variable
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

#Closer look at 3 out of 30 freatures 
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#Data preperation- creating training and test datasets 
wbcd_train <- wbcd[1:469, 2:31]     #Create train set 
wbcd_test <- wbcd[470:569,2:31 ]    #Create the test set 
wbcd_train_labels <- wbcd[1:469, 1]     #Exclude the the Diagnosis  
wbcd_test_labels <- wbcd[470:569, 1] 



#Step 3: TRAINING THE KNN MODEL 
#install.packages("class")
library(class)

#Use knn function to classify the test data
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)




#Step 4: EVALUATING MODEL PERFORMANCE
#install.packages("gmodels")
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

#Applying Mettrics to check the accuracy of our model 
#install.packages("caret")
library(caret)
confusionMatrix(wbcd_test_pred,wbcd_test_labels)
#Applying Mettrics to check the accuracy of our model 
#install.packages("caret")
library(caret)
confusionMatrix(wbcd_test_pred,wbcd_test_labels)



 


