rm(list=ls())


#IDENTIFYING RISKY BANK LOANS USING C5.0 DECISION TREES 
#STEP 2- exploring and preparing the data 
#Importing the dataset
credit <- read.csv("E:/NorthEastern University/Predictive Analytics/M4/Project/credit.csv", stringsAsFactors = FALSE)
str(credit)

#Have a look at the dataset 
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)

#Data preparation- Create random raing and test datasets 
set.seed(12345)                                  #Used to create random number in a predefined sequnece 
credit_rand <- credit[order(runif(1000)), ]      #Generates a list of 1000 random numbers 
                                                 # order() function returns a vector of numbers indicating the sorted posisiton of the 1000 random numbers 
summary(credit$amount)                           #Comparing the same dataframe sorted differently 
summary(credit_rand$amount)
head(credit$amount)                              #head() function to examine the first few values in each data frame
head(credit_rand$amount)

#split the dataset into train set and test set 
credit_train <- credit_rand[1:900, ]
credit_test  <- credit_rand[901:1000, ]
prop.table (table(credit_train$default))                     
prop.table(table(credit_test$default))



#STEP 3- training a model on the data 
#install.packages("C50")
library(C50)
str(credit_train$default)
credit_model<-C5.0(credit_train[-17],factor(credit_train$default)) #eliminating the default column
credit_model
summary(credit_model)



#STEP 4 - evaluating model performance
credit_pred <- predict(credit_model, credit_test)              #Created a vector of predicted class if the test dataset, 
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))      #Using Crosstable to compared between the predicted and the results we have 



#STEP 5-Improving model performance
#Boosting the accuracy of decision trees with 10 trials  
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, #Start with 10 trials as it is standard 
                       trials = 10)
credit_boost10
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual default', 'predicted default'))
#Making some mistakes more costly than others 
error_cost <- matrix(c(0,1,4,0), nrow=2)
error_cost
credit_cost <- C5.0(credit_train[-17], credit_train$default,
                      costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual default', 'predicted default'))








#----------------------------------------------------------------------------------------------------------------------------------------------
#IDENTIFYING RISKY BANK LOANS USING RANDOM TREES MODEL
#STEP 2- Exploring and preparing the data 
credit <- read.csv("E:/NorthEastern University/Predictive Analytics/M4/Project/credit.csv", stringsAsFactors = FALSE)
str(credit)
indx = sample(1:nrow(credit), as.integer(0.9*nrow(credit)))

credit_train = credit[indx,]
credit_test = credit[-indx,]

credit_train_labels = credit[indx,17]
credit_test_labels = credit[-indx,17] 


#STEP 3-Model Training on the Data
library(randomForest)
set.seed(300)
rf.model = randomForest(default ~ ., data = credit_train, ntree = 500,
                        mtry = sqrt(16))
rf.model



#STEP 4- Evaluating Model Perforamnce 
rf.pred = predict(rf.model, credit_test)          #Generate an object that contains a vector of prediction value for the tested data labels  
summary(rf.pred)
confusion.matrix = table(rf.pred, credit_test_labels)
confusion.matrix
sum(diag(confusion.matrix)/sum(confusion.matrix))



#STEP 5: Model Improvement
library(randomForest)
set.seed(300)
rf.model2 = randomForest(default~., data = credit_train, ntree = 1000,
                         mtry = 8)
rf.model2

rf.pred2 = predict(rf.model2, credit_test)
summary(rf.pred2)

table(rf.pred2, credit_test_labels)

sum(rf.pred2==credit_test_labels) / nrow(credit_test)

