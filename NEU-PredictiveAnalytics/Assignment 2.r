rm(list=ls())


#Importing the dataset
sms_raw <- read.csv("E:/NorthEastern University/Predictive Analytics/M2/Project/sms_spam.csv", stringsAsFactors = FALSE)
summary(sms_raw)
str(sms_raw)

#Converting the categorical variable to factor 
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)








#Step 2: DATA CLEANING- PROCESSING TEXT DATA FOR ANALYSIS
#install.packages("tm")
library(tm)

#Creating a Corpus  
sms_corpus <- Corpus(VectorSource(sms_raw$text))       #The Corpus function creates an R object to store text documents 
print(sms_corpus)

#Expecting messages 1-3 from the corpus before cleaning
inspect(sms_corpus[1:3])

#Conver all the SMS messages to lowercase and remove any numbers
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)

#Removing all the common filter words (stop words), punctuation, additional whitespaces 
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())     #Stop words
corpus_clean <- tm_map(corpus_clean, removePunctuation)            #punctuation 
corpus_clean <- tm_map(corpus_clean, stripWhitespace)              #additional white space

#Expecting messages 1-3 from the corpus after cleaning 
inspect(corpus_clean[1:3])

#Tokenization of the data by creaing a sparse matrix given a tm corpus involve a single command
sms_dtm <- DocumentTermMatrix(corpus_clean)


#Data prepatation- creating training and test datasets 
#Spiliting the raw data
sms_raw_train <- sms_raw[1:4180, ]             #Taking 75% of the 5574 elements
sms_raw_test <- sms_raw[4181:5574, ]           #Taking 25% of the 5574 elements
#Document-term matrix 
sms_dtm_train <- sms_dtm[1:4180, ]
sms_dtm_test <- sms_dtm[4181:5574, ]
#Corpus
sms_corpus_train <- corpus_clean[1:4180]
sms_corpus_test <- corpus_clean[4181:5574]
#Comparing the subsets proporton of spam in the training and the test set just to make sure 
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

#Visulizing text data- word clouds 
#install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_train, min.freq = 40, random.order = FALSE)   #10% of the corpus for min-frequen but I settled for 40 instead 
#Comparing the visulization between the "ham" and the "spam" messages
spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))


#Data preparation - creating indicator features for frequent words (eliminate all the words that occur less than 0.1% of records in the training data)
#Find the frequent words and save this list to the Dictionary() function
#install.packages("quanteda")
library(quanteda)
findFreqTerms(sms_dtm_train, 5)
sms_dict <- c(findFreqTerms(sms_dtm_train, 5))
sms_train <- DocumentTermMatrix(sms_corpus_train,
                                list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test,
                                 list(dictionary = sms_dict))
convert_counts<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels=c(0,1),labels=c("No","Yes"))
  return(x)
}                                                    #convert_counts() function to convert counts to factors:
# Use apply() convert_counts() to columns of train/test data
sms_train<-apply(sms_train,MARGIN=2,convert_counts)
sms_test<-apply(sms_test,MARGIN=2,convert_counts)





#STEP 3 - TRAINING A MODEL ON THE DATA
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
#sms_classifier




#STEP 4: EVALUATING MODEL PERFORMANCE
sms_test_pred <- predict(sms_classifier, sms_test)

#Plot the CrossTable 
#install.packages("gmodels")
library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
#Using precrec package to plot the Precision and Recall 
library(caret)
confusionMatrix(sms_test_pred,sms_raw_test$type)




#STEP 5 - IMPROVING MODEL PERFORMANCE
#Build a naive Bayes model with  laplace = 1:
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type,
                              laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
#Using caret package to plot the the Prcision and Recall value 
library(caret)
confusionMatrix(sms_test_pred2,sms_raw_test$type)
#Build a naive Bayes model with  laplace = 5:
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type,
                              laplace = 5)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
#Using caret package to plot the the Prcision and Recall value 
library(caret)
confusionMatrix(sms_test_pred2,sms_raw_test$type)
#Build a naive Bayes model with  laplace = 10:
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type,
                              laplace = 10)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
#Using caret package to plot the the Prcision and Recall value 
library(caret)
confusionMatrix(sms_test_pred2,sms_raw_test$type)



#------------------------------------------------REPEAT EVERYTHING KNN METHOD----------------------------------
#STEP 3 - TRAINING A MODEL ON THE DATA
#install.packages("class")
knn_model <- knn(train= sms_raw_train, test= sms_raw_test, cl= trainLabels, k =65)
knn_model

#Getting the cross table showing predictions 
CrossTable(knn_model,
           testLabels,
           prop.chisq=FALSE)

#Getting the confusion matrix showing the perforamnce metrics 
knn_confusion_matric <- confusionMatrix(knn_model,testLabels)
knn_confusion_matrix 
