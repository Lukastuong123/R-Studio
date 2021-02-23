#rm(list = ls())      #Clean the library 
##########DATA PREPARATION#################################
# Load Llibraires, data and surface analysis 
library(ggplot2)     #Data Visulization
library(dplyr)       #Data Manipulation
library(caret)       #Streamline the model training process
library(Boruta)      #Feature Importance Analysis 
library(caTools)     #To split the data
library(e1071)       #SVM Model
library(rpart)       #Decision Tree Model
library(randomForest) #Random Forest


# Load the dataset
cervical_cancer = read.csv("E:/NorthEastern University/Data Mining/M6- Final Report/kag_risk_factors_cervical_cancer.csv")
dim(cervical_cancer)


#  Explore the data 
glimpse(cervical_cancer)
unique(cervical_cancer$STDs..Time.since.first.diagnosis)


#Verify the dataset integrity- NAs 
prop_NA  <- function(x) {mean(is.na(x))}                   #Setting up the function to see the missing values that we have                                
missdata <- sapply(cervical_cancer, prop_NA)               #Apply the prop_Na function to the dataset
missdata <- data.frame(Variables = names(missdata), Proportion= missdata, Completude =1- missdata)
                                                           #Transform this into a dataframe
missdata <- missdata[order(desc(missdata$Proportion)),]    #Order the data into desc order according to the Proportion 


# Data Visualization: Completude vs NAs
ggplot(missdata, aes(x = Variables, y = Completude))+
       geom_bar(stat = "identity", fill = "lawngreen")+
       theme(axis.text.x = element_text(angle = 45, hjust = 1))+
       labs(title = "Porportion of non NA Values")+
       theme(plot.title = element_text(hjust = 0.5))


#Verify Dataset Integrity - Blanks and Zeroes (Same method as above)
prop_NullZero <- function(x) { mean(x == "" | x == 0)}
nullzerodata <- sapply(cervical_cancer, prop_NullZero)
nullzerodata <- data.frame(Variables = names(nullzerodata), Proportion = nullzerodata, Completude = 1 - nullzerodata)
nullzerodata <- nullzerodata[order(desc(nullzerodata$Completude)),]


#Data Visualization: Completude vs blanks and zeroes (Same method as above)
ggplot(nullzerodata, aes(x = Variables, y = Completude))+
      geom_bar(stat = "identity", fill = "deepskyblue2")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      labs(title = "Proportion of non Zero or Blank Values")+
      theme(plot.title = element_text(hjust = 0.5))



##########DATA MANIPULATION#################################
# Create all the function to identify all columns that need repair (If the sum values of cols is ? then we identify it)
find_cols = function(x){
  cols = vector()
  for (i in 1:ncol(x)){
    if (sum(x[,i] == "?") > 0){
      cols = c(cols,i)
    }  
  }
  return(cols)
}


# Create function to fix missing values
fix_columns = function(x,cols) {
  for (j in 1:length(cols)) {
    x[,cols[j]] = as.character(x[,cols[j]])
    x[which(x[,cols[j]] == "?"),cols[j]] = "-1.0"
    x[,cols[j]] = as.numeric(x[,cols[j]])
  }
  return(x)
}
                                              #The string "?" caused entire columns to be interpreted as factors instead of numerics, so I changed to a numeric to ensure consistency, allowing numeric operations to be performed on them.
                                              #As per the "-1" in particular, I just find it to be a good placeholder as it clearly signals that something is wrong with that observation.


#Apply the two function that we just created above 
cols_to_fix = find_cols(cervical_cancer)
cervical_cancer = fix_columns(cervical_cancer, cols_to_fix)


# corelation between variables to reject unimportant variables
set.seed(123)
correlationMatrix <- cor(cervical_cancer[,12:26])
print(correlationMatrix)


#Cut off the correlation at that less than 0.7
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7)
print(highlyCorrelated)


#Create target varibles to represent the cervical cancer 
cervical_cancer$CervicalCancer = cervical_cancer$Hinselmann + cervical_cancer$Schiller + cervical_cancer$Citology + cervical_cancer$Biopsy
                                            #We plus all of these columns together because it represens the sults of cervical cancer exam
cervical_cancer$CervicalCancer = factor(cervical_cancer$CervicalCancer
                                        , levels=c("0","1","2","3","4"))
                                            #Positive exams reulst might not meaimply in a diagnostic, but if multiple happen at the same timel,
                                            #there is a greater risk that the patientjhas cervical cancer 



##########EXPLORATORY DATA ANALYSIS (EDA)#################################
# Explore target varible distribution 
round(prop.table(table(cervical_cancer$CervicalCancer)),2)     #See the percentage of the latest column to see what is the percentage of them according to the grade we assigned 

## Plot target variable distribution
ggplot(cervical_cancer,(aes(x = CervicalCancer, y = sum(as.integer(as.character(CervicalCancer))),fill = CervicalCancer)))+
        geom_bar(stat="identity")+
        scale_fill_manual(values=c("limegreen","gold","orangered","red2","purple"))+
        labs(title = "Quantity of CervicalCancer Classes")+
        theme(plot.title = element_text(hjust = 0.5))


#Density: CervicalCancer across Age (to see how age affect the cervical cancer)
ggplot(cervical_cancer, aes(x = Age, fill=CervicalCancer))+
      geom_density(alpha = 0.40, color=NA)+
      scale_fill_manual(values=c("limegreen","gold","orangered","red2","purple"))+
      labs(title = "Density of CervicalCancer across Age")+
      theme(plot.title = element_text(hjust = 0.5))+
      facet_grid(as.factor(CervicalCancer) ~ .)


# Density: CervicalCancer across Hormornal Conctraceptive years (How can can years of hormonal contraceptives actually help)
ggplot(cervical_cancer, aes(x = Hormonal.Contraceptives..years., fill=CervicalCancer))+
  geom_density(alpha = 0.40, color=NA)+
  scale_fill_manual(values=c("limegreen","gold","orangered","red2","purple"))+
  labs(title = "Density of CervicalCancer across Years of Hormonal Contraceptives")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_grid(as.factor(CervicalCancer) ~ .)




##########FEATURE IMPORTANCE ANALYSIS WITH BORUTA#################################
#Create copy of the original dataset remove the medical results columns 
train = cervical_cancer
train[,c("Hinselmann","Schiller","Citology","Biopsy")] = NULL


#Perform Boruta analysis on the training set 
set.seed(100)
boruta_analysis = Boruta(CervicalCancer~., data = train, maxRuns=200)


#Plot the Boruta results abd see the dataframe 
plot(boruta_analysis,las=2,main="Boruta Analysis: Variable Importance")
as.data.frame(boruta_analysis$finalDecision)
print(boruta_analysis)


#Get all the confirmed columns as we see here 
final.boruta <- TentativeRoughFix(boruta_analysis)
getSelectedAttributes(final.boruta, withTentative = F)


#Create the data frame of the final result derived from Boruta.
boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)




##########PREDICTION MODELS #################################
#Getting only the "Confirmed" variables that we just find out above 
C_cervical_cancer = select(cervical_cancer,
                           'Age','Number.of.sexual.partners', 'Smokes..years.','Smokes..packs.year.','Hormonal.Contraceptives','Hormonal.Contraceptives..years.', 'IUD',                                
                           'IUD..years.', 'STDs', 'STDs..number.', 'STDs.condylomatosis', 'STDs.vulvo.perineal.condylomatosis', 'STDs.Hepatitis.B',                   
                           'STDs.HPV', 'STDs..Number.of.diagnosis', 'STDs..Time.since.first.diagnosis', 'STDs..Time.since.last.diagnosis',    
                           'Dx.Cancer','Dx.HPV','Dx', 'CervicalCancer')
#Assign the values to yes or no potential for cancers 
C_cervical_cancer$CervicalCancer = as.numeric(C_cervical_cancer$CervicalCance)
C_cervical_cancer$CervicalCancer[C_cervical_cancer$CervicalCancer <2] <- 0
C_cervical_cancer$CervicalCancer[C_cervical_cancer$CervicalCancer >1] <- 1
# Encoding the target feature as factor
C_cervical_cancer$CervicalCancer = factor(C_cervical_cancer$CervicalCancer, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set for prediction 
set.seed(12345)
split = sample.split(C_cervical_cancer$CervicalCancer, SplitRatio = 0.75)
training_set = subset(C_cervical_cancer, split == TRUE)
test_set = subset(C_cervical_cancer, split == FALSE)
# Feature Scaling
training_set[-21] = scale(training_set[-21])
test_set[-21] = scale(test_set[-21])



#SVM
set.seed(123812)
classifier1 = svm(formula = CervicalCancer ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
                              #type:there are a lot of types but we choose C-Classification
#DecisionTree
set.seed(1234341)
classifier2 = rpart(formula = CervicalCancer  ~ .,
                   data = training_set)

#RandomForest 
set.seed(12389)
classifier3 = randomForest(x = training_set[-21],
                          y = training_set$CervicalCancer,
                          ntree = 500)
                                        #x has to be the dataframe,matrix  
                                        #y is teh vector 


# Predicting the Test set results
y_pred1 = predict(classifier1, newdata = test_set[-21])
y_pred2 = predict(classifier2, newdata = test_set[-21], type = 'class')
y_pred3 = predict(classifier3, newdata = test_set[-21], type = 'class')

# Making the Confusion Matrix
confusionMatrix(test_set[, 21], y_pred1)
confusionMatrix(test_set[, 21], y_pred2)
confusionMatrix(test_set[, 21], y_pred3)


