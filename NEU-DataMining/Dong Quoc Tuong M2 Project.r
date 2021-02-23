#1.LOADING DATA AND PACKAGES 
##########Install the "Datarium" dataset from the R system #################################
install.packages("datarium")

########## Load the lilrary that we need #################################
install.packages("caTools")
library("caTools")
library("ggplot2")
install.packages("GGally")
library("GGally")

########## Loading the data #################################
data("marketing", package = "datarium")
data_size = dim(marketing)




#2.EDA
########## Preparing the data #################################
head(marketing)
summary(marketing)

########## Boxplot#################################
par(mfrow=c(1, 4))                                                                                        # divide graph area in 2 columns
boxplot(marketing$youtube, main="Youtube", sub=paste("Outlier rows: ", boxplot.stats(marketing$youtube)$out))       # box plot for 'Youtube'
boxplot(marketing$facebook, main="Facebook", sub=paste("Outlier rows: ", boxplot.stats(marketing$facebook)$out))    # box plot for 'Facebook'
boxplot(marketing$newspaper, main="Newspaper", sub=paste("Outlier rows: ", boxplot.stats(marketing$newspaper)$out)) # box plot for 'Newspaper'
boxplot(marketing$sales, main="sales", sub=paste("Outlier rows: ", boxplot.stats(marketing$sales)$out)) # box plot for 'Newspaper'


########## Density Plot #################################
library(e1071)
par(mfrow=c(1, 2))                                                                                                                                # divide graph area in 2 columns
plot(density(marketing$youtube), main="Density Plot: Youtube", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(marketing$youtube), 2)))      # density plot for 'youtube'
polygon(density(marketing$youtube), col="red")
plot(density(marketing$facebook), main="Density Plot: Facebook", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(marketing$facebook), 2)))   # density plot for 'facebook'
polygon(density(marketing$facebook), col="red")
par(mfrow=c(1, 2)) 
plot(density(marketing$newspaper), main="Density Plot: Newspaper", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(marketing$newspaper), 2)))# density plot for 'newspaper'
polygon(density(marketing$newspaper), col="red")
plot(density(marketing$sales), main="Density Plot: Sales", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(marketing$sales), 2)))   # density plot for 'facebook'
polygon(density(marketing$sales), col="blue")

########## Pairwise plotting technique 1#################################
plot(marketing, col = "purple", main="Plotting Pairs Against Each Other")

########## Pairwise plotting technique 2 ################################# 
ggpairs(marketing)




#3.PREPARING THE DATA
########## Splitting the data #################################  
set.seed(101)      #Set seed sop that same sample can be reproduced in future also 

########## Now selecting 75% of the data as sample from total 'n' rows of the data #################################  
sample = sample.split(marketing$youtube, SplitRatio = 0.75)
train = subset(marketing, sample==TRUE)
test  = subset(marketing, sample==TRUE)
train_size = dim(train)
test_Size = dim(test)




#4. CREATING THE MODEL
########## Creating the model #################################  
Model <- lm(sales ~ youtube +facebook +newspaper, data = marketing)
summary(Model)




#5. MODEL ACCURACY ANALYSIS
########## Predicting#################################  
pred <- predict(Model, test)
numx <- data_size[1]*(1 - 0.75)
x_axis <- seq(numx)
df <- data.frame(x_axis, pred,test$sales)

########## Plotting the predicted values against the actual values#################################  
g <- ggplot(df, aes(x=x_axis))
g <- g + geom_line(aes(y=pred, colour="Predicted"))
g <- g + geom_point(aes(x=x_axis, y=pred, colour="Predicted"))
g <- g + geom_line(aes(y=test$sales, colour="Actual"))
g <- g + geom_point(aes(x=x_axis, y=test$sales, colour="Actual"))
g <- g + scale_colour_manual("", values = c(Predicted="red", Actual="blue"))
g

########## Evaluation#################################  
original = test$sales
predicted = pred
d = original-predicted
mse = mean((d)^2)
mae = mean(abs(d))
rmse = sqrt(mse)
R2 = 1-(sum((d)^2)/sum((original-mean(original))^2))
cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", R2)

