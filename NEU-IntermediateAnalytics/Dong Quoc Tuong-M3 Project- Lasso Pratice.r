rm(list = ls())
install.packages('ncvreg')
library('ncvreg')
install.packages('bigmemory')
library('bigmemory')
install.packages('biglasso')
library('biglasso')



#Excerise 1 
install.packages('lars')     #Install lars,gmlnet packages and introduce the diabetes dataset
library('lars')
data(diabetes)
install.packages('glmnet')
library('glmnet')


#Excerise 2                       #Use the dataset to plot the scatter plot for the predictor x with y 
summary(diabetes$x)
cbind(summary(diabetes$x))
par(mfrow=c(2,5))
for(i in 1:10)
  plot(diabetes$x[,i], diabetes$y)
  abline(lm(diabetes$y~diabetes$x[,i]))
layout(1)


#Excerise 3 
OLS<- lm(diabetes$y ~ diabetes$x) #Regress the result in x using OLS to use as the benchmark
summary(OLS)
cbind(summary(OLS))


#Excerise 4
OLS <- glmnet(diabetes$x, diabetes$y) #Graph to see when the x's coefficients against the L1 norm of the beta vector shink to 0 
plot(OLS, xvar = "norm", label = TRUE)


#Excerise 5            #Cross validation curve and the value of lambda
fitted<-  cv.glmnet(x=diabetes$x, y=diabetes$y, alpha = 1, nlambda = 1000)
plot(fitted)
small_lambda<-fitted$lambda.min

#Excerise 6           #Get the estimated beta matrix, indicates which predictors are important in explaining the variation in y
fitted2<-  glmnet(x=diabetes$x, y=diabetes$y, alpha = 1, lambda =small_lambda)
fitted2$beta

#Excerise 7 
One_se<-fitted$lambda.1se
One_se
fitted3 <-  glmnet(x=diabetes$x, y=diabetes$y, alpha = 1, lambda =One_se)
fitted3$beta






#Excerise 8               
summary(diabetes$x2)          #Using x2 to do the analysis

OLS2<- lm(diabetes$y ~ diabetes$x2)
summary(OLS2)
cbind(summary(OLS2))

#Excerise 9 
OLS2 <- glmnet(diabetes$x2, diabetes$y)
par(mfrow=c(1,1))
plot(OLS, xvar = "norm", label = TRUE)

#Excerise 10 
fitted_x2<-  cv.glmnet(x=diabetes$x2, y=diabetes$y, alpha = 1, nlambda = 1000)
plot(fitted_x2)
small_lambda_x2<-fitted_x2$lambda.min

fitted2_x2<-  glmnet(x=diabetes$x2, y=diabetes$y, alpha = 1, lambda =small_lambda_x2)
fitted2_x2$beta

