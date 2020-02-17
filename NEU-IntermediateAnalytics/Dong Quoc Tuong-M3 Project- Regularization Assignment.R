rm(list = ls())
install.packages('ncvreg')
library('ncvreg')
install.packages('bigmemory')
library('bigmemory')
install.packages('biglasso')
library('biglasso')
install.packages('ISLR')
library(ISLR)
library(dplyr)
library(tidyr)

data(Hitters)
cbind(summary(Hitters))     #Choose the Hitters dataset from R and plot the summary of it 
Hitters= na.omit(Hitters)   #Eliminate all the missing values from the Hitters  

Independents_x =model.matrix(Salary~., Hitters)[,-1] #Choose only the independents variables by leaving out the first column 
Independents_x_small<-head(Independents_x,50)
dim(Independents_x)
dim(Independents_x_small)

Dependent_y = as.numeric(unlist(Hitters$Salary))      #Choose the Dependent vairables and put it as numeric
Dependent_y_small<-head(Dependent_y,50)


#SMALL DATA
Independents_x_small[1:5,1:5]
Independents_x_small.bm <- as.big.matrix(Independents_x_small)    #Convert Independents_x_small to big matrix object 
str(Independents_x_small.bm)                                      #Independents_x.bm is a pointer to the data matrix
dim(Independents_x_small.bm)
Independents_x_small[1:5,1:5]                                     #Same result with the above 


par(mfrow=c(3,7))
for(i in 1:20)
  plot(Independents_x_small[,i], Dependent_y_small)
abline(lm(Dependent_y_small~Independents_x_small[,i]))
layout(1)


fit <- biglasso(Independents_x_small.bm, Dependent_y_small,screen = "SSR-BEDPP")       #Fit entire solution path, using the SSR-BEDPP 
par(mfrow= c(1,1))
plot(fit)

cvfit_1 <-cv.biglasso(Independents_x_small.bm, Dependent_y_small, seed = 1234, nfolds = 10, ncores = 4) #10fold crossvalidation in then parallel

        #Plot the cross validation plots 
        par(mfrow= c(2,2), mar=c(3.5, 3.5,3,1), mgp=c(2.5,0.5,0))
        plot(cvfit_1, type="all")
        
        #Summarize CV object 
        summary(cvfit_1)
        
        #Extract non-zero coefficients at the optimal lambda value 52.9345
        coef(cvfit_1)[which(coef(cvfit_1) !=0)]


        
        
#BIG DATA
xfname <- Independents_x  
  X <- setupX(xfname, sep = '\t')) # create backing files (.bin, .desc)\)
print(time)
dim(Independents_x)
Independents_x[1:5, 1:5]
object.size(Independents_x)


fitdasd<- biglasso(Independents_x, Dependent_y, family='gaussian', screen='SSR-BEDPP')
plot(fitdasd)

time.cvfit <- system.time(
  cvfit_2 <- cv.biglasso(Independents_x, Dependent_y, screen = SSR-BEDPP , seed = 1234, ncores = 4, nfolds = 10))
print(time.cvfit)

      #Plot the cross validation plots
      par(mfrow = c(2, 2), mar = c(3.5, 3.5, 3, 1), mgp = c(2.5, 0.5, 0))
      plot(cvfit_2, type = "all")
      
      #Summarize CV object
      summary(cvfit_2)
      
      #Extract non-zero coefficients at the optimal lambda value 
      coef(cvfit_2)[which(coef(cvfit) != 0)]
                    
                    
        