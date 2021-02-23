##########Load the dataset#################################
personality = read.table("E:/NorthEastern University/Data Mining/M5/Project/5.1_personality_dataset.txt")
head(personality)
str(personality)
summary(personality)


##########Create the Corrplot#################################
# For dataframes with many columns, corrplot can be useful to get a sense of the structure in the data (including larger scale organization)
library(corrplot)
corrplot(cor(personality), order = "hclust", tl.col='black', tl.cex=.75)



##########FACTOR ANALYSIS#################################
##########Normal one
### Standarize the varibles 
d_stan = as.data.frame(scale(personality))

### Factor analysis with no rotation
res1b = factanal(d_stan, factors = 10, rotation = "none", na.action = na.omit)
res1b$loadings

###Compute eigenvalue of factor 1
loadings_fac1 = res1b$loadings[,1]
eigenv_fac1 = sum(loadings_fac1^2); eigenv_fac1
# Compute proportion variance
eigenv_fac1/32

### Uniqueness and communality
res1b$uniquenesses
# Calculate uniqueness
loadings_distant = res1b$loadings[1,]
communality_distant = sum(loadings_distant^2); communality_distant
uniqueness_distant = 1-communality_distant; uniqueness_distant

#### Visulize Loadings
### Plot loadings against one another
load = res1b$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(d_stan),cex=.7) # add variable names




##########ROTATION OF FACTORS #################################
### Factor analysis with rotation
res1a = factanal(d_stan, factors = 10, rotation = "varimax", na.action = na.omit)
res1a$loadings

### Plot loadings against one another
load = res1a$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(d_stan),cex=.7) # add variable names




##########CRREATING COMPOSITE VARIABLES #################################
k = 2
p = 5
yes = 'yes! lets extract k factors!'
no = 'no! we need more data or fewer factors!'
ifelse(((p-k)^2 > p+k), yes, no)

# Let's try combining some synonyms and then look at the extracted Factors!
  shy = rowMeans(cbind(d_stan$distant, d_stan$shy, d_stan$withdrw, d_stan$quiet))
outgoing = rowMeans(cbind(d_stan$talkatv, d_stan$outgoin, d_stan$sociabl))
hardworking = rowMeans(cbind(d_stan$hardwrk, d_stan$persevr, d_stan$discipl))
friendly = rowMeans(cbind(d_stan$friendl, d_stan$kind, d_stan$coopera, d_stan$agreebl, d_stan$approvn, 
                          d_stan$sociabl))
anxious = rowMeans(cbind(d_stan$tense, d_stan$anxious, d_stan$worryin))
#etc, you guys choose what you want to combine
combined_data = cbind(shy,outgoing,hardworking,friendly,anxious)
combined_data = as.data.frame(combined_data)
res2 = factanal(combined_data, factors = 2, na.action=na.omit)
res2$loadings

### Plot loadings against one another
load = res2$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(combined_data),cex=.7) # add variable names

