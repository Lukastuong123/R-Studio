#Choose the Data: Carbon Dioxide Uptake in Grass Plants
head(quine)
cbind(summary(quine))

#t-test for "eth"
#Plot a table to demonstrate them: 
ggplot(quine,aes(x=Eth, y=Days))+
  geom_boxplot()+ coord_flip()
#T-test and p-Value 
print("H0: The days off is not related to the ethnicity of the children")
print("H1: The days off is related to the ethnicity of the children")
Eth<- quine$Eth
Days<- quine$Days
t.test(Days~Eth)



#t-test for "Sex"
#Plot a table to demonstrate them: 
ggplot(quine,aes(x=Sex, y=Days))+
  geom_boxplot()+ coord_flip()
#T-test and p-Value 
print("H0: The days off is not related to the sexes of the children")
print("H0: The days off is related to the sexes of the children")
Sex<- quine$Sex
t.test(Days~Sex, conf.level=0.99)



#Comparison of Two Population Proportions
x<-table(Eth, Sex)
barplot(x, main="Ethnicity and sex",
        xlab="Number of students",
        col=c("darkblue","red"),
        legend = rownames(x))

"Find the 95% confidence interval estimate of the difference between the female proportion of Aboriginal Studentns and the the female proportion of Non Aboriginal students  "
prop.test(table(quine$Eth, quine$Sex), correct=FALSE) 
print('The 95% confidence interval estimate of the difference between the female proportion of Aboriginal students and the female proportion of Non-Aboriginal students is between -15.6% and 16.7%')




