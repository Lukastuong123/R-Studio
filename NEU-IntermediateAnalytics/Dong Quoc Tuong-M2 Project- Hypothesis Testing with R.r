#One-sample t-test:
head(chem)
cbind(summary(chem))
t.test(chem, mu=1, alternative="greater")

   
  
#Two-sample t-test:
head(cats)
ggplot(cats,aes(x=Sex, y=Bwt))+
  geom_boxplot()+ coord_flip()
male <-subset(cats,subset=(cats$Sex=="M"))
female<- subset(cats, subset=(cats$Sex=="F"))
t.test(male$Bwt,female$Bwt)
t.test(male$Bwt,female$Bwt, var.equal=TRUE)




#Paired t-test:
head(shoes)
plot(shoes$A,type = "o",col = "red", ylab = "Size", 
     main = "Lines chart of shoes")
lines(shoes$B, type = "o", col = "blue")
t.test(shoes$A, shoes$B, paired= TRUE, alternative="greater")



#Test of equal or given proportions:
head(bacteria)
cbind(summary(bacteria))
print("H0: THe drug treament doesnt have a significant effect of the presence of the bacteria compared with the placebbo ")
print("Ha: THe drug treament has a significant effect of the presence of the bacteria compared with the placebbo ")
active <- subset(bacteria, subset=(bacteria$ap=="a"))
placebo <- subset(bacteria, subset=(bacteria$ap=="p"))
a<- c(count(active, vars="y")[2,'freq'],
      count(placebo, vars="y")[2,'freq'])
b<-c(nrow(active),
     nrow(placebo))
prop.test(a,b)



#F-test:
res.ftest <- var.test(male$Bwt,female$Bwt, alternative="two.sided" )
res.ftest




