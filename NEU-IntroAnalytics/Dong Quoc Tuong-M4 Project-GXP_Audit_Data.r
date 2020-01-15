#Put this in the Console section with the CSV file and the session directory is at the location 
#as well to import the file 
#SET UP DATA 
raw_data<-read.csv("GXP_Audit_Data(1).csv",TRUE,",")
class(raw_data)
head(raw_data)
install.packages("plyr")            #Install the plyr package 
library("plyr")                     #Activate plyr package 


#ANSWER THESE QUESTIONS 
#1. A table that provides the frequency and percent of each value
#2. A graphic representation of the count of each value
#3. A graphic representation of the percent of each value


      #Audit Status
Audit_status<-count(raw_data,vars="Audit.Status")
Audit_status
require("RColorBrewer")

slices <- c(4,19,4,18,26,14,11)
lbls <- c("Cancelled","Closed","Completed","In Progress","Not In Scope","Pending","Scheduled")
pie(slices ,labels= slices, main="Pie Chart of Audit status", col = rainbow(length(slices)))
legend("topright", legend=lbls, cex = 0.8, fill = rainbow(length(slices)))

piepercent<- round(100*slices/sum(slices), 1)
pie(slices ,labels= paste(piepercent,"%", collapse = NULL), main="Percentage Pie Chart of Audit status", col = rainbow(length(slices)))
legend("topright", legend=lbls, cex = 0.8, fill = rainbow(length(slices)))
#the paste is to connect between 2 the changeable values and the % together into one 




#In USA or OUS
InUSAorOUS<-count(raw_data,vars="In.USA.or.OUS")             
InUSAorOUS
require("RColorBrewer")

slices <- c(45,51)
lbls <- c("OUS","USA")
pie(slices ,labels= slices, main="Pie Chart of In USA or Out", col = rainbow(length(slices)))
legend("topright", legend=lbls, cex = 0.8, fill = rainbow(length(slices)))

piepercent<- round(100*slices/sum(slices), 1)
pie(slices ,labels= paste(piepercent,"%", collapse = NULL), main="Percentage Pie Chart of In USA or Out", col = rainbow(length(slices)))
legend("topright", legend=lbls, cex = 0.8, fill = rainbow(length(slices)))
#the paste is to connect between 2 the changeable values and the % together into one 




#GxP Area
GxP.Area<-count(raw_data,vars="GxP.Area")             
GxP.Area
require("RColorBrewer")

slices <- c(34,7,14,33,2,6)
lbls <- c("GCP","GDP","GLP","GMP","GMP/GDP","GVP")
pie(slices ,labels= slices, main="Pie Chart of In GxP.Area", col = rainbow(length(slices)))
legend("topright", legend=lbls, cex = 0.8, fill = rainbow(length(slices)))

piepercent<- round(100*slices/sum(slices), 1)
pie(slices ,labels= paste(piepercent,"%", collapse = NULL), main="Percentage Pie Chart of GxP.Area", col = rainbow(length(slices)))
legend("topright", legend=lbls, cex = 0.8, fill = rainbow(length(slices)))
#the paste is to connect between 2 the changeable values and the % together into one 




#Audit Type
Audit.Type<-count(raw_data,vars="Audit.Type")            #Dont put them in quotation because it can be taken out 
Audit.Type
require("RColorBrewer")

slices <- c(5,38,50,3)
lbls <- c("(Unknown)","Qualification","Requalification","Requalification/Q")
pie(slices ,labels= slices, main="Pie Chart of Audit.Type", col = rainbow(length(slices)))
legend("topright", legend=lbls, cex = 0.8, fill = rainbow(length(slices)))

piepercent<- round(100*slices/sum(slices), 1)
pie(slices ,labels= paste(piepercent,"%", collapse = NULL), main="Percentage Pie Chart of Audit.Type", col = rainbow(length(slices)))
legend("topright", legend=lbls, cex = 0.8, fill = rainbow(length(slices)))
#the paste is to connect between 2 the changeable values and the % together into one 





#Audit Method
Audit.Method<-count(raw_data,vars="Audit.Method")            #Dont put them in quotation because it can be taken out 
Audit.Method
require("RColorBrewer")

slices <- c(4,24,52,1,1,10,2,2)
lbls <- c("(Unknown)","None","On Site","On Site \n*CSQA did not participate","Q in 2016","Questionnaire","Remote","TBD")
pie(slices ,labels= slices, main="Pie Chart of Audit.Method", col = rainbow(length(slices)))
legend("topleft", legend=lbls, cex = 0.8, fill = rainbow(length(slices)))

piepercent<- round(100*slices/sum(slices), 1)
pie(slices ,labels= paste(piepercent,"%", collapse = NULL), main="Percentage Pie Chart of Audit.Method", col = rainbow(length(slices)))
legend("topleft", legend=lbls, cex = 0.8, fill = rainbow(length(slices)))
#the paste is to connect between 2 the changeable values and the % together into one 





#Proposed Quarter
Proposed.Quarter<-count(raw_data,vars="Proposed.Quarter")            
Proposed.Quarter
require("RColorBrewer")

slices <- c(5,23,27,21,3,17)
lbls <- c("Q1","Q2","Q3","Q3 - Q4","Q4")
pie(slices ,labels= slices, main="Pie Chart of Proposed Quarter", col = rainbow(length(slices)))
legend("topright", legend=lbls, cex = 0.8, fill = rainbow(length(slices)))

piepercent<- round(100*slices/sum(slices), 1)
pie(slices ,labels= paste(piepercent,"%", collapse = NULL), main="Percentage Pie Chart of Proposed Quarter", col = rainbow(length(slices)))
legend("topright", legend=lbls, cex = 0.8, fill = rainbow(length(slices)))
#the paste is to connect between 2 the changeable values and the % together into one 
