##########install packages you need to finish this homework. packages needed: "tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc"
#packages installation may take 20~30 minutes
Needed <- c("tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
            "cluster", "igraph", "fpc")
install.packages(Needed, dependencies = TRUE)
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")







##########Downloaded all the files from:https://drive.google.com/drive/folders/0B914dXn0AXvlaWhmVXdPclZrTjA #################################
#And save them in folder C:/texts

#Create a folder to store all homework input files. For exampes, C:/texts
#And save them in folder C:/texts
cname <- file.path("E:/NorthEastern University/Data Mining/M1/Project", "text")   
cname   
dir(cname)                                                                                #dir:function returns a character vector of file and/or folder names within a directory, everything will be taken from here from now on 







##########Start Analysis#################################
#Load package "tm" and all the files you have downloaded
library(tm)

#Create Corpus after you create the Source
docs <- VCorpus(DirSource(cname))   
summary(docs)   

#Load details of any documents in the corpus
#for example, load the first document in corpus
inspect(docs[1])
inspect(docs[2])







##########Preprocessing#################################

#Remove numbers, capitalization, common words, punctuation, and otherwise prepare your texts for analysis.

docs <- tm_map(docs,removePunctuation) 


for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])
}

docs <- tm_map(docs, removeNumbers)  
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, PlainTextDocument)
#Removing particular words
docs <- tm_map(docs, removeWords, c("syllogism", "tautology"))   

#Combining words that should stay together.for example, combine "inner", "city" as "inner-city" so you can analyze them together
for (j in seq(docs))
{
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)

docs_st <- tm_map(docs, stemDocument)   
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1]))

docs_stc <- tm_map(docs_st, stemCompletion, dictionary = DocsCopy, lazy=TRUE)
docs_stc <- tm_map(docs_stc, PlainTextDocument)
writeLines(as.character(docs_stc[1]))

docs <- tm_map(docs, stripWhitespace)

docs <- tm_map(docs, PlainTextDocument)







##########Stage your data#################################
#A document-term matrix or term-document matrix is a mathematical matrix that describes the frequency of terms that occur in a collection of documents. In a document-term matrix, rows correspond to documents in the collection and columns correspond to terms. 
dtm <- DocumentTermMatrix(docs)   
dtm 

#Transpose the matrix
tdm <- TermDocumentMatrix(docs)   
tdm  

#Organize terms by Frequency
freq <- colSums(as.matrix(dtm))   
length(freq)

ord <- order(freq) 

m <- as.matrix(dtm)   
dim(m) 
#Explore as csv
write.csv(m, file="DocumentTermMatrix.csv")   

#  Start by removing sparse terms:
dtms <- removeSparseTerms(dtm, 0.2) # This makes a matrix that is 20% empty space, maximum.   
dtms

# most and least frequently occurring words.
freq <- colSums(as.matrix(dtm)) 
#Check out the frequency of frequencies
head(table(freq), 20) #The resulting output is two rows of numbers. The top number is the frequency with which words appear and the bottom number reflects how many words appear that frequently. Here, considering only the 20 lowest word frequencies, we can see that 1602 terms appear only once. There are also a lot of others that appear very infrequently.

tail(table(freq), 20) # The ", 20" indicates that we only want the last 20 frequencies

#For a less, fine-grained look at term freqency we can view a table of the terms we selected when we removed sparse terms
freq <- colSums(as.matrix(dtms))   
freq 

#sort the most frequent words as decreasing order
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14) #select top 14

#create a data frame for next steps
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)







##########Plot Word Frequency#################################
library(ggplot2)#load package ggplot2
#Plot a histogram for words that appear at least 50 times
p <- ggplot(subset(wf, freq>50), 
            aes(x = reorder(word, -freq), y = freq)) +geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1))
p  

##########Calculate terms correlations#################################
#identify the words that most highly correlate with that term. If words always appear together, then correlation=1.0
findAssocs(dtm, c("country" , "american"), corlimit=0.85) # specifying a correlation limit of 0.85. Feel free to change the words you want to check and correlation limit

##########Create word clouds#################################
library(wordcloud)   
dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)   
freq <- colSums(as.matrix(dtm))      # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)    








##########Hierarchal Clustering#################################
dtms <- removeSparseTerms(dtm, 0.15)     # This makes a matrix that is only 15% empty space.
library(cluster)   
d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
fit <- hclust(d=d, method="complete")    # Also try: method="ward.D"   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=6)               # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=6, border="red")      # draw dendrogram with red borders around the 5 clusters   

##########K-means clustering#################################
library(fpc)   
library(cluster)  
dtms <- removeSparseTerms(dtm, 0.15)     # Prepare the data (max 15% empty space)   
d <- dist(t(dtms), method="euclidian")   
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  

