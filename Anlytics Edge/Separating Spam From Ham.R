# Here are our libraries
library(tm)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(slam)
#Here is our data

emails=read.csv('C:/Users/arash/Downloads/emails.csv', stringsAsFactors=FALSE)

# Let's do pre-processing to our emails. Our First step is to make a corpus out of our emails

corpus=Corpus(VectorSource(emails$text))
corpus=tm_map(corpus, tolower)
corpus=tm_map(corpus, PlainTextDocument)
corpus=tm_map(corpus, removePunctuation)
corpus=tm_map(corpus, removeWords, stopwords('english'))
corpus=tm_map(corpus, stemDocument)

# Second step in pre-procssing is to make a Documnet Term matrix out of our corpus

dtm=DocumentTermMatrix(corpus)
sprdtm=removeSparseTerms(dtm, .95)
emailsSparse=as.data.frame(as.matrix(sprdtm))
colnames(emailsSparse)=make.names(colnames(sprdtm))
emailsSparse$spam=emails$spam
emailsSparse$spam=as.factor(emailsSparse$spam)

#Here we divide our data set to training and test data frames


set.seed(123)
split=sample.split(emailsSparse$spam, 0.7)
train=subset(emailsSparse, split==TRUE)
test=subset(emailsSparse, split==FALSE)

#After looking at the frequencies of different variables we would like to 
# do a machine learning model

#Let's do logistic regression

spamLOG=glm(spam ~., data=train, family='binomial' )
predLOG=predict(spamLOG, newdata=test)
confusionLOG=table(test$spam, predLOG>0.5)
accuracyLOG=sum(diag(confusionLOG))/sum(confusionLOG)
predROCR.LOG=prediction(predLOG, test$spam)
perfLOG=performance(predROCR.LOG, 'tpr', 'fpr')
AUCLOG=performance(predROCR.LOG, 'auc')@y.values

#And Also CART
spamCART=rpart(spam ~. , data=train, method='class')
predCART=predict(spamCART, newdata=test, type='prob')[,2]
confusionCART=table(test$spam, predCART>0.5)
accuracyCART=sum(diag(confusionCART))/sum(confusionCART)
predROCR.CART=prediction(predCART, test$spam)
perfCART=performance(predROCR.CART, 'tpr', 'fpr')
AUCCART=performance(predROCR.CART, 'auc')@y.values


#Last is Random Forest
set.seed(123)
spamRF=randomForest(spam ~. , data=train)
predRF=predict(spamRF, newdata=test, type='prob')[,2]
confusionRF=table(test$spam, predRF>0.5)
accuracyRF=sum(diag(confusionRF))/sum(confusionRF)
predROCR.RF=prediction(predRF, test$spam)
perfRF=performance(predROCR.RF, 'tpr', 'fpr')
AUCRF=performance(predROCR.RF, 'auc')@y.values

#Integrating Word Count Information
wordCount=rollup(dtm, 2, FUN=sum)$v
emailsSparse$logwordCount=log(wordCount)
test2=subset(emailsSparse, split==FALSE)
train2=subset(emailsSparse, split==TRUE)
#We are going to run another CART model to this new trainig set
spamCART2=rpart(spam ~.,data=train2)
predCART2=predict(spamCART2, newdata=test2, type='prob')
confusionCART2=table(test2$spam, predCART2[,2])
accuracyCART2=sum(diag(confusionCART2))/sum(confusionCART2)
predROCR.CART2=prediction(predCART2[,2], test2$spam)
AUCCART2=performance(predROCR.CART2, 'auc')@y.values

#And also Random Forest model to our new data set
set.seed(123)
spamRF2=randomForest(spam ~.,data=train2)
predRF2=predict(spamRF2, newdata=test2, type='prob')
confusionRF2=table(test2$spam, predRF2[,2]>0.5)
accuracyRF2=sum(diag(confusionRF2))/sum(confusionRF2)
predROCR.RF2=prediction(predRF2[,2], test2$spam)
AUCRF2=performance(predROCR.RF2, 'auc')@y.values





