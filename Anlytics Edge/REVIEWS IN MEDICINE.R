# Libraries we need
library(caTools)
library(tm)
library(rpart)
library(rpart.plot)
library(ROCR)
#First we load the data and then make two corpera of our data frame. Since we have two piece of 
#informations.

trials=read.csv('C:/Users/arash/Downloads/clinical_trial.csv', stringsAsFactor=FALSE)

corpusTitle=Corpus(VectorSource(trials$title))
corpusTitle=tm_map(corpusTitle, content_transformer(tolower))
corpusTitle=tm_map(corpusTitle, removePunctuation)
corpusTitle=tm_map(corpusTitle, removeWords, stopwords('english') )
corpusTitle=tm_map(corpusTitle, stemDocument)
dtmTitle=DocumentTermMatrix(corpusTitle)
dtmTitleSparse=removeSparseTerms(dtmTitle, 0.95)
dtmTitle=as.data.frame(as.matrix(dtmTitleSparse))

corpusAbstract=Corpus(VectorSource(trials$abstract))
corpusAbstract=tm_map(corpusAbstract, content_transformer(tolower))
corpusAbstract=tm_map(corpusAbstract, removePunctuation)
corpusAbstract=tm_map(corpusAbstract, removeWords, stopwords('english'))
corpusAbstract=tm_map(corpusAbstract, stemDocument)
dtmAbstract=DocumentTermMatrix(corpusAbstract)
dtmAbstractSparse=removeSparseTerms(dtmAbstract, 0.95)
dtmAbstract=as.data.frame(as.matrix(dtmAbstractSparse))

colnames(dtmTitle)=paste0('T', colnames(dtmTitle))
colnames(dtmAbstract)=paste0('A', colnames(dtmAbstract))

dtm=cbind(dtmTitle, dtmAbstract)
dtm$trial=trials$trial

# It is time to build a model for our data frame
set.seed(144)
split=sample.split(dtm$trial, 0.7)
train=subset(dtm, split==TRUE)
test=subset(dtm, split==FALSE)

#let's build a CART model to our training data set
trialCART=rpart(trial ~. , data=train, method='class')
confusionTrain=table(train$trial, predict(trialCART, type='class'))
accuracyTrian=sum(diag(confusionTrain))/sum(confusionTrain)
confusionTest=table(test$trial, predict(trialCART,newdata=test,  type='class'))
accuracyTest=sum(diag(confusionTest))/sum(confusionTest)

predROCR=prediction(predict(trialCART,newdata=test)[,2], test$trial)
perfROCR=performance(predROCR, 'auc')




