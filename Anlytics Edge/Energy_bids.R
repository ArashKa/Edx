# Here is the list of libraries we need to use
library(rpart)
library(rpart.plot)
library(tm)
library(caTools)
library(ROCR)
#We do pre-processing on the data file 
emails=read.csv('C:/Users/arash/Downloads/energy_bids.csv', stringsAsFactors=FALSE)
corpus=Corpus(VectorSource(emails$email))
corpus=tm_map(corpus, content_transformer(tolower))
corpus=tm_map(corpus, removePunctuation)
corpus=tm_map(corpus, removeWords, stopwords('english'))
corpus=tm_map(corpus, stemDocument)

# We make a Documnet Term Matrix

dtm=DocumentTermMatrix(corpus)
dtm=removeSparseTerms(dtm, 0.97)
labelTerms=as.data.frame(as.matrix(dtm))
labelTerms$responsive=emails$responsive

#Here we make training and test sest
set.seed(144)
split=sample.split(labelTerms$responsive, 0.7)
train=subset(labelTerms, split==TRUE)
test=subset(labelTerms, split==FALSE)

# Now we build a CART model to our training set and compare it to the baseline model
emailCART=rpart(responsive ~. , data=train, method='class')
predCART=predict(emailCART, newdata=test)
predCART.prob=predCART[,2]
confusion=table(test$responsive, predCART.prob>=0.5)
accuracy=sum(diag(confusion))/sum(confusion)
predROCR=prediction(predCART.prob, test$responsive)
perfROCR=performance(predROCR, 'tpr', 'fpr')

