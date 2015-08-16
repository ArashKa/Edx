library(tm)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)

#Let's read the data and do pre-proccessing on this data

wiki=read.csv('C:/Users/arash/Downloads/wiki.csv', stringsAsFactors=FALSE)
wiki$Vandal=as.factor(wiki$Vandal)
corpusAdded=Corpus(VectorSource(wiki$Added))
corpusAdded=tm_map(corpusAdded, removeWords, stopwords('english'))
corpusAdded=tm_map(corpusAdded, stemDocument)
corpusAdded=tm_map(corpusAdded, content_transformer(tolower))
dtmAdded=DocumentTermMatrix(corpusAdded)

#Added Bags of Words

SparseAdded=removeSparseTerms(dtmAdded, 0.997)
WordsAdded=as.data.frame(as.matrix(SparseAdded))
colnames(WordsAdded)=paste('A', colnames(WordsAdded))

#Removed Bag of Words

corpusRemoved=Corpus(VectorSource(wiki$Removed))
corpusRemoved=tm_map(corpusRemoved, content_transformer(tolowern))
corpusRemoved=tm_map(corpusRemoved, removeWords, stopwords('english'))
corpusRemoved=tm_map(corpusRemoved, stemDocument)
dtmRemoved=DocumentTermMatrix(corpusRemoved)
SparseRemoved=removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved=as.data.frame(as.matrix(SparseRemoved))
colnames(wordsRemoved)=paste('R', colnames(wordsRemoved))


#Now we just make one data frame which is the combination of last two data frames
# and also training and test sets

wikiWords=cbind(WordsAdded, wordsRemoved)
wikiWords$Vandal=wiki$Vandal
set.seed(123)
split=sample.split(wikiWords$Vandal, 0.7)
train=subset(wikiWords, split==TRUE)
test=subset(wikiWords, split==FALSE)

#I am building a CART model to predict Vandal, using all other vars as independent
# variable. We were not able to improve on the baseline model using the raw textual 
# information and CART model

wikiCART=rpart(Vandal ~. ,  data=train, method='class')
wikiCART.pred=predict(wikiCART, newdata=test, type='class')
confusion=table(test$Vandal, wikiCART.pred)
accuracy=sum(diag(confusion))/sum(confusion)

#Here we are going to use Website addresses for prediction. We see just a slight 
#imporvment over the CART
wikiWords2=wikiWords
wikiWords2$HTTP=ifelse(grepl('http', wiki$Added, fixed=TRUE), 1, 0)
wikiWords2$NumWordsAdded=rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved=rowSums(as.matrix(dtmRemoved))
train2=subset(wikiWords2, split==TRUE)
test2=subset(wikiWords2, split==FALSE)
wikiCART2=rpart(Vandal ~. , data=train2, method='class')
wikiCART2.pred=predict(wikiCART2, test2, type='class')
confusion2=table(test2$Vandal, wikiCART2.pred)
accuracy2=sum(diag(confusion2))/sum(confusion2)

# We have 2 pieces of 'metadata'(data about data) that we haven't used yet. 
#In our next CART model we are going to use these variables for predictions

wikiWords3=wikiWords2
wikiWords3$Minor=wiki$Minor
wikiWords3$Loggedin=wiki$Loggedin
train3=subset(wikiWords3, split==TRUE)
test3=subset(wikiWords3, split==FALSE)
wikiCART3=rpart(Vandal ~. , data=train3, method='class')
wikiCART3.pred=predict(wikiCART3, test3, type='class')
confusion3=table(test3$Vandal, wikiCART3.pred)
accuracy3=sum(diag(confusion3))/sum(confusion3)


