#We load libraries here
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
# At first we do Pre-Processing on our data by removing all the punctuations and make everything 
# to a lower case. We also delete all the stopwords and apple

tweets=read.csv('C:/Users/arash/Downloads/tweets.csv', stringsAsFactors=FALSE)
tweets$Negative=as.factor(tweets$Avg<=-1)
table(tweets$Negative)
corpus=Corpus(VectorSource(tweets$Tweet))
corpus=tm_map(corpus , content_transformer(tolower))
corpus=tm_map(corpus, removePunctuation)
corpus=tm_map(corpus, removeWords, c('apple', stopwords('english')))
corpus=tm_map(corpus, stemDocument)

#Now we make a matrix of our document and we delete those terms that don't appear in about
# more than 6 tweets and then make a dataframe of it

frequencies=DocumentTermMatrix(corpus)
sparse=removeSparseTerms(frequencies, 0.995)
tweetsSparse=as.data.frame(as.matrix(sparse))
colnames(tweetsSparse)=make.names(colnames(tweetsSparse))
tweetsSparse$Negative=tweets$Negative

#here we make our test and training sets
set.seed(123)
split=sample.split(tweetsSparse$Negative, SplitRatio=0.7)
train=subset(tweetsSparse, split==TRUE)
test=subset(tweetsSparse, split==FALSE)

#After pre-proccessing our data we are ready to apply different model on our data set
#Our first modle is CART. We see that this model slighty does better predciction compare to 
# the baseline model
tweetCART=rpart(Negative ~ . ,data=train, method='class')
predictCART=predict(tweetCART, newdata=test, type='class')
confusionCART=table(test$Negative, predictCART)

#How about Random Forest Model and we see slight change in the accuracy

set.seed(123)
tweetRF=randomForest(Negative~., data=train)
predictRF=predict(tweetRF, newdata=test, type='class')
confusionRF=table(test$Negative, predictRF)
 
# Time to Logistic Regression

tweetLOG=glm(Negative ~ ., data=train, family='binomial')
predictLOG=predict(tweetLOG, newdata=test, type='response')
confusionLOG=table(test$Negative, predictLOG>0.5)
