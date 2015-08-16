#letter recognition
#Here are the libraries we need to work
library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(randomForest)
# we first read the data and split it in training and test data sets
letters=read.csv('C:/Users/arash/Downloads/letters_ABPR.csv')
letters$isB=as.factor(letters$letter=='B')
letters$letter=as.factor(letters$letter)
set.seed(1000)
split=sample.split(letters, SplitRatio=0.5)
test=subset(letters, split==FALSE)
train=subset(letters, split==TRUE)
# First of all we want to predict wether a letter is B or not
letter.CART.B=rpart(isB ~.-letter, data=train, method='class')
letter.RF.B=randomForest(isB ~. -letter, data=train)
#No we move on to the problem that we were orignillay intereted in, which is to predict whether or not a letter is A, B, P, or R.
set.seed(2000)
split=sample.split(letters$letter, SplitRatio=0.5)
letters$baseline=as.factor(letters$letter=='P')
train=subset(letters, split==TRUE)
test=subset(letters, split==FALSE)
letters.CART.letter=rpart(letter ~ xbox +ybox	+width+height+onpix+xbar+ybar+x2bar+y2bar+xybar+x2ybar+xy2bar+xedge
+xedgeycor+yedge+yedgexcor, data=train, method='class')
letters.RF.letter=randomForest(letter ~ xbox +ybox  +width+height+onpix+xbar+ybar+x2bar+y2bar+xybar+x2ybar+xy2bar+xedge
                          +xedgeycor+yedge+yedgexcor, data=train, method='class')

prediction=predict(letters.RF.letter, newdata=test, type='class')
confusion=table(test$letter, prediction)
accuracy=sum(diag(confusion))/sum(confusion)

