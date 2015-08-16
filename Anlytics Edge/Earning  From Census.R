#Predict Earnings From Census Data
library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
census=read.csv('C:/Users/arash/Downloads/census.csv')
set.seed(2000)
split=sample.split(census$over50k, SplitRatio=0.6)
train=subset(census, split==TRUE)
test=subset(census, split==FALSE)

#Here we build a logistic regression model to predict the "over50k" variable. We see that this logistic regression have high accuracy
# but there are so many significant variablles here for the 
census.LOG=glm(over50k ~. , data=train, family='binomial')
census.LOG.pred=predict(census.LOG, newdata=test, type='response')
ROCR.LOG.pred=prediction(census.LOG.pred, test$over50k)
ROCR.LOG.perf=performance(ROCR.LOG.pred, 'tpr', 'fpr')

# This time we want to build a classification tree to predict 'over50k'
# We observed that ROC curve for CART is less smooth than Logistic model
census.CART=rpart(over50k ~ ., data=train, cp=0.002)
census.CART.pred=predict(census.CART, newdata=test, type='class')
ROCR.CART.pred=prediction(census.CART.pred, test$over50k)
ROCR.CART.perf=performance(ROCR.CART.pred, 'auc')

# Here we build a Random Forest Model and we notice that variable age is the most used variable to split and 
#occupation gives the larger reduction in impurity

set.seed(1)
trainSmall=train[sample(nrow(train), 2000), ]
census.RF=randomForest(over50k ~. , data=trainSmall)
census.RF.pred=predict(census.RF, newdata=test)
vu=varUsed(census.RF, count=TRUE)
vusorted=sort(vu, decreasing=FALSE, index.return=TRUE)
dotchart(vusorted$x, names(census.RF$forest$xlevels[vusorted$ix]))
varImpPlot(census.RF)

#Let us select the cp parameter for our CART model using k-fold cross validation with k=10
set.seed(2)
cartGrid=expand.grid(.cp=seq(0.002, 0.1, 0.002))
control=trainControl(method='cv', number=10)
train(over50k ~., data=train, method='rpart', trControl=control, tuneGrid=cartGrid)
