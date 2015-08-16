
#In this script I am going to show why people vote. 
#here we are going to load the libraries we need to solve this problem
library(ROCR)
library(rpart)
library(rpart.plot)

#Here is the data
gerber=read.csv('C:/Users/arash/Downloads/gerber.csv')

#baselinemodel

table(gerber$voting)
gerber$baseline=0
gerber.BL.pred=gerber$baseline
confusion.BL=table(gerber$voting, gerber.BL.pred)
accuracy.BL=sum(diag(confusion.BL))/sum(confusion.BL)

#We use logistic regression for voting using four treatment group as independent variables with different 
#threshold to compare acccuracy for differnet threshold. The AUC of our logistic model indicates that our 
#model doesn't improve over the baseline model 

gerber.LOG=glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family=binomial)
gerber.LOG.pred=predict(gerber.LOG, newdata=gerber, type='response')
confusion.LOG=table(gerber$voting, gerber.LOG.pred > 0.3)
accuracy_0.3=sum(diag(confusion.LOG))/sum(confusion.LOG)
gerber.ROCR.pred=prediction(gerber.LOG.pred, gerber$voting)
gerber.ROCR.perf=performance(gerber.ROCR.pred, 'auc')
AUC=gerber.ROCR.perf@y.values

# We build a CART tree for voting using all data and same four treatment variables

gerber.CART=rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0) #, minbucket= , method=# 
prp(gerber.CART)
gerber.CART2=rpart(voting ~ civicduty + hawthorne + self + neighbors+sex, data=gerber, cp=0.0)
prp(gerber.CART2, digit=6)

#Interaction Terms: We ust trees and also logistic regression to show that women in the control 
# are less likely to vote

gerber.CART3=rpart(voting~control, data=gerber, cp=0.0)
gerber.CART4=rpart(voting ~ control+sex, data=gerber, cp=0.0)
gerber.LOG2=glm(voting ~ control+sex, data=gerber, family=binomial)
gerber.LOG3=glm(voting ~ sex+control+sex:control, data=gerber, family=binomial)
possibilities=data.frame(sex=c(0, 0, 1, 1), control=c(0,1,0,1))

