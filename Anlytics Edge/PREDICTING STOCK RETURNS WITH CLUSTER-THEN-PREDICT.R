
library(caret)
library(caTools)
library(flexclust)
#In this script we will use the data set which contains monthly stock returns 
#from the NASDAQ stock change
stocks=read.csv('C:/Users/arash/Downloads/StocksCluster.csv', header=TRUE)

#Initial Logistic Regression Model

set.seed(144)
split=sample.split(stocks$PositiveDec, SplitRatio=0.7)
train=subset(stocks, split==TRUE)
test=subset(stocks, split==FALSE)
stockLOG=glm(PositiveDec ~. , data=train, family=binomial)
predStockLOG=predict(stockLOG, newdata=test, type='response')
confusion=table(test$PositiveDec, predStockLOG>0.5)
accuracy=sum(diag(confusion))/sum(confusion)

#Let's do pre-processing on the data set

limitedTrain=train
limitedTest=test
limitedTrain$PositiveDec=NULL
limitedTest$PositiveDec=NULL
preproc=preProcess(limitedTrain)
normTrain=predict(preproc, limitedTrain)
normTest=predict(preproc, limitedTest)

#After doing preprocessing we are ready for Clustering
set.seed(144)
KMC=kmeans(normTrain, centers=3)
KMC.kcca=as.kcca(KMC, normTrain)
clusterTrain=predict(KMC.kcca)
clusterTest=predict(KMC.kcca, newdata=normTest)

#Cluster-Specific Predictions

train1=subset(train, clusterTrain==1)
train2=subset(train, clusterTrain==2)
train3=subset(train, clusterTrain==3)
test1=subset(test, clusterTest==1)
test2=subset(test, clusterTest==2)
test3=subset(test, clusterTest==3)

#Let's do another logistic regression on each cluster separatly 

stockModel1=glm(PositiveDec ~. , data=train1, family='binomial')
stockModel2=glm(PositiveDec ~., data=train2, family='binomial')
stockModel3=glm(PositiveDec ~., data=train3, family='binomial')
predTest1=predict(stockModel1, newdata=test1, type='response')
predTest2=predict(stockModel2, newdata=test2, type='response')
predTest3=predict(stockModel3, newdata=test3, type='repsonse')
AllPred=c(predTest1>0.5, predTest2>0.5, predTest3>0.5)
AllOutcomes=c(test1$PositiveDec, test2$PositiveDec, test3$PositiveDec)

