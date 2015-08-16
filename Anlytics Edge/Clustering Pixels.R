#Clustring Pixels

flower=read.csv('C:/Users/arash/Downloads/flower.csv', header=FALSE)
flowerMatrix=as.matrix(flower)
flowerVector=as.vector(flowerMatrix)
distance=dist(flowerVector, method='euclidean')
clusterIntensity=hclust(distance, method='ward')
flowerCluster=cutree(clusterIntensity, k=3)
dim(flowerCluster)=c(50,50)
image(flowerMatrix, axes=FALSE, col=grey(seq(0,1, length=256)))