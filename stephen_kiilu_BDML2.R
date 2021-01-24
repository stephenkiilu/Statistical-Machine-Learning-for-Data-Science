






library(class)
library(MASS)
library(kernlab)
library(mlbench)
library(reshape2)
library(ROCR)
library(ggplot2)



### Load data
prostate.cancer.1 <- read.csv("~/AIMS/REVIEW PHASE/STATISTICAL MACHINE LEARNING AND BIG DATA/Datasets/prostate-cancer-1.csv")
prostate=prostate.cancer.1 ##data

head(prostate)###first 5 rows
tail(prostate)###last 5 rows


n=nrow(prostate)##sample size
p=ncol(prostate)-1 ##
y=prostate[,1]##Response variable
x=prostate[,-y]###Data matrix
dim(x)## Dimension of data

y.te=y## we use whole data set
y.tr=y## we use whole data set
x.te=x## we use whole data set
x.tr=x## we use whole data set

###### 1-NN
k=1   
y.te.hat=knn(x,x,y, k=1)##True responses in test set
confusion.mat.te<- table(y.te, y.te.hat) ### Predicted responses in test
confusion.mat.te
###### 3-NN
k=3   
y.te.hat=knn(x,x,y, k=k)##True responses in test set
confusion.mat.te <- table(y.te, y.te.hat) ### Predicted responses in test
confusion.mat.te 
###### 5-NN
k=5  
y.te.hat=knn(x,x,y, k=k)##True responses in test set
confusion.mat.te <- table(y.te, y.te.hat) ### Predicted responses in test
confusion.mat.te 

###### 7-NN
k=7  
y.te.hat=knn(x,x,y, k=k)##True responses in test set
confusion.mat.te <- table(y.te, y.te.hat) ### Predicted responses in test
confusion.mat.te 


# 
library(ROCR)###receiver operating characteristic to compare different classifiers
kNN.mod <- class::knn(x, x, y, k=1, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.1NN <- prediction(prob, y)
perf.1NN <- performance(pred.1NN, measure='tpr', x.measure='fpr')

kNN.mod <- class::knn(x, x, y, k=3, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.3NN <- prediction(prob, y)
perf.3NN <- performance(pred.3NN, measure='tpr', x.measure='fpr')

kNN.mod <- class::knn(x, x, y, k=5, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.5NN <- prediction(prob, y)
perf.5NN <- performance(pred.5NN, measure='tpr', x.measure='fpr')

kNN.mod <- class::knn(x, x, y, k=7, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.7NN <- prediction(prob, y)
perf.7NN <- performance(pred.7NN, measure='tpr', x.measure='fpr')

plot(perf.1NN, col=2, lwd= 2, lty=2, main=paste('comparative ROC curve'))
plot(perf.3NN, col=3, lwd= 2, lty=3, add=TRUE)
plot(perf.5NN, col=4, lwd= 2, lty=4, add=TRUE)
plot(perf.7NN, col=5, lwd= 2, lty=5, add=TRUE)
abline(a=0,b=1)
legend('bottomright', inset=0.05, c('1NN','3NN','5NN', '7NN'),  col=2:5, lty=2:5)



####Question 3
set.seed(19671210)##random numbers
e=1/3 ####  # Proportion of observations in the test set in the split
nte =round(n*e)##number observations in test set
ntrain=n-ntest ##number observations in training set
R=100   # Number of replications
test.err=matrix(0, nrow=R, ncol=4)


for(r in 1:R)
{
  # Split the data
  
  id.tr   <- sample(sample(sample(n)))[1:ntr]     
  id.te   <- setdiff(1:n, id.tr)
  y.te         <- y[id.te]                                        # True responses in test set
  
  # Machine 1: 1NN
  
  y.te.hat     <- knn(x[id.tr,], x[id.te,], y[id.tr], k=1)        # Predicted responses in test set
  ind.err.te   <- ifelse(y.te!=y.te.hat,1,0)                      # Random variable tracking error. Indicator
  test.err[r,1]  <- mean(ind.err.te)
  
  # Machine 2: 2NN
  y.te.hat     <- knn(x[id.tr,], x[id.te,], y[id.tr], k=3)        # Predicted responses in test set
  ind.err.te   <- ifelse(y.te!=y.te.hat,1,0)                      # Random variable tracking error. Indicator
  test.err[r,2]  <- mean(ind.err.te)
  # Machine 3: 5NN
  y.te.hat     <- knn(x[id.tr,], x[id.te,], y[id.tr], k=5)        # Predicted responses in test set
  ind.err.te   <- ifelse(y.te!=y.te.hat,1,0)                      # Random variable tracking error. Indicator
  test.err[r,3]  <- mean(ind.err.te)
  # Machine 4: 7NN
  y.te.hat     <- knn(x[id.tr,], x[id.te,], y[id.tr], k=7)        # Predicted responses in test set
  ind.err.te   <- ifelse(y.te!=y.te.hat,1,0)                      # Random variable tracking error. Indicator
  test.err[r,4]  <- mean(ind.err.te)
}
test <- data.frame(test.err)
Method<-c('1NN', '3NN', '5NN', '7NN')
colnames(test) <- Method
boxplot(test)


require(reshape2)
ggplot(data = melt(test), aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))+
  labs(x='Learning Machine', y="Test Error")+
  theme(legend.position="none")+ggtitle("Test error Boxplots")
