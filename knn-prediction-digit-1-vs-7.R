#
#             k Nearest Neighbors 
#       Cross Validation and Prediction
#      
#      source('knn-prediction-digit-1-vs-7.R')
#          (c) EPF, Spring 2009-2018
#

# Packages

  library(class)
  library(MASS)
  library(kernlab)
  library(mlbench)
  library(reshape2)
  library(ROCR)
  
# Clear

  graphics.off()
   
# Datasets are coming from the package kernlab

  library(ElemStatLearn)
  
  data(zip.train)
  data(zip.test)
  
  id.tr  <- which(zip.train[, 1]=='1'  | zip.train[, 1]=='7')
  id.te  <- which(zip.test[,  1]=='1'  | zip.test[,  1]=='7')
  
  xtrain <- zip.train[id.tr,-1]
  ytrain <- zip.train[id.tr, 1]
  xtest  <- zip.test[id.te,-1]
  ytest  <- zip.test[id.te, 1]
  

  ntr <- nrow(xtrain)   # Sample size
  p   <- ncol(xtrain)   # Dimensionality of the input space
  nte <- nrow(xtest)

# Training confusion matrices
  
  y.tr.hat.1nn <- knn(xtrain, xtrain, ytrain, k=1) # Predicted responses in test set
  y.tr.hat.7nn <- knn(xtrain, xtrain, ytrain, k=7) # Predicted responses in test set  
  y.tr.hat.9nn <- knn(xtrain, xtrain, ytrain, k=9) # Predicted responses in test set
  
  conf.tr.1nn <- table(ytrain, y.tr.hat.1nn)
  conf.tr.7nn <- table(ytrain, y.tr.hat.7nn)
  conf.tr.9nn <- table(ytrain, y.tr.hat.9nn)
  
  print(conf.tr.1nn)
  print(conf.tr.7nn)
  print(conf.tr.9nn)
  
# Test confusion matrices
  
  y.te.hat.1nn <- knn(xtrain, xtest, ytrain, k=1) # Predicted responses in test set
  y.te.hat.7nn <- knn(xtrain, xtest, ytrain, k=7) # Predicted responses in test set  
  y.te.hat.9nn <- knn(xtrain, xtest, ytrain, k=9) # Predicted responses in test set

  conf.te.1nn <- table(ytest, y.te.hat.1nn)
  conf.te.7nn <- table(ytest, y.te.hat.7nn)
  conf.te.9nn <- table(ytest, y.te.hat.9nn)
  
  print(conf.te.1nn)
  print(conf.te.7nn)
  print(conf.te.9nn)
  
  
#
# Comparative ROC Curves on the training set
# 
  library(ROCR)
  
  ytrain.roc <- ifelse(ytrain=='1',1,0)
  
  kNN.mod <- class::knn(xtrain, xtrain, ytrain.roc, k=1, prob=TRUE)
  prob    <- attr(kNN.mod, 'prob')
  prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1
  
  pred.1NN <- prediction(prob, ytrain.roc)
  perf.1NN <- performance(pred.1NN, measure='tpr', x.measure='fpr')
  
  kNN.mod <- class::knn(xtrain, xtrain, ytrain.roc, k=7, prob=TRUE)
  prob    <- attr(kNN.mod, 'prob')
  prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1
  
  pred.7NN <- prediction(prob, ytrain.roc)
  perf.7NN <- performance(pred.7NN, measure='tpr', x.measure='fpr')
  
  kNN.mod <- class::knn(xtrain, xtrain, ytrain.roc, k=9, prob=TRUE)
  prob    <- attr(kNN.mod, 'prob')
  prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1
  
  pred.9NN <- prediction(prob, ytrain.roc)
  perf.9NN <- performance(pred.9NN, measure='tpr', x.measure='fpr')
  
  plot(perf.1NN, col=2, lwd= 2, lty=2, main=paste('Comparative ROC curves in Training'))
  plot(perf.7NN, col=3, lwd= 2, lty=3, add=TRUE)
  plot(perf.9NN, col=4, lwd= 2, lty=4, add=TRUE)
  abline(a=0,b=1)
  legend('bottomright', inset=0.05, c('1NN','7NN', '9NN'),  col=2:4, lty=2:4)
  
#
# Comparative ROC Curves on the test set
# 
  
  library(ROCR)
  
  ytest.roc <- ifelse(ytest=='1',1,0)
  
  kNN.mod <- class::knn(xtrain, xtest, ytrain.roc, k=1, prob=TRUE)
  prob    <- attr(kNN.mod, 'prob')
  prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1
  
  pred.1NN <- prediction(prob, ytest.roc)
  perf.1NN <- performance(pred.1NN, measure='tpr', x.measure='fpr')
  
  kNN.mod <- class::knn(xtrain, xtest, ytrain.roc, k=7, prob=TRUE)
  prob    <- attr(kNN.mod, 'prob')
  prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1
  
  pred.7NN <- prediction(prob, ytest.roc)
  perf.7NN <- performance(pred.7NN, measure='tpr', x.measure='fpr')
  
  kNN.mod <- class::knn(xtrain, xtest, ytrain.roc, k=9, prob=TRUE)
  prob    <- attr(kNN.mod, 'prob')
  prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1
  
  pred.9NN <- prediction(prob, ytest.roc)
  perf.9NN <- performance(pred.9NN, measure='tpr', x.measure='fpr')
  
  plot(perf.1NN, col=2, lwd= 2, lty=2, main=paste('Comparative ROC curves in Test'))
  plot(perf.7NN, col=3, lwd= 2, lty=3, add=TRUE)
  plot(perf.9NN, col=4, lwd= 2, lty=4, add=TRUE)
  abline(a=0,b=1)
  legend('bottomright', inset=0.05, c('1NN','7NN', '9NN'),  col=2:4, lty=2:4)

# 1NN
  
  id.fp.1nn.te <- which(y.te.hat.1nn == '1' & ytest=='7')

  par(mfrow=c(1,2))
  for(im in id.fp.1nn.te)
  { 
    img <- zip2image(cbind(ytest,xtest), im) 
    image(img, xaxt= "n", yaxt= "n")
  } 
  
  id.fn.1nn.te <- which(y.te.hat.1nn == '7' & ytest=='1')
  
  par(mfrow=c(2,2))
  for(im in id.fn.1nn.te)
  { 
    img <- zip2image(cbind(ytest,xtest), im) 
    image(img, xaxt= "n", yaxt= "n")
  } 
  
# 
  x <- rbind(xtrain, xtest)
  y <- c(ytrain, ytest)
  w <- c(rep('tr', ntr), rep('te', nte))
  
  z <- summary(princomp(x))$scores[,1:2]
  
  ztrain <- z[which(w=='tr'),]
  ztest  <- z[which(w=='te'),]
  
  digit.2d.tr <- data.frame(ytrain,ztrain)
  colnames(digit.2d.tr) <- c('y', 'z1', 'z2')
  digit.2d.te <- data.frame(ytest, ztest)
  colnames(digit.2d.te) <- c('y', 'z1', 'z2')
  
  ggplot(digit.2d.tr)+
    geom_text(aes( z1 , z2 , label = y , colour = factor(y)))+
    labs(x=expression(z[1]), y=expression(z[2]))+
    theme(legend.position="none") 
  
  ggplot(digit.2d.te)+
    geom_text(aes( z1 , z2 , label = y , colour = factor(y)))+
    labs(x=expression(z[1]), y=expression(z[2]))+
    theme(legend.position="none") 
  
#   
# Comparative ROC Curves on the test set
# 
  
  library(ROCR)
  
  ytest.roc <- ifelse(ytest=='1',1,0)
  
  kNN.mod <- class::knn(xtrain, xtest, ytrain.roc, k=7, prob=TRUE)
  prob    <- attr(kNN.mod, 'prob')
  prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1
  
  y.te.hat.7nn.hd <- knn(xtrain, xtest, ytrain, k=7)
  conf.te.7nn.hd  <- table(ytest, y.te.hat.7nn.hd)
  print(conf.te.7nn.hd)
  
  pred.7NN.HD <- prediction(prob, ytest.roc)
  perf.7NN.HD <- performance(pred.7NN.HD, measure='tpr', x.measure='fpr')
  
  kNN.mod <- class::knn(ztrain, ztest, ytrain.roc, k=7, prob=TRUE)
  prob    <- attr(kNN.mod, 'prob')
  prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1
  
  
  y.te.hat.7nn.2d <- knn(ztrain, ztest, ytrain, k=7)
  conf.te.7nn.2d  <- table(ytest, y.te.hat.7nn.2d)
  print(conf.te.7nn.2d)
  
  pred.7NN.2D <- prediction(prob, ytest.roc)
  perf.7NN.2D <- performance(pred.7NN.2D, measure='tpr', x.measure='fpr')
  
  plot(perf.7NN.HD, col=2, lwd= 2, lty=2, main=paste('Comparative ROC curves in Test'))
  plot(perf.7NN.2D, col=3, lwd= 2, lty=3, add=TRUE)
  abline(a=0,b=1)
  legend('bottomright', inset=0.05, c('7NN.HD', '7NN.2D'),  col=2:3, lty=2:3)
  