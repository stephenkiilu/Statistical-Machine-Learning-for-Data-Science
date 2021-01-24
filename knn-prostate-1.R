#
# source('knn-prostate-1.R') 
#

  xy <- read.csv('prostate-cancer-1.csv')
  
  barplot(prop.table(table(xy[,1])))
  
  x <- xy[,-1]
  y <- xy[, 1]
  
  #
  # Comparative ROC Curves on the training set
  # 
  library(ROCR)
  
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
  
  plot(perf.1NN, col=2, lwd= 2, lty=2, main=paste('Comparative ROC curves in Training'))
  plot(perf.3NN, col=3, lwd= 2, lty=3, add=TRUE)
  plot(perf.5NN, col=4, lwd= 2, lty=4, add=TRUE)
  plot(perf.7NN, col=5, lwd= 2, lty=5, add=TRUE)
  abline(a=0,b=1)
  legend('bottomright', inset=0.05, c('1NN','3NN', '5NN', '7NN'),  col=2:5, lty=2:5)
  
  n <- nrow(x)
  
# Random replicated splits into training and test sets
  
  # Split the data
  
  set.seed (19671210) # Set seed for random number generation to be reproducible
  
  epsilon <- 1/3               # Proportion of observations in the test set
  nte     <- round(n*epsilon)  # Number of observations in the test set
  ntr     <- n - nte
  
  R <- 500   # Number of replications
  test.err <- matrix(0, nrow=R, ncol=4)
  
  for(r in 1:R)
  {
    # Split the data
    
    id.tr   <- sample(sample(sample(n)))[1:ntr]                   # For a sample of ntr indices from {1,2,..,n}
    id.te   <- setdiff(1:n, id.tr)
    
    y.te         <- y[id.te]                                        # True responses in test set
    
    # First machine: 1NN
    
    y.te.hat     <- knn(x[id.tr,], x[id.te,], y[id.tr], k=1)        # Predicted responses in test set
    ind.err.te   <- ifelse(y.te!=y.te.hat,1,0)                      # Random variable tracking error. Indicator
    test.err[r,1]  <- mean(ind.err.te)
    
    # Second machine: 3NN 
    y.te.hat     <- knn(x[id.tr,], x[id.te,], y[id.tr], k=3) # Predicted responses in test set
    ind.err.te   <- ifelse(y.te!=y.te.hat,1,0)                      # Random variable tracking error. Indicator
    test.err[r,2]  <- mean(ind.err.te)
    
    # Third machine: 5NN
    y.te.hat     <- knn(x[id.tr,], x[id.te,], y[id.tr], k=5)       # Predicted responses in test set
    ind.err.te   <- ifelse(y.te!=y.te.hat,1,0)                      # Random variable tracking error. Indicator
    test.err[r,3]  <- mean(ind.err.te)

    # Fourth machine: 7NN
    y.te.hat     <- knn(x[id.tr,], x[id.te,], y[id.tr], k=7)       # Predicted responses in test set
    ind.err.te   <- ifelse(y.te!=y.te.hat,1,0)                      # Random variable tracking error. Indicator
    test.err[r,4]  <- mean(ind.err.te)
    
  }  
  
  test <- data.frame(test.err)
  Method<-c('1NN', '3NN', '5NN', '7NN')
  colnames(test) <- Method
  boxplot(test)
  
  
  require(reshape2)
  ggplot(data = melt(test), aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))+
    labs(x='Method', y=expression(hat(R)[te](kNN)))+
    theme(legend.position="none") 
  
  #  
  # Is the difference between the methods significant
  #  
  aov.method <- aov(value~variable, data=melt(test))
  anova(aov.method)
  #summary(aov.method)
  
  TukeyHSD(aov.method, ordered = TRUE)
  plot(TukeyHSD(aov.method))
  