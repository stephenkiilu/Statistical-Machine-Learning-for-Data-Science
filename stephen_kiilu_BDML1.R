gifted <- read.csv("~/AIMS/REVIEW PHASE/STATISTICAL MACHINE LEARNING AND BIG DATA/Datasets/gifted.csv")


##NO.4
##(1) Pairwise scatterplot
plot(gifted,lower.panel=NULL,col="tomato3")

##(2)Correlation matrix
round(cor(gifted),digits = 4)
##(3)Histogram of response
hist(gifted$score,probability=TRUE,xlab = "score",main="Histogram of score")
x=140:170
y=dnorm(x,mean = mean(gifted$score),sd=sd(gifted$score))
lines(x=x,y=y)

###shapiro test of normality
shapiro.test(gifted$score)
## (4)SRL Model
fit=lm(gifted$score~motheriq,data=gifted)
summary(fit)
##(5)Residuals
par(mfrow=c(2,2))
plot(fit,which=1)## Linear association
plot(fit,which=2)## Normality
plot(fit,which=3)## Homoscedasticity
plot(fit,which=4)## outliers
### (7)prediction and confidence bands


conf=predict (fit ,data.frame(motheriq=c(100 ) ),interval ="confidence")## confidence bands
predict=predict (fit ,data.frame(motheriq=c(100) ),interval ="prediction")## prediction bands

####### plotting confidence and prediction bands
library(ggplot2)
data1 <- cbind(gifted, conf)
data2 <-cbind(gifted, predict)

##### confidence bands
plt <- ggplot(data1, aes(motheriq,score)) +
  geom_point() +
  stat_smooth(method = lm)
plt + geom_line(aes(y = lwr), color = "tomato3", linetype = "dashed")+
  geom_line(aes(y = upr), color = "tomato3", linetype = "dashed")+
  ggtitle("Score confidence bands") 

####3 prediction bands
plt1 <- ggplot(data2, aes(motheriq,score)) +
  geom_point() +
  stat_smooth(method = lm)
plt1 + geom_line(aes(y = lwr), color = "tomato3", linetype = "dashed")+
  geom_line(aes(y = upr), color = "tomato3", linetype = "dashed")+
  ggtitle("Score Prediction bands")


###(8)MLR Model
data=gifted[,-1]
fitt=lm(gifted$score~.,data=gifted)
summary(fitt)

##################################################################################################################
### N0.2
prostate.cancer.1 <- read.csv("~/AIMS/REVIEW PHASE/STATISTICAL MACHINE LEARNING AND BIG DATA/Datasets/prostate-cancer-1.csv")
prostrate=prostate.cancer.1 ## Data loading
### (2)
y=prostate.cancer.1$Y
barplot(prop.table(table(y)),ylab="probablity",main = "Barplot of Y",col = "tomato3")## barplot of response variable
### (3)
dim(prostrate) ### dimension of the data
dim(prostrate) [1] ## sample size
dim(prostrate) [2]-1## number of variables (p), dimension of input space

###(4)
set.seed(123)
input=prostrate[,-1]## the input space
sample=input[,sample(1:10)]## sample 10 explanatory variables from the data for analysis
boxplot(sample,names = c(1:10),xlab="variable",main="Boxplot")## boxplot
##Checking for missing values
is.na(prostrate)
which(is.na(prostrate)) ##check missing values


dat=prostrate[1:10]## first 10 columns
library(corrplot)
summary(dat) ##summary statistics
plot(dat)
round(cor(dat),digits = 4)## correlation matrix in 4 decimal places

corrplot(cor(dat))## corplot 




