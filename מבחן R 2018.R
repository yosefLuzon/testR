
testset<-read.csv("test.csv", header = T)
str(testset)
####1
any(is.na(testset))
install.packages('Amelia')
library(Amelia)
missmap(testset,main = "missing values", col = c("yellow","brown"))
testset$not.fully.paid<-as.factor(testset$not.fully.paid,levels=c("yes","no"))
test$purpose<-as.Date.factor(test$purpose)
install.packages("ggplot2")
library(ggplot2)

####2
###2.3
p1<-ggplot(testset,aes(x= int.rate,y=fico))
p1<-p1+geom_point(alpha=0.2)
##2.2
ggplot(testset, aes(purpose, fill = factor(not.fully.paid))) + geom_bar(position="fill")
##2.1
ggplot(testset, aes(int.rate, fill = factor(not.fully.paid))) + geom_histogram(binwidth = 0.005)   
###2.4
corp<-cbind(test$not.fully.paid,test$credit.policy)
cor.data <- cor(corp)
plot(test$not.fully.paid,test$credit.policy)
 ###3
 install.packages('rpart')
 library(rpart)
 install.packages('caTools')
 library(caTools)
 install.packages('ropart.plot')
 library(rpart.plot)
 sample <- sample.split(testset$not.fully.paid, SplitRatio = 0.7)
 train <- subset(testset, sample)
 test <- subset(testset, !sample)
 
 tree <- rpart(not.fully.paid ~ .,method = 'class' ,data = train)
 tree2 <- rpart(not.fully.paid ~ .,method = 'class' ,data = test)
 prp(tree2)
 
 
 ####4
 install.packages('dplyr')
 library(dplyr)
 str(df.test)
 ggplot(testset,aes(not.fully.paid))+geom_bar()
 ###4.1
 #The logistic regression model 
 log.model.train <- glm(not.fully.paid ~ ., family = binomial(link = logit),train)
 summary(log.model)
 ###4.2
 predicted.probabilities <- predict(log.model.train,test, type = 'response')
 predicted.values <- ifelse(predicted.probabilities>0.5,1,0)
 mean(c(TRUE, FALSE)) #TRUE - 1, FALSE - 0 
 
 misClassError <- mean(predicted.values !=test$not.fully.paid)
 ####4.3
 predict.prod<-predict(log.model.train,train,type='response')
 cm0.5=table(train$not.fully.paid,predict.prod>0.5)
 cm0.2=table(train$not.fully.paid,predict.prod>0.2)
 ###4.4
 install.packages('pROC')
 library(pROC)
 str(test)
 predictedlog<-predict(log.model.train,train,type='terms')
 rocCurve.rf<-roc(test$not.fully.paid,predictedlog[,'0'],levels =c("0","1"))
 plot(rocCurve.rf,col="purple", main = "ROC chart")
 ###

 
 
 