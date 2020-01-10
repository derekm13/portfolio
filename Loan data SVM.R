library(ggplot2)
library(readr)
library(caTools)
library(rmarkdown)
library(e1071)
library(ggplot2)


loans <- read.csv("D:/derek/Documents/loan_data.csv")
summary(loans)
str(loans)
loans$credit.policy <- as.factor(loans$credit.policy)
loans$inq.last.6mths <-as.factor(loans$inq.last.6mths)
loans$delinq.2yrs <- as.factor(loans$delinq.2yrs)
loans$pub.rec <- as.factor(loans$pub.rec)
loans$not.fully.paid <- as.factor(loans$not.fully.paid)

ggplot(loans, aes(fico)) + geom_histogram(aes(fill=not.fully.paid),
color='black') + theme_bw()

ggplot(loans, aes(factor(purpose))) + geom_bar(aes(fill=not.fully.paid),
position='dodge') + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(loans, aes(int.rate, fico)) + geom_point(aes(color=not.fully.paid), 
                                                alpha=0.5) + theme_bw()

set.seed(123)
sample <-sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train <- subset(loans, sample == TRUE)
test <- subset(loans, sample == FALSE)

model <- svm(not.fully.paid ~., data = train[1:14])
summary(model)

predict.values <- predict(model,test[1:13])
table(predict.values, test$not.fully.paid)

tuned.svm1<-tune.svm(not.fully.paid ~., data=train[1:14], cost = 10^(-3:2), gamma = 10^(-5:-1))
summary(tuned.svm1)

tuned.svm <-svm(not.fully.paid ~., data=train[1:14], kernal='radial', cost =80,gamma=0.1)
predicted.values <- predict(tuned.svm, test[1:13])
table(predicted.values, test$not.fully.paid)


accuracy <- (2211 + 78)/(2211 + 382 + 202 + 78)
print(accuracy)

precision <- 2211/(2211+382)
print(precision)

recall <- 2211/(2211+202)
print(recall)


