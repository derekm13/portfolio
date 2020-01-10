library(dplyr) 
library(tidyverse) 
library(stargazer) 
library(descr) 
library(forcats) 
library(e1071)
library(caret)
library(randomForest)

loan <- read.csv("D:/derek/Documents/Loan_status.csv")

loan <- loan%>%filter(!loan_status=="")
loan$status <- ifelse(loan$loan_status=="Current" |
                        loan$loan_status=="Fully Paid" |
                        loan$loan_status=="Does not meet the credit policy. Status:Fully Paid","good","bad")
loan$status <- as.factor(loan$status)

loan$fico <- (loan$fico_range_high+loan$fico_range_low)/2
summary(loan$fico)
loan$ficocat <- cut(loan$fico, breaks=c(0,687,742,1000),
                    labels=c("bottom 25%","middle 50%", "top 25%"))
table(loan$ficocat)
summary(loan$dti)
loan$dticat <- cut(loan$dti, breaks=c(0,8.2,18.68,100),
                   labels=c("bottom 25%","middle 50%", "top 25%"))
table(loan$dticat)
table(loan$purpose)

loan$purpose <- factor(loan$purpose)
levels(loan$purpose)

loan$purpose <- fct_collapse(loan$purpose, other=c("renewable_energy", "other"))
levels(loan$purpose)
table(loan$purpose)

loan <- select(loan, status, ficocat, dticat, purpose)
set.seed(364)
loan$rand <- runif(nrow(loan))
train <- filter(loan, rand<=0.8)
test <- filter(loan, rand>0.8)

classifier <- naiveBayes(status ~ ficocat+dticat+purpose,train)
classifier
prediction <- predict(classifier, select(test, ficocat, dticat, purpose), type="raw")
summary(prediction)

test$status_pred <- ifelse(prediction[,"good"] > 0.65, "good", "bad")
table(test$status_pred)

test1<-factor(test$status)
test2<-factor(test$status_pred)
confusionMatrix(test2, test1, positive = 'good')

