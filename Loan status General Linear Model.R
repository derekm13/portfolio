library(tidyverse)
library(stargazer)
library(descr)
library(dplyr)
library(ggplot2)
library(class)

loan <- read.csv("D:/derek/Documents/Loan_status.csv")

table(loan$loan_status)
loan <- loan%>%filter(!loan$loan_status=='')
loan$status <- ifelse(loan$loan_status == "Current" | 
                        loan$loan_status == "Fully Paid" |
                        loan$loan_status == "Does not meet the credit policy. Status:Fully Paid","good","bad")
table(loan$status)
loan$int_rate <- as.numeric(gsub("%","", loan$int_rate))
ggplot(loan, aes(x=status,y=int_rate)) + geom_boxplot()
crosstab(loan$home_ownership, loan$status, prop.r = TRUE, plot = FALSE)
loan$fico <- (loan$fico_range_high+loan$fico_range_low)/2


stargazer(select(filter(loan, status == "good"),dti, fico), median = TRUE, type = "text")

stargazer(select(filter(loan, status == "bad"),dti, fico), median = TRUE, type = "text")

ggplot(aes(x = dti, color = status) ,data = loan) + geom_density()
ggplot(aes(x = fico, color = status) ,data = loan) + geom_density()

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

loan$fico_n <- normalize(loan$fico)
loan$dti_n <- normalize(loan$dti)
summary(select(loan, fico, fico_n))


set.seed(364)
sample <- sample(nrow(loan),floor(nrow(loan)*0.8))
head(sample)

train <- loan[sample,]
test <- loan[-sample,]
prop.table(table(train$status))
prop.table(table(test$status))

train_knn <- select(train, fico_n, dti_n)
test_knn <- select(test, fico_n, dti_n)


pred <- knn(train_knn, test_knn, train$status, k = 8)
head(pred)

crosstab(test$status, pred, prop.t = TRUE, plot=FALSE)

#logistic regression

loan$status <- as.factor(loan$status)
logit1 <- glm(status ~ fico, data = loan, family = "binomial")
summary(logit1)

exp(coef(logit1))

test <- data.frame(fico=c(700,750))
test$pred <- predict(logit1,test, type="response")
test

logit2 <- glm(status ~ fico + loan_amnt, data = loan, family = "binomial")
summary(logit2)

exp(coef(logit2))

logit3 <- glm(status ~ fico + loan_amnt + purpose, data = loan, family = "binomial")
summary(logit3)
round(exp(coef(logit3)),3)


logit4 <- glm(status ~ fico + loan_amnt + loan$int_rate + loan$annual_inc, data = loan, family = "binomial")
summary(logit4)
exp(coef(logit4))
coef(logit4)
