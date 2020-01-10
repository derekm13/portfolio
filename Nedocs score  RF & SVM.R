library(readxl)
library(lubridate)
library(dplyr)
library(readr)
library(caTools)
library(rmarkdown)
library(e1071)
library(caret)
library(tidyverse)
library(car)
library(ggplot2)
library(scales)
library(randomForest)
library(varhandle)


#Need to clean up the Date column to match the Components Data
#load the nedoc file
Nedoc_scores<-read.csv("C:/Users/derek/Documents/GitHub/portfolio/Nedoc_scores.csv")

#Get the date column ready
str(Nedoc_scores)
Nedoc_scores$EVENT_TIME<-as.POSIXct(Nedoc_scores$EVENT_TIME,format = '%m/%d/%y %H')
#add hour column for grouping
Nedoc_scores$Hourofday<-as.character(Nedoc_scores$EVENT_TIME, format='%H')
#created a new table to group the data and verify it was correct
Nedoc_scores<- Nedoc_scores %>%
  group_by(EVENT_TIME, Hourofday) %>%
  summarize(meanNedoc = mean(DEPT_NEDOC_SCORE))


colnames(Nedoc_scores)[1]<-"Date_and_Time"

#Now to work on the components tab
Nedoc_data<-read.csv("C:/Users/derek/Documents/GitHub/portfolio/Nedoc_data.csv")
str(Nedoc_data)
#Lets join the tables together
Nedoc_data$Date_and_Time<-as.POSIXct(Nedoc_data$Date_and_Time,format = '%m/%d/%y %H')
Nedoc_data<-left_join(Nedoc_data,Nedoc_scores, by = "Date_and_Time")
remove(Nedoc_scores)

#lets add some columns
attach(Nedoc_data)
Nedoc_data$Numofbedsavailable<-Nedoc_data$Num_of_ED_Beds - Nedoc_data$Num_of_ED_Pts
Nedoc_data$Day<-weekdays(as.Date(Nedoc_data$Date_and_Time))
Nedoc_data$Hourofday<-as.numeric(Nedoc_data$Hourofday)
Nedoc_data$timeofday<-Nedoc_data$Hourofday
#Morning 2 - 9
#Afternoon 10 - 17
#Night 18 - 1

Nedoc_data$timeofday<-recode(Nedoc_data$timeofday, "c(2,3,4,5,6,7,8,9)='Morning'")
Nedoc_data$timeofday<-recode(Nedoc_data$timeofday, "c(10,11,12,13,14,15,16,17)='Afternoon'")
Nedoc_data$timeofday<-recode(Nedoc_data$timeofday, "c(18,19,20,21,22,23,0,1)='Night'")


Nedoc_data$Overcrowded<-ifelse(Nedoc_data$meanNedoc>= 100, "Overcrowded","Not Overcrowded")

#select the data that we want
Nedoc_data_rf<-Nedoc_data%>% select(Num_of_ED_Pts,timeofday,Hourofday,Day)
Nedoc_data_rf$timeofday<-as.factor(Nedoc_data_rf$timeofday)
Nedoc_data_rf$Day<-as.factor(Nedoc_data$Day)
#run the first RF model

set.seed(123)
train_ind<- sample(nrow(Nedoc_data_rf), nrow(Nedoc_data_rf)*.75)
train<- Nedoc_data_rf[train_ind, ]
test<- Nedoc_data_rf[-train_ind, ]

rf.model<- randomForest(Num_of_ED_Pts ~ .,
                        data = train,
                        ntree = 1800,
                        type ="regression",
                        importance=TRUE)
print(rf.model)
importance<- as.data.frame(importance(rf.model))
names(importance)[names(importance)=="%IncMSE"] <- "IncMSE"
importance <- importance[order(-importance$IncMSE),]
importance


plot.data<-as.data.frame(plot(rf.model))
rf.model.prediction<- predict(object = rf.model, newdata = test)
results<-data.frame(actual = round(test$Num_of_ED_Pts,2), predicted = round(rf.model.prediction, 2))
#find the optimal number of trees
which.min(rf.model$mse)
#find the RMSE
rmse<-sqrt(mean((test$Num_of_ED_Pts-rf.model.prediction)^2))
rmse
#RF model number 2
#remove columns
Nedoc_data_rf2<-Nedoc_data%>%select(Num_of_ED_Pts,Num_of_ED_Pts_Waiting_IP_Bed,Num_of_Critical_Care_Pts,Door_to_Bed_Time_for_Last_ED_Patient,
                                    Longest_Admit_Time_Waiting_in_ED,Hourofday, meanNedoc, Numofbedsavailable, Day, timeofday)
Nedoc_data_rf2$Day<-as.factor(Nedoc_data_rf2$Day)
Nedoc_data_rf2$timeofday<-as.factor(Nedoc_data_rf2$timeofday)

set.seed(123)
train_ind<- sample(nrow(Nedoc_data_rf2), nrow(Nedoc_data_rf2)*.75)
train2<- Nedoc_data_rf2[train_ind, ]
test2<- Nedoc_data_rf2[-train_ind, ]

rf.model2<- randomForest(meanNedoc ~ .,
                        data = train2,
                        ntree = 1000,
                        type ="regression",
                        importance=TRUE)
print(rf.model2)
importance2<- as.data.frame(importance(rf.model2))
names(importance2)[names(importance2)=="%IncMSE"] <- "IncMSE"
importance2<- importance2[order(-importance2$IncMSE),]
importance2


plot.data2<-as.data.frame(plot(rf.model2))
rf.model.prediction2<- predict(object = rf.model2, newdata = test2)
results2<-data.frame(actual = round(test2$meanNedoc,2), predicted = round(rf.model.prediction2, 2))
#find optimal number of trees
which.min(rf.model2$mse)
#Find RMSE
rmse2<-sqrt(mean((test2$meanNedoc-rf.model.prediction2)^2))
rmse2

#SVM model
#remove columns
Nedoc_data_svm<-Nedoc_data%>%select(Num_of_ED_Pts, Num_of_ED_Pts_Waiting_IP_Bed, Num_of_Critical_Care_Pts, Door_to_Bed_Time_for_Last_ED_Patient,
                                    Longest_Admit_Time_Waiting_in_ED, Hourofday, meanNedoc, Day, timeofday, Overcrowded)

#convert categorical data into dummy variables

svm_cat<-Nedoc_data_svm %>% select(Day)
svm_cat<-to.dummy(svm_cat$Day, "Day")
svm_cat<-as.data.frame(svm_cat)

svm_cat1<-Nedoc_data_svm %>% select(timeofday)
svm_cat1<-to.dummy(svm_cat1$timeofday, "Timeofday")
svm_cat1<-as.data.frame(svm_cat1)
#add these two tables back into the main table
Nedoc_data_svm<-cbind(Nedoc_data_svm,svm_cat,svm_cat1)
Nedoc_data_svm$Day<-NULL
Nedoc_data_svm$timeofday<-NULL


Nedoc_data_svm$Overcrowded<-as.factor(Nedoc_data_svm$Overcrowded)
train_ind<- sample(nrow(Nedoc_data_svm), nrow(Nedoc_data_svm)*.70)
train_svm<- Nedoc_data_svm[train_ind, ]
test_svm<- Nedoc_data_svm[-train_ind, ]

SVM<-svm(Overcrowded ~ ., data = train_svm, kernel ='radial', cost = 10, gamma = .5)
print(SVM)

predict_svm<- predict(SVM,test_svm)
table(predict_svm, test_svm$Overcrowded)
confusionMatrix(train_svm$Overcrowded, predict(SVM), positive = "Overcrowded")

#accuracy
accuracy<-(98+89)/(98+22+15+89)
print(accuracy)
#precision
precision<-(89)/(89+15)
print(precision)


