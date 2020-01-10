library(dplyr)
library(reshape2)
library(tidyr)
library(bupaR)
library(processmapR)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(multidplyr)
library(readxl)
library(data.table)
options(scipen = 999)
#----Step1----#
start_service<-read.csv("C:/Users/dminich/OneDrive/Documents/start_service_research_2.csv")
names(start_service)[1]<-"date"
start_service$date<-as.Date(start_service$date, format = "%m/%d/%Y")
start_service$clientId<-as.character(start_service$clientId)
timestamp<-start_service%>%select(date, sessionId)
#----Step2----#
start<-melt(start_service, id.vars = c("sessionId"),measure.vars = 4:24)
start<-start[order(start$sessionId),]
start<-start%>%filter(!value=='')
start<-inner_join(start, timestamp, by = c("sessionId"="sessionId"))
start$sessionId<-as.character(start$sessionId)
#----Step3----#
clientid<-start_service%>%select(clientId,sessionId)
clientid$sessionId<-as.character(clientid$sessionId)
Startstopmove<-inner_join(start, clientid, by = c("sessionId","sessionId"))%>%distinct(sessionId,variable,.keep_all = T)%>%filter(!is.na(clientId))
remove(timestamp, clientid)
#----Step4----#
cluster<-new_cluster(6)
Starteventlog<-Startstopmove%>%partition(cluster)%>%simple_eventlog(case_id = "clientId", activity_id = "value", timestamp = "date", order = "auto")
tracestarteeventlog<-Starteventlog%>%traces()
pm1<-Starteventlog%>%filter_trace_frequency(interval = c(500,NA))%>%process_map()
Starteventlog%>%filter_activity_frequency(interval = c(10000,NA))%>%process_map()

#----Prev pages only----#
beforestart<-Startstopmove%>%filter(!variable %in% c("nextPageTen","nextPageNine","nextPageEight","nextPageSeven","nextPageSix","nextPageFive","nextPageFour",
                                                     "nextPageThree","nextPageTwo","nextPageOne"))
eventlog<-beforestart%>%partition(cluster)%>%simple_eventlog(case_id = "sessionId", activity_id = "value", timestamp = "date", order = "auto")
logfreq<-eventlog%>%activity_frequency(level = "activity")
tracetest<-eventlog%>%filter_activity_frequency(interval = c(5000,NA))%>%traces()
logtraces<-eventlog%>%traces()
eventlog%>%filter_trace_frequency(interval = c(1000,NA))%>%process_map()
eventlog%>%filter_activity_frequency(interval = c(10000,NA))%>%process_map()
eventlog%>%filter_activity_frequency(interval = c(5000,NA))%>%filter_trace_frequency(interval = c(1000,NA))%>%process_map()


#authenticated start service
start_service_auth<-read_excel("C:/Users/dminich/OneDrive/Documents/Start_service_auth.xlsx", sheet = 1)
start_service_auth<-start_service_auth%>%filter(!is.na(clientId))
timestamp<-start_service_auth%>%select(date, sessionId)
clientid<-start_service_auth%>%select(clientId,sessionId)

start_auth<-melt(start_service_auth, id.vars = c("sessionId"),measure.vars = 4:24)
start_auth<-start_auth[order(start_auth$sessionId),]
start_auth<-start_auth%>%filter(!value=='')
start_auth<-inner_join(start_auth, timestamp, by = c("sessionId"="sessionId"))
start_auth<-inner_join(start_auth, clientid, by = c("sessionId","sessionId"))
start_auth<-start_auth%>%distinct(sessionId,variable,.keep_all = T)
remove(timestamp,clientid)

cluster<-new_cluster(6)
eventlog_auth<-start_auth%>%partition(cluster)%>%simple_eventlog(case_id = "clientId", activity_id = "value", timestamp = "date", order = "auto")
eventlog_auth%>%filter_activity_frequency(interval = c(10000,NA))%>%process_map()
eventlog_auth%>%filter_activity_frequency(interval = c(10000,NA))%>%filter_trace_frequency(interval = c(10,NA))%>%process_map()
trace_auth<-eventlog_auth%>%filter_activity_frequency(interval = c(10000,NA))%>%filter_trace_frequency(interval = c(10,NA))
test<-eventlog_auth%>%activity_frequency(level = "activity")
test2<-eventlog_auth%>%filter_activity_frequency(interval = c(10000,NA))%>%traces()
eventlog_auth%>%filter_(interval = 3000)%>%process_map()


#Before start Page Auth#
beforestart_auth<-start_auth
beforestart_auth<-beforestart_auth%>%filter(!variable %in% c("nextPageTen","nextPageNine","nextPageEight","nextPageSeven","nextPageSix","nextPageFive","nextPageFour",
                                                             "nextPageThree","nextPageTwo","nextPageOne"))

beforestart_auth<-beforestart_auth%>%filter(!value==lag(value) | is.na(lag(value)))
beforestart_auth$order<-seq.int(nrow(beforestart_auth))
eventlog_auth<-beforestart_auth%>%partition(cluster)%>%simple_eventlog(case_id = "clientId", activity_id = "value", timestamp = "date", order = "order")
logfreq_auth<-eventlog_auth%>%activity_frequency(level = "activity")
test4<-eventlog_auth%>%filter_activity_frequency(interval = c(1000,NA))%>%traces()
logtraces_auth<-eventlog_auth%>%traces()
#----#
eventlog_auth%>%filter_trace_frequency(interval = c(50,NA))%>%process_map()
#----#
eventlog_auth%>%filter_activity_frequency(interval = c(2000,NA))%>%process_map()
filteredtrace<-eventlog_auth%>%filter_activity_frequency(interval = c(1000,NA))%>%traces()
eventlog_auth%>%filter_activity_frequency(interval = c(1000,NA))%>%filter_trace_frequency(interval = c(100,NA))%>%process_map()

