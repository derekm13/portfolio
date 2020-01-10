library(devtools)
library(githubinstall)
library(multidplyr)
library(data.table)
library(bigrquery)
library(dplyr)
library(lubridate)

project_id<-"VV-774"
sql_string<-"SELECT
PARSE_DATE('%Y%m%d',
date) AS DATE,
clientId, hits.page.pagePath,
FORMAT_TIMESTAMP('%Y-%m-%d %H:%M:%S', TIMESTAMP_SECONDS(SAFE_CAST(visitStartTime+hits.time/1000 AS INT64)), 'America/New_York') hitTimestampEST,
(
  SELECT
  value
  FROM
  t.customDimensions
  WHERE
  index = 4) Jurisdiction,
  (
  SELECT
  value
  FROM
  t.customDimensions
  WHERE 
  index = 21) HUB,
  (SELECT value FROM t.customDimensions WHERE index =12) Operating
  FROM
  `VV-774.127948911.ga_sessions_*` t,
  UNNEST(hits) AS hits
  WHERE
  REGEXP_CONTAINS(hits.page.pagePath,
  r'vpv')
  AND PARSE_DATE('%Y%m%d',
  date) >= DATE_ADD(CURRENT_DATE(),INTERVAL -30 DAY)"
vpv_issue<- query_exec(sql_string, project = project_id, use_legacy_sql = FALSE, max_pages = Inf)
write.csv(vpv_issue, "C:/Users/dminich/OneDrive/Documents/vpv_issue.csv")
vpv_issue<-read.csv("C:/Users/dminich/OneDrive/Documents/vpv_issue.csv")
test<-vpv_issue
vpv_issue<-test
#change test to vpv_issue
vpv_issue<-vpv_issue%>%filter(!is.na(HUB))

vpv_issue$hitTimestampEST<-as.POSIXct(vpv_issue$hitTimestampEST, format='%Y-%m-%d %H:%M:%S')

vpv_issue$DATE<-as.Date(vpv_issue$DATE, format = '%Y-%m-%d')
vpv_issue$HUB<-as.character(vpv_issue$HUB)
#this method may work better
vpv_issue<-vpv_issue%>%
arrange(HUB, hitTimestampEST) %>%
group_by(HUB) %>%
mutate(diff = hitTimestampEST - lag(hitTimestampEST),
diff_secs = as.numeric(diff, units = 'secs'))
  

cluster <- new_cluster(20)
vpv_issue<-vpv_issue%>%
  arrange(HUB, hitTimestampEST) %>%
  group_by(HUB) %>% partition(cluster)


vpv_issue$hitTimestampEST<-as.POSIXct(test2$hitTimestampEST, format='%Y-%m-%d %H:%M:%S')
vpv_issue<-vpv_issue%>%mutate(diff = hitTimestampEST - lag(hitTimestampEST),
                      diff_secs = as.numeric(diff, units = 'secs'))
test<-vpv_issue%>%filter(pagePath == lag(pagePath))

vpv_issue_list<-test%>%filter(diff_secs<10)

write.csv(test2, "C:/Users/dminich/OneDrive/Documents/vpv_issue_test2.csv")
