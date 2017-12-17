train=read.csv("train.csv")
test=read.csv("test.csv")
stores=read.csv("stores.csv")
features=read.csv("features.csv")
sampleSubmission=read.csv("sampleSubmission.csv")
library(dplyr)
## Store Level Aggr
Avg_store_sales=train%>%
  group_by(Store) %>%
  summarise(Avg_store_weekly_sales=mean(Weekly_Sales))%>%
  arrange(desc(Avg_store_weekly_sales))

Avg_dept_sales=train%>%
  group_by(Dept) %>%
  summarise(Avg_Dept_weekly_sales=mean(Weekly_Sales))%>%
  arrange(desc(Avg_Dept_weekly_sales))
## Store and Dept Level Aggr
train$store_dept=paste(train$Store,train$Dept,sep="_")
#head(train)
Avg_store_dept_sales=train%>%
  group_by(store_dept) %>%
  summarise(Avg_store_Dept_weekly_sales=mean(Weekly_Sales))%>%
  arrange(desc(Avg_store_Dept_weekly_sales))
####

test$store_dept=paste(test$Store,test$Dept,sep="_")

temp=merge(test,Avg_store_dept_sales,by="store_dept",all.x = T)
temp=merge(temp,Avg_dept_sales,by="Dept",all.x = T)
temp$Avg_store_Dept_weekly_sales=ifelse(is.na(temp$Avg_store_Dept_weekly_sales),
                                        temp$Avg_Dept_weekly_sales,temp$Avg_store_Dept_weekly_sales)
temp$Id=paste(temp$store_dept,temp$Date,sep="_")
subm1=data.frame(Id=temp$Id,Weekly_Sales=temp$Avg_store_Dept_weekly_sales)
sum(is.na(subm1$Weekly_Sales))
head(subm1)
write.csv(subm1,file="store_dept.csv",row.names=FALSE)
