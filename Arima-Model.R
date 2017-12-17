train=read.csv("train.csv")
test=read.csv("test.csv")
stores=read.csv("stores.csv")
features=read.csv("features.csv")
sampleSubmission=read.csv("sampleSubmission.csv")
train$Str_dept=paste(train$Store,train$Dept,sep="_")
test$Str_dept=paste(test$Store,test$Dept,sep="_")
head(train)
train_store_dept_11=train[train$Str_dept=="1_1",]
test_store_dept_11=test[test$Str_dept=="1_1",]
dim(train_store_dept_11)
dim(test_store_dept_11)
myts <- ts(train_store_dept_11$Weekly_Sales,frequency=52) ## Holwinter
fit=HoltWinters(myts, beta = F,gamma=FALSE)  ## Holwinter
plot(fit)
library(forecast)
myts <- ts(train_store_dept_11$Weekly_Sales,frequency=52)
#plot(myts)
fit <- stlf(myts, s.window="period",method="arima")
#summary(fit)
summary(fit)
store_dept_key_test=unique(test$Str_dept)
store_dept_key_train=unique(train$Str_dept)
common_key=intersect(store_dept_key_test,store_dept_key_train)
head(common_key)
length(common_key)
op_all=c()
for(i in 1:length(common_key))
{
  store_dept_sub_train<-train[train$Str_dept==common_key[i],c("Weekly_Sales","Date")]
  store_dept_sub_test<-test[test$Str_dept==common_key[i],]
  myts <- ts(store_dept_sub_train$Weekly_Sales,frequency=52)
  lead_time=nrow(store_dept_sub_test)
  fit <- try(stlf(myts, s.window="period",h=lead_time,method="arima"),silent = T)
  summary(fit)
  plot(fit)
  fit[[4]]
  pred<-try(as.numeric(fit[[4]]),silent = T)
  store_dept_sub_test$Weekly_Sales=pred
  store_dept_sub_test$Id=paste(store_dept_sub_test$Str_dept,store_dept_sub_test$Date,sep="_")
  op=store_dept_sub_test[,c("Id","Weekly_Sales")]
  op_all=rbind(op_all,op)
  print(i)
}
op_all_1=op_all[op_all$Weekly_Sales!="Error in fit[[4]] : subscript out of bounds\n",]
sampleSubmission3=merge(subm1,op_all_1,by="Id",all.x = TRUE)
sum(is.na(sampleSubmission3$Weekly_Sales.y))
sampleSubmission3$Weekly_Sales=ifelse(is.na(sampleSubmission3$Weekly_Sales.y),sampleSubmission3$Weekly_Sales.x,sampleSubmission3$Weekly_Sales.y)
sum(is.na(sampleSubmission3$Weekly_Sales))
STLF_sub=sampleSubmission3[,c("Id","Weekly_Sales")]
write.csv(STLF_sub,file="ARIMA_using_stlf_stepwise.csv",row.names=F)
