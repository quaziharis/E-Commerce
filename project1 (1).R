library(dplyr)
library(ggplot2)
bank
summary(bank)
str(bank)##to check the structure of data
head(bank)##to print the head of data
sum(is.na(bank))##to check the missing values in data
##data Visualization
ggplot(bank, aes(factor(y), age)) + geom_boxplot(aes(fill = factor(y)))
ggplot(bank, aes(factor(y), duration)) + geom_boxplot(aes(fill = factor(y)))
ggplot(bank, aes(factor(y), campaign)) + geom_boxplot(aes(fill = factor(y)))
ggplot(bank,aes(x=contact,fill=poutcome))+geom_bar(position = "fill")+theme_classic()
ggplot(bank,aes(x=age,fill=poutcome))+geom_bar(position = "fill")+theme_classic()
ggplot(bank,aes(x=month,fill=poutcome))+geom_bar(position = "fill")+theme_classic()
##Data Manipulation
project_data=bank
project_data_recast=project_data %>% 
  mutate(loan=factor(ifelse(loan=='yes','1','0'))) %>%
  mutate(housing=factor(ifelse(housing=='yes','1','0'))) %>%
  mutate(default=factor(ifelse(default=='yes','1','0'))) %>%
  mutate(contact=factor(ifelse(contact=='cellular','1',
                               ifelse(contact=='telephone','2','0')))) %>%
  mutate(poutcome=factor(ifelse(poutcome=='success','1',
                                ifelse(poutcome=='failure','0',
                                       ifelse(poutcome=='other','2','3')))))%>%
  mutate(y=factor(ifelse(y=='yes',1,0)))
project_data_recast
head(project_data_recast)

#### training and testing sets
set.seed(1234)####start value should not be random
create_train_test=function(data,size=0.8,train=TRUE)
{
  n_row=nrow(data)
  total_row=size*n_row
  train_sample=1:total_row
  if(train==TRUE){
    return(data[train_sample, ])
  }else{
    return(data[-train_sample, ])
  }
}
data_training=create_train_test(project_data_recast,0.8,train=TRUE)
data_testing=create_train_test(project_data_recast,0.8,train=FALSE)
dim(data_training)
dim(data_testing)
prop.table(table(data_training$y))
prop.table(table(data_testing$y))
##models
model=glm(y~.,family = 'binomial',data=data_training)
summary(model)
model1=glm(y~age+contact+month+duration+poutcome+loan,family = 'binomial',data=data_training)
model1
prediction_1=predict(model1,newdata=data_testing[1,],type='response')
prediction_1
testing[1,]
prediction_all=predict(model1,newdata=data_testing,type='response')
prediction_all 
cutoff=0.15
pred_cutoff_15=ifelse(prediction_all>cutoff,1,0)
table(data_testing$y,pred_cutoff_15)
accuracy1=(693+81)/(693+101+30+81)
accuracy1
precision1=693/(693+101)
precision1 
recall1=693/(693+30)
recall1
##model5
model2=glm(y~age+contact+month+poutcome,family = 'binomial',data=data_training)
model2
prediction_2=predict(model2,newdata=data_testing[1,],type='response')
prediction_2
testing[1,]
prediction_all=predict(model2,newdata=data_testing,type='response')
prediction_all 
cutoff=0.15
pred_cutoff_15=ifelse(prediction_all>cutoff,1,0)
table(data_testing$y,pred_cutoff_15)
accuracy2=(712+44)/(712+82+67+44)
accuracy2
precision2=712/(82+712)
precision2
recall2=712/(712+67)
recall2
####mixing of data
set.seed(678)
project_data1=project_data_recast
head(project_data1)
shuffle_index=sample(1:nrow(project_data1))
head(shuffle_index)
project_data1=project_data1[shuffle_index,]
head(project_data1)
create_train_test=function(data,size=0.8,train=TRUE)
{
  n_row=nrow(data)
  total_row=size*n_row
  train_sample=1:total_row
  if(train==TRUE){
    return(data[train_sample, ])
  }else{
    return(data[-train_sample, ])
  }
}
data_training1=create_train_test(project_data1,0.8,train=TRUE)
data_testing1=create_train_test(project_data1,0.8,train=FALSE)
dim(data_training1)
dim(data_testing1)
prop.table(table(data_training1$y))
prop.table(table(data_testing1$y))
model3=glm(y~.,family = 'binomial',data=data_training1)
summary(model3)
model4=glm(y~age+contact+month+duration+poutcome+loan,family = 'binomial',data=data_training1)
model4
prediction_4=predict(model4,newdata=data_testing1[1,],type='response')
prediction_4
data_testing1[1,]
prediction_all=predict(model4,newdata=data_testing1,type='response')
prediction_all 
cutoff=0.15
pred_cutoff_15=ifelse(prediction_all>cutoff,1,0)
table(data_testing1$y,pred_cutoff_15)
accuracy4=(718+57)/(718+98+32+57)
accuracy4
precision4=718/(718+98)
precision4
recall4=718/(718+32)
recall4 
########Decision tree
data_training1= data_training1 %>%
  mutate(marital=as.factor(marital),
         job = as.factor(job),
         month=as.factor(month))

library(rpart.plot)
fit=rpart(y~age+loan+contact+poutcome+duration,data=data_training1,method='class')
rpart.plot(fit,extra=106)
predict_dtree=predict(fit,data_testing1,type ='class')
predict_dtree
##confusion matrix
table_mat=table(data_testing1$y,predict_dtree)
table_mat
##accuracy test
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
##precision
precision=804/(804+12)
precision
##RECALL
recall=804/(800+65)
recall

