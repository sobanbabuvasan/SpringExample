rm(list = ls(all=TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Import libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# confusion matrix and data partitioning

library(caret)
# multicollinearity 
library(car)
# auc 
library("ROCR")
# decision tree & plotting decision tree
library(rpart)
library(rattle)
library("randomForest")
# install.packages("CHAID", repos="http://R-Forge.R-project.org")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#             Import the data set and perform preprocessing 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("D:/dat1/")
getwd()


churnDf = read.csv("D:/dat1/pychurn.csv", header=FALSE, 
                   col.names=c(features,"churn"))


churnDf <- na.omit(churnDf)
str(churnDf)

table(churnDf$churn)

churnflg <- as.numeric(churnDf$churn)
churnDf$churn <-  ifelse(churnflg==2,0,
                         ifelse(churnflg==3,1,NA))

#converting categorical data into binary #
churnDf$international.plan <- as.character(churnDf$international.plan)
churnDf$international.plan.ip <-NA #one-hot encoding
View(churnDf)
churnDf$international.plan.ip[which(churnDf$international.plan ==" yes")]=1
churnDf$international.plan.ip[which(is.na(churnDf$international.plan.ip))]=0
table(churnDf$international.plan.ip)


churnDf$voice.mail.plan.ip <- NA
churnDf$voice.mail.plan.ip[which(churnDf$voice.mail.plan ==" yes")]=1
churnDf$voice.mail.plan.ip[which(is.na(churnDf$voice.mail.plan.ip))]=0

churnDf$international.plan <- NULL
churnDf$voice.mail.plan <- NULL

table(churnDf$international.plan.ip)
table(churnDf$voice.mail.plan.ip)
table(churnDf$churn)
summary(churnDf)
# churnDf$account.length <- churnDf$account.length

ind<-createDataPartition(y = churnDf$churn,times = 1,p = 0.7,list = FALSE)
# ind <- createDataPartition(churnDf$churn,p=0.6,list=FALSE)
train <- churnDf[ind,]
test  <- churnDf[-ind,]


options(width = 100)

# lft <- glm(churnDf$churn~.,data=train,family="binomial",na.action = "na.exclude")
lft <- glm(churn~.,data=train,family = "binomial")
summary(lft)
step(lft)

confint(lft)

# To produce odds ratio 
exp(coef(lft))


vif(lft)

pred.lr1<- predict(lft,test,type="response")
library(ROCR)

pred <- prediction(pred.lr1,test$churn)
perf <- performance(pred,"tpr","fpr")
plot(perf)
auc <-  performance(pred,"auc")
auc
pred <- ifelse( pred.lr1 >.7,1,0)

# install.packages("caret",dependencies=T)

confusionMatrix(data=factor(pred),
                reference=factor(test$churn),
                positive='1')



