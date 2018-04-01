print ("Om Vakratuntaya Namah")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("D:/dat1/") #set your working directory by replacing folder location
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

churndata <- read.csv("churn.csv",sep = ",",header = TRUE,na.strings = c(NA,"")) #command to import dataset
prop.table(table(churndata$Churn)) #check percentage of churners and non churners in dataset

str(churndata)#checking structure of our dataset

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Preparation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
churndata$Churn<-factor(churndata$Churn,levels = c("1","0")) #converting target variable to factor

#converting categorical data into binary #
churndata$Int.l.Plan_ohe<-NA #one-hor encoding
churndata$Int.l.Plan_ohe[which(churndata$Int.l.Plan=="yes")]=1
churndata$Int.l.Plan_ohe[which(is.na(churndata$Int.l.Plan_ohe))]=0

# validating transformation
table(churndata$Int.l.Plan)
table(churndata$Int.l.Plan_ohe)
churndata$Int.l.Plan<-NULL #removing categorical variable

churndata$VMail.Plan_ohe <-NA
churndata$VMail.Plan_ohe[which(churndata$VMail.Plan=="yes")]=1
churndata$VMail.Plan_ohe[which(is.na(churndata$VMail.Plan_ohe))]=0

#validating transformation
table(churndata$VMail.Plan)
table(churndata$VMail.Plan_ohe)
churndata$VMail.Plan<-NULL

row.names(churndata)<-churndata$Phone
churndata$Phone<-NULL

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ploting distribution of all variable 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hist(churndata$Account.Length,col = "red",main = "Account length")

# hist(log(churndata$VMail.Message),col = "red",main = "No of Voice Mail Messages",breaks = 10) ***
hist(churndata$Day.Mins,col = "red", main ="Totalno Day Mins")
hist(churndata$Day.Calls,col= "red" ,main= "Total No of Day Calls")# hist(churndata$Day.Charge,col= "red" ,main= "Total Day Charge")

# hist(sqrt(churndata$Intl.Calls)) ***
hist(churndata$Intl.Charge)
plot(as.factor(churndata$Int.l.Plan_ohe) ,col="red",main="International Plan Subscribed ")
plot(as.factor(churndata$VMail.Plan_ohe),col="red",main="Voice Mail Plan")
plot(churndata$Churn,col="red",main="No of churners and Non Churners")
legend("bottomright",c("1 = Churned","0 = Not churned"),cex = 0.4)


#plot1
#Hypotheses1:more number of call to customer care higher chances of churning
count<-table(churndata$Churn,churndata$CustServ.Calls)
prop<-prop.table(table(churndata$Churn,churndata$CustServ.Calls),margin = 2)
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
barplot(prop, col=heat.colors(length(rownames(prop))), width=2,main="call to customer care v/s churning")
legend("topright",inset=c(-0.25,0), fill=heat.colors(length(rownames(prop))),legend = rownames(count))

#plot2
# hypotheses 2: subscribing International calling plan increases churn
count1<-table(churndata$Churn,churndata$Int.l.Plan)
prop1<-prop.table(table(churndata$Churn,churndata$Int.l.Plan),margin = 2)
barplot(prop1,col=heat.colors(length(rownames(prop1))),width=2,main="Int'l call v/s churning rate")
legend("topright",inset=c(-0.25,0),fill=heat.colors(length(rownames(prop1))),legend=rownames(count1))

#plot3 
# hypotheses 3: Customers with the VoiceMail Plan tend to churn less frequently.
count2<-table(churndata$Churn,as.factor(churndata$VMail.Plan_ohe))
prop2<-prop.table(table(churndata$Churn,as.factor(churndata$VMail.Plan_ohe)),margin = 2)
barplot(prop2,col=heat.colors(length(rownames(prop2))),width = 2,main="Voice Mail Plan v/s churning rate")
legend("topright",inset=c(-0.25,0),fill=heat.colors(length(rownames(prop2))),legend=c("yes","no"))

churndata$CustServ.Calls<-as.numeric(churndata$CustServ.Calls) #again convert variable to its original form

summary(churndata) # lets check is there any missing value in dataset

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  PART 2 : DATA PARTITIONING 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Discard variables that are not relevant

churndata = churndata[,! names(churndata) %in% c("State", "Area.Code") ] # 

# creating training and testdata set using caret packge
library("caret")
index<-createDataPartition(y = churndata$Churn,times = 1,p = 0.7,list = FALSE)


set.seed(2) # set seed to get the same sample 
ind = sample(2, nrow(churndata), replace = TRUE, prob=c(0.7, 0.3))
trainset = churndata[ind == 1,]
testset = churndata[ind == 2,]

# checking proportion of churn in both the data set (appox. 15 % churner 85 % non churners)
prop.table(table(trainset$Churn))
prop.table(table(testset$Churn))

# checking dimension of both the dataset
dim(trainset)
dim(testset)


#************************************ PART 3 : MODEL BUILDING ************************************************#

# MODEL 1 "DECISION TREE"                                                                                     #
# MODEL 2 "Logistic Regression"                                                                               #
# MODEL 3 "SVM"                                                                                               #
# MODEL 4 "RANDOM FORREST"                                                                                    #
# MODEL 5 "NEURAL NETWORK"                                                                                    
# MODEL 5 "CLTV ANALYSIS" #


#*************************************************************************************************************#
# MODEL 1 : DECISON TREE #
#************************#

library("rpart") # load package

# using rpart function to build a classification model
# FIRST WE ARE DEALING WITH FULLY GROWN TREE THEN WE WILL PRUNE THE TREE AND PREDICT WITH OPTIMIZED TREE

churn.rp.full<-rpart(Churn~.,data = trainset) # It will generate fully grown tree

churn.rp.full$variable.importance

print(churn.rp.full) # retriving node details of decision tree
printcp(churn.rp.full) # examining complexity parameter to avoid overfitting of data
plotcp(churn.rp.full) # It is the other way to visualize above table and we can see that min.xvalidation error is occuuring when tree size is 12

library(rattle)
fancyRpartPlot(churn.rp.full)
# summary(churn.rp)

# Visualizing Tree
plot(churn.rp.full,margin=0.1,uniform=TRUE, branch=0.6)
text(churn.rp.full,all=TRUE,use.n = TRUE)

#measuring prediction performance of tree model on train dataset
library("caret")
pred.f.t<-predict(churn.rp.full,testset,type="class")
#confusionMatrix(table(pred.f.t,trainset$Churn))

# measuring prediction performance of tree model on test dataset

conf.matrix.f.tree<-confusionMatrix(table(pred.f.t, testset$Churn),positive = "1") 
conf.matrix.f.tree

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prune the tree to prevent overfitting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

churn.rp.full$cptable

# To avoid overfitting we want to prune our decision tree and remove those variable which are not helping much in prediction we will prune tree on the basis of cross complexity parameter

min(churn.rp.full$cptable[,"xerror"]) # find minimum crossvalidation error of tree
which.min(churn.rp.full$cptable[,"xerror"]) # locate record with minimum crossvalidation error
churn.cp = churn.rp.full$cptable[8,"CP"] #Get the cost complexity parameter of the record with the minimum cross-validation errors
churn.cp
prune.tree = prune(churn.rp.full, cp= churn.cp) # prune the tree by setting min cross validation error

# visualize prune tree
plot(prune.tree, margin= 0.1,uniform=TRUE,branch=0.6)
text(prune.tree, all=TRUE , use.n=TRUE)

#lets check the performance of pruned tree
pred.p.t<-predict(prune.tree,testset,type = "class")
conf.matrix.p.t<-confusionMatrix(table(pred.p.t,testset$Churn),positive = "1") # if we check our specificity has been improved

conf.matrix.p.t


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   K Nearest Neighbor
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

knn_model_optk <- train(as.factor(Churn)~.,data=trainset,
                        method='knn',
                        tuneGrid=expand.grid(k=1:40),
                        metric="Accuracy",
                        trControl = trainControl(method = 'repeatedcv',
                                                 number=10,
                                                 repeats = 5))

plot(knn_model_optk)

library(class)
# Implement the K-NN model for optimal K.
knn_model <- knn(trainset[,-16],testset[,-16],trainset$Churn,k=17,prob=TRUE)

names(trainset)
#see the confusion matrix for the KNN model
table(knn_model,testset[,16])
confusionMatrix(knn_model, testset[,16], positive ="1")

#Accuracy of the model is 0.7957,
attr(knn_model,"prob") <- ifelse(knn_model==1,attr(knn_model,"prob"),1 - attr(knn_model,"prob"))

#create the prediction object for knn model
knn_pred <- prediction(attr(knn_model,"prob"),testset[,"Churn"])
knn_pref <- performance(knn_pred,"tpr","fpr")
#plot the ROC curve for knn model
plot(knn_pref,col="red",lty=3,lwd=3,colorize=T)
abline(a=0,b=1,lty=2,lwd=2)
#see the area under the curve for knn model
knn_auc <- performance(knn_pred,"auc")
knn_auc

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Naive Bayes Model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(e1071)

  nb.fit <-naiveBayes(Churn~.,data = trainset) 
pred.nb <- predict(nb.fit,newdata=testset)

table(pred.nb,testset[,16])




sqrt((8-2)^2 + (2-1)^2)
sqrt((8-4)^2 + (2-8)^2)
