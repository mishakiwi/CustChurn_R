getwd()

#set the directory

setwd("D:\\R direc\\Project")
getwd()
dir()

#read the csv file

df <- read.csv("CustomerChurn.csv")

#get the basic details of data

str(df)
head(df)
names(df)
nrow(df) 
ncol(df)

# Check and remove the NULLS

colSums(is.na(df))
Custchurn <-na.omit(df)
colSums(is.na(Custchurn))
nrow(Custchurn)

# remove the data not required for analysis

Custchurn$customerID <-NULL

library(plyr)
names(Custchurn)

#check for numerical and categorical variables and transform the variables if required

sapply(Custchurn, class)
Custchurn <- transform(
  Custchurn,
  gender=as.factor(gender),
  SeniorCitizen=as.factor(SeniorCitizen),
  Partner=as.factor(Partner),
  Dependents=as.factor(Dependents),
  PhoneService=as.factor(PhoneService),
  MultipleLines=as.factor(MultipleLines),
  InternetService=as.factor(InternetService),
  OnlineSecurity=as.factor(OnlineSecurity),
  OnlineBackup=as.factor(OnlineBackup),
  DeviceProtection=as.factor(DeviceProtection),
  TechSupport=as.factor(TechSupport),
  StreamingTV=as.factor(StreamingTV),
  StreamingMovies=as.factor(StreamingMovies),
  Contract=as.factor(Contract),
  PaperlessBilling=as.factor(PaperlessBilling),
  PaymentMethod=as.factor(PaymentMethod),
  Churn=as.factor(Churn)
)

sapply(Custchurn, class)

#change the data values in 7 categorical variables

Custchurn$SeniorCitizen <- revalue(Custchurn$SeniorCitizen, c("1"="Yes"))
Custchurn$SeniorCitizen <- revalue(Custchurn$SeniorCitizen, c("0"="No"))

Custchurn$OnlineSecurity <- revalue(Custchurn$OnlineSecurity, c("No internet service"="No"))
Custchurn$OnlineBackup <- revalue(Custchurn$OnlineBackup, c("No internet service"="No"))
Custchurn$DeviceProtection <- revalue(Custchurn$DeviceProtection, c("No internet service"="No"))
Custchurn$TechSupport <- revalue(Custchurn$TechSupport, c("No internet service"="No"))
Custchurn$StreamingTV <- revalue(Custchurn$StreamingTV, c("No internet service"="No"))
Custchurn$StreamingMovies <- revalue(Custchurn$StreamingMovies, c("No internet service"="No"))
Custchurn$MultipleLines <- revalue(Custchurn$MultipleLines, c("No phone service"="No"))


summary(Custchurn)


library(ggplot2)
library(ggpubr)
library(plyr)

#correlation between numeric variables

cor(CustchurnInt$MonthlyCharges, as.matrix(CustchurnInt[sapply(CustchurnInt,is.numeric)]))
library(car)
scatterplot(MonthlyCharges~TotalCharges, data = Custchurn)

#Total charges and Monthly charges are highly correlated, drop one of the two

Custchurn$MonthlyCharges <-NULL
names(Custchurn)
ncol(Custchurn)

#Frequency graphs for categorical variables

ggplot(Custchurn, aes(x=gender)) + ggtitle("Gender")+
  geom_bar(aes(y = ..count..)) + theme_minimal()

ggplot(Custchurn, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen")+
  geom_bar(aes(y = ..count..)) + theme_minimal()

ggplot(Custchurn, aes(x=Partner)) + ggtitle("Partner")+
  geom_bar(aes(y = ..count..)) + theme_minimal()           

ggplot(Custchurn, aes(x=Dependents)) + ggtitle("Dependents")+
  geom_bar(aes(y = ..count..)) + theme_minimal()

ggplot(Custchurn, aes(x=PhoneService)) + ggtitle("Phone Service")+
  geom_bar(aes(y = ..count..)) + theme_minimal()

ggplot(Custchurn, aes(x=MultipleLines)) + ggtitle("Multiple Lines")+
  geom_bar(aes(y = ..count..)) + theme_minimal()

ggplot(Custchurn, aes(x=InternetService)) + ggtitle("Internet Service")+
  geom_bar(aes(y = ..count..)) + theme_minimal()

ggplot(Custchurn, aes(x=OnlineSecurity)) + ggtitle("Online Security")+
  geom_bar(aes(y = ..count..)) + theme_minimal()

ggplot(Custchurn, aes(x=OnlineBackup)) + ggtitle("Online Backup")+
  geom_bar(aes(y = ..count..)) + theme_minimal()

ggplot(Custchurn, aes(x=DeviceProtection)) + ggtitle("DeviceProtection")+
  geom_bar(aes(y = ..count..)) + theme_minimal()

ggplot(Custchurn, aes(x=TechSupport)) + ggtitle("Tech Support")+
  geom_bar(aes(y = ..count..)) + theme_minimal()

ggplot(Custchurn, aes(x=StreamingTV)) + ggtitle("Streaming TV")+
  geom_bar(aes(y = ..count..)) + theme_minimal()

ggplot(Custchurn, aes(x=StreamingMovies)) + ggtitle("Streaming Movies")+
  geom_bar(aes(y = ..count..)) + theme_minimal()

ggplot(Custchurn, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing")+
  geom_bar(aes(y = ..count..)) + theme_minimal()

pie(table(Custchurn$PaymentMethod), main = "Payment Methods")

ggplot(Custchurn, aes(x=Contract)) + ggtitle("Contract")+
  geom_bar(aes(y = ..count..)) + theme_minimal()

#graphs with respect to output variable

ggplot(Custchurn, aes(x=gender, fill= Churn)) +
  geom_bar(position="stack")

ggplot(Custchurn, aes(x=TotalCharges, fill= Churn)) +
  geom_histogram()

ggplot(Custchurn, aes(x=tenure, fill= Churn)) +
  geom_bar(position="stack")

ggplot(Custchurn, aes(x=Contract, fill= Churn)) +
  geom_bar(position="stack")

ggplot(Custchurn, aes(x=PaperlessBilling, fill= Churn)) +
  geom_bar(position="stack")

ggplot(Custchurn, aes(x=PaymentMethod, fill= Churn)) +
  geom_bar(position="stack")

#Logistic regression

library(caTools)
set.seed(2000)

#split the data and create training and test sets

split = sample.split(Custchurn, SplitRatio = 0.75)
training_set = subset(Custchurn, split == TRUE)
test_set = subset(Custchurn, split == FALSE)
dim(Custchurn); dim(training_set); dim(test_set)

#create the model with all the variables

logitmodel1 = glm(Churn ~ ., family = binomial,  data = training_set)
summary(logitmodel1)

#create the model using only significant vairables

logitmodel2 = glm(Churn ~ tenure+InternetService+PhoneService+MultipleLines+OnlineSecurity+OnlineBackup+PaperlessBilling+TechSupport+Contract+PaymentMethod+TotalCharges, family = binomial,  data = training_set)
summary(logitmodel2)

#test the accuracy of the model with test data set

prob_pred = predict(logitmodel2, type = 'response', newdata = test_set)
summary(prob_pred)
head(cbind(test_set,prob_pred ),10)

y_pred = ifelse(prob_pred > 0.5, 1, 0)
head(cbind(test_set$Churn, y_pred),10)

#confusing matrix of logistic regression

library(caret)
cm = table(test_set[,19], y_pred)
cm

#Decision tree

library(rpart)
library(rpart.plot)

#split to create two data sets

set.seed(777)
Index = sample(x = 1:nrow(Custchurn), size = 0.7*nrow(Custchurn))
Index

train= Custchurn[Index, ]
nrow(train)

test = Custchurn[-Index, ]
nrow(test)
nrow(test) + nrow(train)

#write the model

trainModel = rpart(Churn ~ . , data = train)
trainModel

#plot the regression tree

rpart.plot(trainModel, type = 4,fallen.leaves = T, cex = 0.5, nn=T, roundint = FALSE)


printcp(trainModel)
trainModel_prune = prune(trainModel, cp=0.01)
rpart.plot(trainModel_prune)

#predict and check accuracy for test data set

predictChurn_test = predict(trainModel_prune, newdata = test, type="vector")
predictChurn_test 

head(cbind(test$Churn,predictChurn_test))

library(forecast)
cm = table(test[,19], predictChurn_test)
cm


#random forest model

library(randomForest)

#split the data to create training and test sets

sample = sample.split(Custchurn, SplitRatio = .75)
train = subset(Custchurn, sample == TRUE)
test  = subset(Custchurn, sample == FALSE)
dim(train);dim(test)

#create the model

rf <- randomForest(Churn ~ ., data=train)
rf


#use the model to predict for test set

pred = predict(rf, newdata=test[-19])
test[-19]
pred
library(caret)

#get the confusion matrix

cm = table(test[,19], pred)
cm
caret::confusionMatrix(cm)

#plot random forest model
plot(rf)

#tune the model
tuneRF(train[,-19],train[,19],stepFactor =0.5,plot=TRUE,ntreeTry =200,trace =TRUE,improve =0.05 )

#now build a better model using the information from above two plots

rfnew <- randomForest(Churn ~ ., data=train, ntree=200, mtry=2, inportance=TRUE, proximity= TRUE)
rfnew

#use the model to predict for test set

pred = predict(rfnew, newdata=test[-19])
test[-19]
pred
library(caret)

#get the confusion matrix

cm = table(test[,19], pred)
cm
varImp(rfnew)
varImpPlot(rfnew,sort = T,n.var = 10,main ="Top 10 Variable Importance" )
varUsed(rfnew)
