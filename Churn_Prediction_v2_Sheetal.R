## CHURN PREDICTION CLASSIFICATTION PROJECT

## Classification project, we will do linear regression on the data to precdict wheather the customer 
## will churn 1 or not churn 0

#importing Churn data from Churn.xls file
setwd("D:/Data Analytics/R AcGg/Projects/Projects/Project 2 - Churn Prediction")
library(readxl)
Churn <- read_excel("Churn.xls")
class(Churn)
View(Churn)
nrow(Churn) 
str(Churn) # 3333 obs , 21 variables
table(Churn$Churn)
summary(Churn)
   

##Changing the names of the column for ROSE package:
names(Churn) <- c("Account.Length", "VMail.Message", "Day.Mins", "Eve.Mins", "Night.Mins", "Intl.Mins",   
                 "CustServ.Calls", "Churn", "Int.l.Plan", "VMail.Plan", "Day.Calls", "Day.Charge",    
                 "Eve.Calls", "Eve.Charge", "Night.Calls", "Night.Charge", "Intl.Calls", "Intl.Charge" ,  
                 "State" , "Area.Code" ,"Phone"  )
names(Churn)

##obseravtions
  #variable 8 is churn(binomial variable with 0 and 1)
  #Variable 21 can be omitted as it is just a phone number and cannot be a predictor
  #variable 19 (state) is string which needs to be converted to factor

#### MISSING VALUES TREATMENT #####
# visualize the missing values using the missing map from the Amelia package
library(Amelia)
missmap(Churn,col=c("yellow","red"))
any(is.na(Churn))
## no missing values were detected.
sapply(Churn, function(x) sum(is.na(x)))
## NO missing values were found

#for adjusting plots
par(mfcol=c(1,1)) #1 graph per plot
par(cex=1, mai=c(0.3,0.3,0.3,0.3)) #adjusting margins and borders

###variable exploration
library(ggplot2)
#histogram for all variables
hist(Churn$`Account.Length`, col = "blue")
hist(Churn$`VMail.Message`, col = "orange")
hist(Churn$`Day.Mins`, col = "blue")
hist(Churn$`Eve.Mins`, col = "orange")
hist(Churn$`Night.Mins`, col = "blue")
hist(Churn$`Intl.Mins`, col = "orange")
hist(Churn$`CustServ.Calls`, col = "blue")
ggplot(data = Churn) + geom_bar(mapping = aes(x = Churn), width = 0.1)
hist(Churn$`Int.l.Plan`, col = "blue")
hist(Churn$`VMail.Plan`, col = "orange")
hist(Churn$`Day.Calls`, col = "blue")
hist(Churn$`Day.Charge`, col = "orange")
hist(Churn$`Eve.Calls`, col = "blue")
hist(Churn$`Eve.Charge`, col = "orange")
hist(Churn$`Intl.Calls`, col = "blue")
hist(Churn$`Intl.Charge`, col = "orange")
ggplot(data = Churn) + geom_bar(mapping = aes(x = State))
hist(Churn$`Area.Code`, col = "orange")

##correlations between variables
cor(Churn[, c(-19,-21)])
library(corrplot)
corrplot(cor(Churn[,c(-19,-21)]), type = "upper")
library(DataExplorer)
plot_correlation(Churn, type = 'continuous')

##Checking outliers
boxplot(Churn$`Account.Length`)
boxplot(Churn$`VMail.Message`)
boxplot(Churn$`Day.Mins`)
boxplot(Churn$`Eve.Mins`)
boxplot(Churn$`Night.Mins`)
boxplot(Churn$`Intl.Mins`)
boxplot(Churn$`CustServ.Calls`)
boxplot(Churn$Churn)
boxplot(Churn$`Int.l.Plan`)
boxplot(Churn$`VMail.Plan`)

###Model building without outliers treatmrent.

## MOdel1: simple split in 80 ~ 2- for training and testing
library(caTools)
set.seed(1)
split <- sample.split(Churn, SplitRatio = 0.80)
Churn_train <- subset(Churn, split == TRUE)
Churn_train<-Churn_train[,c(-19, -21)]     ###Remove phone number variable and state as it will make the model complex
## Churn_train$State<-as.factor(Churn_train$State)
Churn_test <- subset(Churn, split == FALSE)
table(Churn$Churn)  #0 = 2850, 1 = 483
table(Churn_train$Churn) #0 = 1885, 1 = 338
table(Churn_test$Churn)  #0 = 965, 1 = 145

##MODEL 1 : 80:20 split without outlier treatment
glm_model1.null<-glm(Churn~1,family=binomial(link="logit"),data= Churn_train)
summary(glm_model1.null)

glm_model1.full<-glm(Churn ~., family=binomial(link="logit"), data=Churn_train)
summary(glm_model1.full)

#to find the best model with lowest AIC
step(glm_model1.null, scope = list(upper=glm_model1.full), direction="both", test="Chisq", data=Churn_train)

#final model 1 training:
glm_model1 <- glm(formula = Churn ~ Int.l.Plan + CustServ.Calls + Day.Charge + 
                    Eve.Mins + VMail.Plan + Intl.Calls + Night.Mins + Intl.Charge + 
                    VMail.Message + Eve.Charge, family = binomial(link = "logit"), 
                  data = Churn_train)

summary(glm_model1)



## Model2: Split the data into 80~ 20 having equal proportion of '0' & '1'
data_1<-Churn[Churn$Churn==1,]
set.seed(1)
nrow(data_1)
ind_1<-sample(1:nrow(data_1),0.8*nrow(data_1))     

data_0<-Churn[Churn$Churn==0,]
set.seed(1)
nrow(data_0)
ind_0<-sample(1:nrow(data_0),0.8*nrow(data_0))

Churn_train2<-Churn[c(ind_1,ind_0) , -c(19, 21) ]
str(Churn_train2)
table(Churn_train2$Churn)

Churn_test2<-Churn[-c(ind_1,ind_0),]
table(Churn_test2$Churn)

glm_model2.null<-glm(Churn~1,family=binomial(link="logit"),data= Churn_train2)
summary(glm_model1.null)

glm_model2.full<-glm(Churn ~., family=binomial(link="logit"), data=Churn_train2)
summary(glm_model1.full)

#to find the best model with lowest AIC
step(glm_model2.null, scope = list(upper=glm_model1.full), direction="both", test="Chisq", data=Churn_train2)

#final model 2 training:
glm_model2 <- glm(formula = Churn ~ Int.l.Plan + Day.Mins + CustServ.Calls + 
                    Eve.Mins + VMail.Plan + Intl.Charge + Intl.Calls + Night.Charge + 
                    Intl.Mins + VMail.Message, family = binomial(link = "logit"), 
                  data = Churn_train)
summary(glm_model2)


##Model3: undresampling with 483 'Zeros' and 483 'Ones'
library(ROSE)
data_483 <- ovun.sample(Churn ~ ., data = Churn, method = "under", N = 966, seed = 1)$data
data_483 <- data_483[, -c(19, 21)]
table(data_483$Churn)

glm_model483.null<-glm(Churn ~ 1,family=binomial(link="logit"),data=data_483)
glm_model483.full<-glm(Churn ~ .,family=binomial(link="logit"),data=data_483)

#to find the best model with lowest AIC
step(glm_model483.null, scope = list(upper=glm_model483.full), direction="both", test="Chisq", data=data_483)

#final model 3 training:
glm_model3<- glm(formula = Churn ~ `Day.Calls` + `VMail.Message` + `Intl.Calls` + `Night.Mins` + `VMail.Plan` +
                   `Eve.Mins` + `Int.l.Plan` + `CustServ.Calls`, family = binomial(link = "logit"), 
                 data = data_483)
summary(glm_model3)


# Model 4-------------------------------------------------------------------------
# undersampling with 1449 zeros and 483 ones
data_1449 <- ovun.sample(Churn ~ ., data = Churn, method = "under", N = 1932, seed = 1)$data
data_1449 <- data_1449[, -c(19, 21)]
table(data_1449$Churn)

glm_model1449.null<-glm(Churn ~ 1,family=binomial(link="logit"),data=data_1449)
glm_model1449.full<-glm(Churn ~ .,family=binomial(link="logit"),data=data_1449)

#to find the best model with lowest AIC
step(glm_model1449.null, scope = list(upper=glm_model1449.full), direction="both", test="Chisq", data=data_1449)

#final model 4 training:
glm_model4<- glm(formula = Churn ~ Int.l.Plan + CustServ.Calls + Day.Mins + 
                   VMail.Plan + Eve.Mins + Intl.Charge + Night.Charge + Intl.Calls + 
                   VMail.Message, family = binomial(link = "logit"), 
                 data = data_1449)
summary(glm_model4)


# Model 5--------------------------------------------------------------------------
# undersampling with 2415 zeros and 483 ones
data_2415 <- ovun.sample(Churn ~ ., data = Churn, method = "under", N = 2898, seed = 1)$data
data_2415 <- data_2415[, -c(19, 21)]
table(data_2415$Churn)

glm_model2415.null<-glm(Churn ~ 1,family=binomial(link="logit"),data=data_2415)
glm_model2415.full<-glm(Churn ~ .,family=binomial(link="logit"),data=data_2415)

#to find the best model with lowest AIC
step(glm_model2415.null, scope = list(upper=glm_model2415.full), direction="both", test="Chisq", data=data_2415)

#final model 5 training:
glm_model5<- glm(formula = Churn ~ Int.l.Plan + CustServ.Calls + Day.Mins + 
                   VMail.Plan + Eve.Mins + Intl.Charge + Intl.Calls + Night.Charge + 
                   VMail.Message, family = binomial(link = "logit"), 
                 data = data_2415)
summary(glm_model5)


##TESTING THE MODEL AND FINDING ACCURACY, ROC and AUC

#Check dispresion of model
summary(glm_model1)$deviance / summary(glm_model1)$df.residual
#=0.6669978, model is not overfitted.
summary(glm_model2)$deviance / summary(glm_model2)$df.residual
summary(glm_model3)$deviance / summary(glm_model3)$df.residual
summary(glm_model4)$deviance / summary(glm_model4)$df.residual
summary(glm_model5)$deviance / summary(glm_model5)$df.residual

# Finding the best model
library(ROCR)

pred1 <- predict(glm_model1,Churn_test,type="response")
pr1<-prediction(pred1, Churn_test$Churn)
#ROC curve
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
plot(prf1)
#Area under the curve
auc1 <- performance(pr1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1               # model1

pred2 <- predict(glm_model2,Churn_test2,type="response")
pr2<-prediction(pred2, Churn_test2$Churn)
#ROC curve
prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
plot(prf2)
#Area under the curve
auc2 <- performance(pr2, measure = "auc")
auc2 <- auc2@y.values[[1]]
auc2               # model2

pred3 <- predict(glm_model3,Churn_test,type="response")
pr3<-prediction(pred3, Churn_test$Churn)
prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
plot(prf3)
auc3 <- performance(pr3, measure = "auc")
auc3 <- auc3@y.values[[1]]
auc3                # model3

pred4 <- predict(glm_model4,Churn_test,type="response")
pr4<-prediction(pred4, Churn_test$Churn)
prf4 <- performance(pr4, measure = "tpr", x.measure = "fpr")
plot(prf4)
auc4 <- performance(pr4, measure = "auc")
auc4 <- auc4@y.values[[1]]
auc4               # model4

pred5 <- predict(glm_model5,Churn_test,type="response")
pr5<-prediction(pred5, Churn_test$Churn)
prf5 <- performance(pr5, measure = "tpr", x.measure = "fpr")
plot(prf5)
auc5 <- performance(pr5, measure = "auc")
auc5 <- auc5@y.values[[1]]
auc5               # model5

sort(as.data.frame(table(auc1, auc2, auc3, auc4, auc5)))

#Accuracy
library(rpart)
pred1 <- predict(glm_model1,Churn_test,type="response")
pred1 <- ifelse(pred1 > 0.5,1,0)
plot(Churn_test$Churn~pred1)
cfmatrix1=table(Churn_test$Churn,pred1)
cfmatrix1
Accuracy_model1<-((cfmatrix1[1]+cfmatrix1[2,2])/
                    (cfmatrix1[1,1]+cfmatrix1[1,2]+cfmatrix1[2,1]+cfmatrix1[2,2]))
Accuracy_model1               # model1

pred2 <- predict(glm_model2,Churn_test2,type="response")
pred2 <- ifelse(pred2 > 0.5,1,0)
plot(Churn_test2$Churn~pred2)
cfmatrix2=table(Churn_test2$Churn,pred2)
cfmatrix2
Accuracy_model2<-((cfmatrix2[1]+cfmatrix2[2,2])/
                    (cfmatrix2[1,1]+cfmatrix2[1,2]+cfmatrix2[2,1]+cfmatrix2[2,2]))
Accuracy_model2               # model2

pred3 <- predict(glm_model3,Churn_test,type="response")
pred3 <- ifelse(pred3 > 0.5,1,0)
plot(Churn_test$Churn~pred3)
cfmatrix3=table(Churn_test$Churn,pred3)
cfmatrix3
Accuracy_model3<-((cfmatrix3[1]+cfmatrix3[2,2])/
                    (cfmatrix3[1,1]+cfmatrix3[1,2]+cfmatrix3[2,1]+cfmatrix3[2,2]))
Accuracy_model3               # model3

pred4 <- predict(glm_model4,Churn_test,type="response")
pred4 <- ifelse(pred4 > 0.5,1,0)
plot(Churn_test$Churn~pred4)
cfmatrix4=table(Churn_test$Churn,pred4)
cfmatrix4
Accuracy_model4<-((cfmatrix4[1]+cfmatrix4[2,2])/
                    (cfmatrix4[1,1]+cfmatrix4[1,2]+cfmatrix4[2,1]+cfmatrix4[2,2]))
Accuracy_model4               # model4

pred5 <- predict(glm_model5,Churn_test,type="response")
pred5 <- ifelse(pred5 > 0.5,1,0)
plot(Churn_test$Churn~pred5)
cfmatrix5=table(Churn_test$Churn,pred5)
cfmatrix5
Accuracy_model5<-((cfmatrix5[1]+cfmatrix5[2,2])/
                    (cfmatrix5[1,1]+cfmatrix5[1,2]+cfmatrix5[2,1]+cfmatrix5[2,2]))
Accuracy_model5               # model5

sort(as.data.frame(table(Accuracy_model1, Accuracy_model2, Accuracy_model3, Accuracy_model4, 
                         Accuracy_model5)))  
# Model 5 has greater accuracy 



##from the above results we can conclude that we will use model as the best fit.

# Adjusting the probability limit in prediction
#we create a function to test different probability limit
Accuracyfunction<- function(x){
  glm_model1 <- glm(formula = Churn ~ Int.l.Plan + CustServ.Calls + Day.Charge + 
                      Eve.Mins + VMail.Plan + Intl.Calls + Night.Mins + Intl.Charge + 
                      VMail.Message + Eve.Charge, family = binomial(link = "logit"), 
                    data = Churn_train)
  pred1 <- predict(glm_model1,Churn_test,type="response")
  pred1 <- ifelse(pred1 > x ,1,0)
  plot(Churn_test$Churn~pred1)
  cfmatrix1=table(Churn_test$Churn,pred1)
  cfmatrix1
  Accuracy_model1<-((cfmatrix1[1]+cfmatrix1[2,2])/
                      (cfmatrix1[1,1]+cfmatrix1[1,2]+cfmatrix1[2,1]+cfmatrix1[2,2]))
  paste(Accuracy = Accuracy_model1)}

x<- c(Accuracyfunction(0.3),
      Accuracyfunction(0.4),
      Accuracyfunction(0.5),
      Accuracyfunction(0.6),
      Accuracyfunction(0.7))
as.data.frame(x)

# threshhold probability is between 0.4 to 0.6
x<- c(Accuracyfunction(0.406),
      Accuracyfunction(0.408),
      Accuracyfunction(0.500),
      Accuracyfunction(0.502),
      Accuracyfunction(0.504),
      Accuracyfunction(0.506),
      Accuracyfunction(0.508))
as.data.frame(x)

# Hence Threshhold Probability = 0.506

#============================================================================================
#     CONCLUSION       #
#============================================================================================


# Thus the final Model is as below with threshhold probability = 0.506

glm_model_final <- glm(formula = Churn ~ Int.l.Plan + CustServ.Calls + Day.Charge + 
                         Eve.Mins + VMail.Plan + Intl.Calls + Night.Mins + Intl.Charge + 
                         VMail.Message + Eve.Charge, family = binomial(link = "logit"), 
                       data = Churn_train)

summary(glm_model_final)
pred_final <- predict(glm_model_final,Churn_test,type="response")
pred_final <- ifelse(pred_final > 0.506 ,1,0)
table(pred_final)

