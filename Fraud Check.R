#Use decision trees to prepare a model on fraud data 
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good"

#Data Description :
  
#Undergrad : person is under graduated or not
#Marital.Status : marital status of a person
#Taxable.Income : Taxable income is the amount of how much tax an individual owes to the government 
#Work Experience : Work experience of an individual person
#Urban : Whether that person belongs to urban area or not


install.packages("readr")
library(readr)
setwd('C://Users//Lenovo//Desktop//ExcelR//Assignments//Decision Trees')
getwd()
Fraud <- read.csv("Fraud_check.csv")
View(Fraud)
install.packages("ISLR")
library(ISLR)
install.packages("tree")
library(tree)
hist(Fraud$Taxable.Income)
FaadMagic <- ifelse(Fraud$Taxable.Income<=30000,"Risky","Good")
head(FaadMagic)

Fraud <- data.frame(Fraud,FaadMagic)
View(Fraud)
Fraud <- Fraud[-3]
View(Fraud)
Fraudtree <- tree(factor(FaadMagic)~. , data = Fraud, split=c("deviance", "gini"))
is.na(Fraudtree)
sum(is.na(Fraudtree))
str(Fraudtree)
summary(Fraudtree)
library(caret)
Training <- createDataPartition(Fraud$FaadMagic, p=0.50, list=F)

Train <- Fraud[Training,]
table(Train$FaadMagic)

Test <- Fraud[-Training,]
table(Test$FaadMagic)

Model_train <- tree(factor(FaadMagic)~., data = Train)
dim(Model_train)
summary(Model_train)


pred <- predict(Model_train, newdata=Test[-7], type = "class")
summary(pred)


dim(pred)

mean(pred==Test$FaadMagic)
library(gmodels)
CrossTable(pred,Test$FaadMagic)
confusionMatrix(pred,factor(Test$FaadMagic))
# We find the model is overfit in pred object there is only "Good" value.
#To over come this we will apply begging methond
acc <- NULL
for (i in 1:100) 
{ print(i)
  Training <- createDataPartition(factor(Fraud$FaadMagic), p=0.50, list=F)
  Train <- Fraud[Training,]
  Test <- Fraud[-Training,]
  
  Model_train <- tree(factor(FaadMagic)~., data = Train)
  pred <- predict(Model_train, newdata=Test, type = "class")
  
  acc <- c(acc, mean(pred==Test$FaadMagic))
  
}
mean(acc)
CrossTable(pred,Test$FaadMagic)

#Conclusion:-
# Very strange thing found in this data set afer a long research in bagging method
#if you run this program at 60% data partion you model will over fit
# but if you run this program at 50% your model will not over fit.