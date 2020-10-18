#Perform the following activity in R: 
#Build a decision tree for the 'iris' data with function 'ctree()' in package "party".

install.packages("readr")
install.packages("party")
install.packages("caret")
library(party)
library(caret)
data("iris")
View(iris)

# Data partion for model building and testing
inTraininglocal <- createDataPartition(iris$Species,p=.75,list=F)
training <- iris[inTraininglocal,]
View(training)
testing <- iris[-inTraininglocal,]
table(testing$Species)

# Create the tree (Building the Model)
output.tree <- ctree(
  training$Species~.,data = training)
output.tree

# Generating the model summary
summary(output.tree)
plot(output.tree)
pred <- predict(output.tree,testing[,-5])
table(pred)
a <- table(testing$Species,pred)
a
sum(diag(a)/sum(a))


###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(iris$Species,p=.85,list=F)
  training1<-iris[inTraininglocal,]
  testing<-iris[-inTraininglocal,]
  
  fittree<-ctree(training1$Species~.,data=training1)
  pred<-predict(fittree,testing[,-5])
  a<-table(testing$Species,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

acc
summary(acc)