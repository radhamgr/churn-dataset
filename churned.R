#import Churn Dat
Churn_data <- read.csv(file.choose())

str(Churn_data)

# Step 1:Split data in train and test data
#install.packages("caTools")
library(caTools)

set.seed(350)
split <- sample.split(Churn_data, SplitRatio = 0.7)
split

train <- subset(Churn_data, split== "TRUE")
test <- subset(Churn_data, split== "FALSE")
str(train)
str(test)
library(rpart)
library(rpart.plot)
# Step 2:Train model with logistics regression using glm function
decision_tree_model<-rpart(Churn ~ ., data = train, method = "class")
decision_tree_model
summary(decision_tree_model)
printcp(decision_tree_model)
plotcp(decision_tree_model)

rpart.plot(decision_tree_model)


# Step 3:Predict test data based on trained model 
test$Churn_Predicted<-predict(decision_tree_model,newdata=test,type="class")


# Step 4: Evauate Model Accuracy using Confusion matrix
table(test$Churn,test$Churn_Predicted)
library(caret)
confusionMatrix(table(test$Churn,test$Churn_Predicted))


#Tree Pruning

#Find the value of CP for which cross validation error is minimum
min(decision_tree_model$cptable[,"xerror"])
which.min(decision_tree_model$cptable[,"xerror"])
cpmin <- decision_tree_model$cptable[5, "CP"]

#Prune the tree by setting the CP parameter as =  cpmin
decision_tree_pruned = prune(decision_tree_model, cp = cpmin)
rpart.plot(decision_tree_pruned)


# Predict test data based on trained model 
test$Churn_Predicted<-predict(decision_tree_pruned,newdata=test,type="class")


# Evauate Model Accuracy using Confusion matrix
table(test$Churn,test$Churn_Predicted)
library(caret)
confusionMatrix(table(test$Churn,test$Churn_Predicted))

