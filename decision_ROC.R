getwd()
getwd()
#stevens = read.csv("stevens.csv")
stevens = read.csv("E:\\data science\\1st sem\\business analytics nd strategy\\stevens\\stevens.csv")
head(stevens)
str(stevens) #str shows structure
#library to set split
install.packages("caTools")
library(caTools)
#To avoid overfitting and underfitting we set seed
#ensures that the same random split happens every time you run the code.
set.seed(3000)
#sample.split is function to split data in train and tes
#stevens is dataset, reverse is dependent variable, split rtio decided percentage of train test split
#it requires randomness therefore we set seed before. 0.7 means 70% data used as training
#output TRUE → observation goes to training set
#FALSE → observation goes to test set
spl= sample.split(stevens $Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl == TRUE)
Test = subset(stevens,spl == FALSE)
#Packages for decision trees and visualization
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

#Decision tree
#predicts reverse based on other predictors
#minbucket shows minimum observations in a leaf node
StevensTree = rpart(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data = Train, method = "class", minbucket = 25)
#Plots the decision tree
prp(StevensTree)

#predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")

#confusion matrix
table(Test $ Reverse,PredictCART)
#accuracy
acc <- (41+71)/(41+36+22+71)
acc

#ROC curve and AUC
install.packages("ROCR")
library(ROCR)
#Gets probabilities not just class
PredictROC=predict(StevensTree,newdata = Test) 
PredictROC

#Uses probability of class 2 for ROC
pred=prediction(PredictROC[,2],Test$Reverse)

#Plots ROC Curve
pref=performance(pred,"tpr","fpr")
plot(pref)

#area under curve
#0.5 means bad model 0.7 or above good
AUC = as.numeric(performance(pred, "auc")@y.values)
AUC

#now lets experiment by changing minbucket value
stTree= rpart(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data = Train, method = "class", minbucket = 5)
prp(stTree)
STree= rpart(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data = Train, method = "class", minbucket = 100)
prp(STree)
newTree =  rpart(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data = Train, method = "class", minbucket = 100)
prp(newTree)
