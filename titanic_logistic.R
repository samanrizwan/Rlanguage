#import libraries
install.packages("caTools")
install.packages("caret")

library(caTools)
library(caret)  #library for evaluation tools like confusion matrix and accuracy 

#load data
data <- read.csv("D:\\Rlanguage\\titanic\\gender_submission.csv")
test <- read.csv("D:\\Rlanguage\\titanic\\test.csv")
train <- read.csv("D:\\Rlanguage\\titanic\\train.csv")
head(train)
str(train)

# Convert categorical to numeric
train$Sex <- ifelse(train$Sex == "male", 0, 1)
test$Sex <- ifelse(test$Sex == "male", 0, 1)

# Handle missing values
train$Age[is.na(train$Age)] <- mean(train$Age, na.rm = TRUE)
test$Age[is.na(test$Age)] <- mean(test$Age, na.rm = TRUE)

model <- glm(Survived ~ Pclass + Sex + Age + Fare,
             data = train,family = binomial)

summary(model)
prob_predictions <- predict(model,newdata = test,
                            type = "response")

predicted_class <- ifelse(prob_predictions > 0.5, 1, 0)

submission <- data.frame(
  PassengerId = test$PassengerId,
  Survived = predicted_class
)
write.csv(submission, "submission.csv", row.names = FALSE)

#Since test.csv has no true labels we can't check accuracy.

