library(tidyverse)
library(caret)
library(e1071)
library(randomForest)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(GGally)
library(rpart)
library(rpart.plot)
library(lightgbm)

dataset <- read.csv("C:/Users/adars/OneDrive/Desktop/Yukti-project/Diabetic/diabetes.csv")
head(dataset)

str(dataset)
dim(dataset)
summarize_all(dataset, funs(sum(is.na(.))))

ggplot(dataset, aes(x = Outcome)) + 
  geom_bar(fill = "steelblue") + 
  labs(title = "Distribution of Outcome Variable")

ggpairs(dataset, aes(color = factor(Outcome)))

cor_matrix <- cor(dataset[, -ncol(dataset)])
corrplot(cor_matrix, method = "color", tl.cex = 0.8)

set.seed(2)
dataset$Outcome <- as.factor(dataset$Outcome)
trainIndex <- createDataPartition(dataset$Outcome, p = 0.8, list = FALSE)
train_data <- dataset[trainIndex, ]
test_data <- dataset[-trainIndex, ]

preProcess_range_model <- preProcess(train_data[, -ncol(train_data)], method = c("center", "scale"))
train_scaled <- predict(preProcess_range_model, train_data[, -ncol(train_data)])
test_scaled <- predict(preProcess_range_model, test_data[, -ncol(test_data)])

regressor <- rpart(Outcome ~ ., data = train_data, method = "class")
rpart.plot(regressor)

new_input <- data.frame(Pregnancies = 6, Glucose = 148, BloodPressure = 72, SkinThickness = 35, 
                         Insulin = 0, BMI = 33.6, DiabetesPedigreeFunction = 0.627, Age = 50)
prediction <- predict(regressor, new_input, type = "class")

if (prediction == 1) {
  print("The person is diabetic")
} else {
  print("The person is not diabetic")
}

predicted_values <- predict(regressor, test_data, type = "class")
conf_matrix <- confusionMatrix(predicted_values, test_data$Outcome)
print(conf_matrix)
