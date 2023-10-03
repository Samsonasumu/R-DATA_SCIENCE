##### QUESTION 1
### RANDOM FOREST

# Load the required library 
# install.packages("readr")
library(readr)

# Specify the full absolute file path to your dataset
file_path <- "C:/Users/samso/Desktop/HMEQ_Scrubbed.csv"
data <- read.csv(file_path, header = TRUE)

# View the first few rows of the dataset 
head(data)

# Print the column names of the dataset
col_names <- colnames(data)
print(col_names)


# install.packages("randomForestSRC")
library(rpart)
library(randomForest)
library(randomForestSRC)

# Step 1: Decision Tree
# Create a Decision Tree model for regression

#  'TARGET_LOSS_AMT' is the numeric target variable  to predict
target_variable <- data$TARGET_LOSS_AMT
predictor_variables <- data[, -which(names(data) %in% c("TARGET_LOSS_AMT", "TARGET_BAD_FLAG"))]

# Create a Decision Tree model
decision_tree_model <- rpart(target_variable ~ ., data = data)

# Print the decision tree
print(decision_tree_model)


# Step 2: Random Forest
# Create a Random Forest model for regression

#  'TARGET_LOSS_AMT' is the numeric target variable  to predict
target_variable <- data$TARGET_LOSS_AMT
predictor_variables <- data[, -which(names(data) %in% c("TARGET_LOSS_AMT", "TARGET_BAD_FLAG"))]

# Create a Random Forest model
random_forest_model <- randomForest(target_variable ~ ., data = data, ntree = 100)

# Print the Random Forest model summary
print(random_forest_model)


##### QUESTION 2
# Load necessary libraries
library(MASS)

# Step 1: Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))  # 70% for training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Step 2: Create a Logistic Regression model (All Variables)
logistic_regression_model_all <- glm(TARGET_BAD_FLAG ~ ., data = train_data, family = binomial)

# Display model summary
summary(logistic_regression_model_all)


# Step 3: Create a Logistic Regression model (Backward Variable Selection)
# Fit a full Logistic Regression model
logistic_regression_full <- glm(TARGET_BAD_FLAG ~ ., data = train_data, family = binomial)

# Perform backward variable selection
logistic_regression_backward <- stepAIC(logistic_regression_full, direction = "backward")

# estimate variable importance
library(mlbench)
library(caret)
importance_logi <- varImp(logistic_regression_backward, scale=FALSE)
# summarize importance
print(importance_logi)

importance_randomforest <- varImp(random_forest_model, scale=FALSE)
# summarize importance
print(importance_randomforest)

# Display selected model summary
summary(logistic_regression_backward)

# Step 4: Create a Logistic Regression model (Forward Stepwise Selection using a Decision Tree)
# Start with an empty model
logistic_regression_forward <- glm(TARGET_BAD_FLAG ~ 1, data = train_data, family = binomial)

# Perform forward stepwise selection with Decision Tree
while (TRUE) {
  current_formula <- formula(logistic_regression_forward)
  available_predictors <- setdiff(names(train_data), all.vars(current_formula))
  best_aic <- Inf
  best_predictor <- NULL
  
  for (predictor in available_predictors) {
    candidate_formula <- as.formula(paste(current_formula, "+", predictor))
    candidate_model <- glm(candidate_formula, data = train_data, family = binomial)
    candidate_aic <- AIC(candidate_model)
    if (candidate_aic < best_aic) {
      best_aic <- candidate_aic
      best_predictor <- predictor
    }
  }
  
  if (best_aic < AIC(logistic_regression_forward)) {
    logistic_regression_forward <- glm(as.formula(paste(current_formula, "+", best_predictor)), 
                                       data = train_data, family = binomial)
  } else {
    break
  }
}

# Display selected model summary
summary(logistic_regression_forward)

#display ROC for the all models
 
logistic_regression_full
random_forest_model
decision_tree_model
library(pROC)
# Predict probabilities on the test data
prob_scores1 <- predict(logistic_regression_full, newdata = test_data, type = "response")
prob_scores2 <- predict(random_forest_model, newdata = test_data, type = "response")
prob_scores3log <- predict(logistic_regression_forward, newdata = test_data, type = "response")

 
# Calculate the ROC curves
roc1 <- roc(test_data$TARGET_BAD_FLAG ~ prob_scores1, plot = FALSE)
roc2 <- roc(test_data$TARGET_BAD_FLAG ~ prob_scores2, plot = FALSE)
roc3 <- roc(test_data$TARGET_BAD_FLAG ~ prob_scores3log, plot = FALSE)



# Plot ROC curves on the same  line
plot(roc1, col = "red", main = "ROC Curves for Logistic Regressions")
lines(roc2, col = "blue")
lines(roc3, col = "green")
legend("bottomright", legend = c("logistic_regression_full 1",
                                 "random_forest_model", "logistic_regression_forward
                                 "), col = c("red", "blue", "green"), lty = 1)





##### QUESTION 3

# Load necessary libraries
library(MASS)

file_path <- "C:/Users/samso/Desktop/HMEQ_Scrubbed.csv"
 
# Read the CSV file into a data frame
data <- read.csv(file_path, header = TRUE)

# Step 1: Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))  # 70% for training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

  

# Step 2: Create a  linear Regression model (All Variables)
linear_regression_model_all <- lm(TARGET_LOSS_AMT ~ ., data = train_data)

# Display model summary
summary(linear_regression_model_all)

# Step 3: Create a linear Regression model (Backward Variable Selection)
linear_regression_full <- lm(TARGET_LOSS_AMT ~ ., data = train_data)

# Perform backward variable selection
linear_regression_backward <- stepAIC(linear_regression_full, direction = "backward")

# Display selected model summary
summary(linear_regression_backward)

# Step 4: Create a  linear Regression model (Forward Stepwise Selection using a Decision Tree)
# Start with an empty model
linear_regression_forward <- lm(TARGET_LOSS_AMT ~ 1, data = train_data)

# Perform forward stepwise selection with Decision Tree
while (TRUE) {
  current_formula <- formula(linear_regression_forward)
  available_predictors <- setdiff(names(train_data), all.vars(current_formula))
  best_aic <- Inf
  best_predictor <- NULL
  
  for (predictor in available_predictors) {
    candidate_formula <- as.formula(paste(current_formula, "+", predictor,collapse = ""))
    candidate_model <- lm(candidate_formula, data = train_data)
    candidate_aic <- AIC(candidate_model)
    if (candidate_aic < best_aic) {
      best_aic <- candidate_aic
      best_predictor <- predictor
    }
  }
  
  if (best_aic < AIC(linear_regression_forward)) {
    linear_regression_forward <- lm(as.formula(paste(current_formula, "+", best_predictor)), 
                                       data = train_data)
  } else {
    break
  }
}

# Display selected model summary
summary(linear_regression_forward)

# IMPORTANT VARIABLES
#install.packages("mlbench")
#install.packages("caret")
library(mlbench)
library(caret)

# estimate variable importance
importance <- varImp(linear_regression_full, scale=FALSE)
# summarize importance
print(importance)

#compare with those of random forest variables
  
print(importanceRandom)


#RMSE OF TEST
#  step 1 Provide the model predictions
full_predictions <- predict(linear_regression_full, newdata = test_data) 
backward_predictions <- predict(linear_regression_backward, newdata = test_data) 
forward_predictions <- predict(linear_regression_forward, newdata = test_data) 

forward_predictions
# Step 2: Calculate RMSE for each model

# Calculate RMSE for model 1
full_rmse <- RMSE(test_data$TARGET_LOSS_AMT, linear_predictions)  
backward_rmse <- RMSE(test_data$TARGET_LOSS_AMT, backward_predictions)  
foward_rmse <- RMSE(test_data$TARGET_LOSS_AMT, forward_predictions)  

full_rmse 
backward_rmse
foward_rmse


#STEP 4

#     CODE FOR STEP TWO
# Load necessary libraries
library(MASS)

# Step 1: Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))  # 70% for training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Step 2: Create a Logistic Regression model (All Variables)
logistic_regression_model_all <- glm(TARGET_BAD_FLAG ~ ., data = train_data, family = binomial)

# Display model summary
summary(logistic_regression_model_all)

 
#PREDICT TARGET_BAD_FLAG
logistic_regression_model_all
train_data
head(train_data)

# seleecting where  TARGET_BAD_FLAG is 1
library(dplyr)
only1 = train_data %>%
  filter(TARGET_BAD_FLAG == 1)
  
tail(only1)
linear_ml <- lm(TARGET_LOSS_AMT ~ ., data = only1)
summary(linear_ml)

#important variables for both models are 
# estimate variable importance
importa_for_Linear <- varImp(linear_ml, scale=FALSE)
# summarize importance
print(importa_for_Linear)

importa_for_Logistic  <- varImp(logistic_regression_model_all, scale=FALSE)
print(importa_for_Logistic)

#probability of default
only1

# Calculate predicted probabilities
predicted_prob <- plogis(predict(logistic_regression_model_all, newdata = test_data))
         
predicted_prob
