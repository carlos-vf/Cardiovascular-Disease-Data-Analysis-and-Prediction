cardio_with_id <- read.csv("data/cardio_train.csv", sep= ";", header=TRUE)
cardio <- cardio_with_id[,-1]
cardio <- cardio[!(cardio$ap_hi < 0 | cardio$ap_hi > 400 | cardio$ap_lo < 0 | cardio$ap_lo > 400 | cardio$ap_hi < cardio$ap_lo | cardio$ap_hi-cardio$ap_lo > 250),]
cardio$gender <- factor(cardio$gender)
levels(cardio$gender) = c("F", "M")

cardio$cardio <- as.factor(cardio$cardio)

cardio$cholesterol <- factor(cardio$cholesterol)
levels(cardio$cholesterol) = c("normal", "above normal", "well above normal")

cardio$gluc <- factor(cardio$gluc)
levels(cardio$gluc) = c("normal", "above normal", "well above normal")

cardio$smoke <- factor(cardio$smoke)
levels(cardio$smoke) = c("No", "Yes")

cardio$alco <- factor(cardio$alco)
levels(cardio$alco) = c("No", "Yes")

cardio$active <- factor(cardio$active)
levels(cardio$active) = c("No", "Yes")
set.seed(23)
train_index <- sample(1:nrow(cardio), 0.8 * nrow(cardio))  
train_data <- cardio[train_index, ]
test_data <- cardio[-train_index, ]

# Model matrix creation

train_data_dummies <- model.matrix(cardio ~ ., data = train_data)[, -1]  # Exclude intercept column
test_data_dummies <- model.matrix(cardio ~ ., data = test_data)[, -1]    # Exclude intercept column

X_train <- scale(train_data_dummies)
Y_train <- as.numeric(train_data$cardio) - 1  # Assuming 'cardio' is a factor variable (0/1)
X_test <- scale(test_data_dummies)

# Fit the model using cv.glmnet
cvfit <- cv.glmnet(X_train, Y_train, family = "binomial", type.measure = "class")
plot(cvfit)
print(cvfit)

# Get optimal lambda values
lambda_min <- cvfit$lambda.min
lambda_1se <- cvfit$lambda.1se
cat("Lambda.min:", lambda_min, "\n")
cat("Lambda.1se:", lambda_1se, "\n")

# Coefficients
cat("Coefficients at lambda.min:\n")
print(coef(cvfit, s = "lambda.min"))

cat("Coefficients at lambda.1se:\n")
print(coef(cvfit, s = "lambda.1se"))

# Predictions on test data
predictions <- predict(cvfit, newx = X_test, s = "lambda.min", type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
print(predicted_classes)

# Training predictions (lambda.min)
train_predictions_min <- predict(cvfit, newx = X_train, s = "lambda.min", type = "response")
train_pred_classes_min <- ifelse(train_predictions_min > 0.5, 1, 0)
train_accuracy_min <- mean(train_pred_classes_min == Y_train)
train_roc_min <- roc(Y_train, as.vector(train_predictions_min))
train_conf_matrix_min <- table(Predicted = train_pred_classes_min, Actual = Y_train)

cat("Training Accuracy (lambda.min):", train_accuracy_min, "\n")
cat("Training AUC (lambda.min):", auc(train_roc_min), "\n")
cat("Training Confusion Matrix (lambda.min):\n")
print(train_conf_matrix_min)

# Test predictions (lambda.min)
test_predictions_min <- predict(cvfit, newx = X_test, s = "lambda.min", type = "response")
test_pred_classes_min <- ifelse(test_predictions_min > 0.5, 1, 0)
test_accuracy_min <- mean(test_pred_classes_min == as.numeric(test_data$cardio) - 1)
test_roc_min <- roc(as.numeric(test_data$cardio) - 1, as.vector(test_predictions_min))
test_conf_matrix_min <- table(Predicted = test_pred_classes_min, Actual = as.numeric(test_data$cardio) - 1)

cat("Test Accuracy (lambda.min):", test_accuracy_min, "\n")
cat("Test AUC (lambda.min):", auc(test_roc_min), "\n")
cat("Test Confusion Matrix (lambda.min):\n")
print(test_conf_matrix_min)

# Training predictions (lambda.1se)
train_predictions_1se <- predict(cvfit, newx = X_train, s = "lambda.1se", type = "response")
train_pred_classes_1se <- ifelse(train_predictions_1se > 0.5, 1, 0)
train_accuracy_1se <- mean(train_pred_classes_1se == Y_train)
train_roc_1se <- roc(Y_train, as.vector(train_predictions_1se))
train_conf_matrix_1se <- table(Predicted = train_pred_classes_1se, Actual = Y_train)

cat("Training Accuracy (lambda.1se):", train_accuracy_1se, "\n")
cat("Training AUC (lambda.1se):", auc(train_roc_1se), "\n")
cat("Training Confusion Matrix (lambda.1se):\n")
print(train_conf_matrix_1se)

# Test predictions (lambda.1se)
test_predictions_1se <- predict(cvfit, newx = X_test, s = "lambda.1se", type = "response")
test_pred_classes_1se <- ifelse(test_predictions_1se > 0.5, 1, 0)
test_accuracy_1se <- mean(test_pred_classes_1se == as.numeric(test_data$cardio) - 1)
test_roc_1se <- roc(as.numeric(test_data$cardio) - 1, as.vector(test_predictions_1se))
test_conf_matrix_1se <- table(Predicted = test_pred_classes_1se, Actual = as.numeric(test_data$cardio) - 1)

cat("Test Accuracy (lambda.1se):", test_accuracy_1se, "\n")
cat("Test AUC (lambda.1se):", auc(test_roc_1se), "\n")
cat("Test Confusion Matrix (lambda.1se):\n")
print(test_conf_matrix_1se)

# Calculate MSE for training and test sets
train_mse_min <- mean((train_predictions_min - Y_train)^2)
train_mse_1se <- mean((train_predictions_1se - Y_train)^2)
test_mse_min <- mean((test_predictions_min - (as.numeric(test_data$cardio) - 1))^2)
test_mse_1se <- mean((test_predictions_1se - (as.numeric(test_data$cardio) - 1))^2)

cat("Training MSE (lambda.min):", train_mse_min, "\n")
cat("Training MSE (lambda.1se):", train_mse_1se, "\n")
cat("Test MSE (lambda.min):", test_mse_min, "\n")
cat("Test MSE (lambda.1se):", test_mse_1se, "\n")

# Create a summary table
results <- data.frame(
  Metric = c(
    "Training Accuracy", "Training AUC", "Training MSE",
    "Test Accuracy", "Test AUC", "Test MSE"
  ),
  Lambda_Min = c(
    train_accuracy_min, auc(train_roc_min), train_mse_min,
    test_accuracy_min, auc(test_roc_min), test_mse_min
  ),
  Lambda_1SE = c(
    train_accuracy_1se, auc(train_roc_1se), train_mse_1se,
    test_accuracy_1se, auc(test_roc_1se), test_mse_1se
  )
)

cat("\nSummary of Results:\n")
print(results)

# Print confusion matrices for better understanding
cat("\nTraining Confusion Matrix (Lambda.Min):\n")
print(train_conf_matrix_min)
cat("\nTraining Confusion Matrix (Lambda.1SE):\n")
print(train_conf_matrix_1se)

cat("\nTest Confusion Matrix (Lambda.Min):\n")
print(test_conf_matrix_min)
cat("\nTest Confusion Matrix (Lambda.1SE):\n")
print(test_conf_matrix_1se)






