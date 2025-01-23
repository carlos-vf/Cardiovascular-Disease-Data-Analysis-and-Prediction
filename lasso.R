<<<<<<< HEAD

library(caret)
library(glmnet)
library(stats)
train_data_dummies <- model.matrix(cardio ~ ., data = train_data)[, -1]  # Exclude intercept column

# Separate predictors (X) and response (Y)
X <- train_data_dummies
Y <- as.numeric(train_data$cardio) - 1  # Convert the response to 0/1 for logistic regression


# Standardize the predictors (important for Lasso regression)
X_scaled <- scale(X)

lasso_model_ <- glmnet(X_scaled, Y, family = "binomial", alpha = 1)

# Fit the Lasso logistic regression model using glmnet
lasso_model <- glmnet(X_scaled, Y, family = "binomial", alpha = 1)

summary(lasso_model)
# View the model coefficients
print(coef(lasso_model))

# Use cross-validation to select the best lambda (regularization strength)
cv_model <- cv.glmnet(X_scaled, Y, family = "binomial", alpha = 1)

# Plot the cross-validation results
plot(cv_model)

# The best lambda from cross-validation
best_lambda <- cv_model$lambda.min
cat("Best lambda:", best_lambda, "\n")

# Fit the model with the best lambda
lasso_best_model <- glmnet(X_scaled, Y, family = "binomial", alpha = 1, lambda = best_lambda)

deviance_diff = lasso_best_model$nulldev - deviance(lasso_best_model)
k = lasso_best_model$df
n = lasso_best_model$nobs
AIC_lasso = -deviance_diff + 2*k+2*k*(k +1)/(n -k - 1)
# Make predictions
pred_probs <- predict(lasso_best_model, X_scaled, type = "response")
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)

# Confusion matrix
conf_matrix <- confusionMatrix(as.factor(pred_classes), as.factor(Y))
print(conf_matrix)

predictions <- predict(cv_model, newdata = test_data, type = "response")

# Step 3: Evaluate the predictions
# For a regression model, you can compute metrics like RMSE, MSE, or R-squared.
actual_values <- test_data$cardio  # Replace target_variable with your actual response variable

# Example: RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((predictions - actual_values)^2))
print(paste("RMSE on Test Set: ", rmse))

# Example: R-squared
r_squared <- 1 - sum((predictions - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
print(paste("R-squared on Test Set: ", r_squared))

=======

library(caret)
library(glmnet)
library(stats)
train_data_dummies <- model.matrix(cardio ~ ., data = train_data)[, -1]  # Exclude intercept column

# Separate predictors (X) and response (Y)
X <- train_data_dummies
Y <- as.numeric(train_data$cardio) - 1  # Convert the response to 0/1 for logistic regression


# Standardize the predictors (important for Lasso regression)
X_scaled <- scale(X)

lasso_model_ <- glmnet(X_scaled, Y, family = "binomial", alpha = 1)

# Fit the Lasso logistic regression model using glmnet
lasso_model <- glmnet(X_scaled, Y, family = "binomial", alpha = 1)

summary(lasso_model)
# View the model coefficients
print(coef(lasso_model))

# Use cross-validation to select the best lambda (regularization strength)
cv_model <- cv.glmnet(X_scaled, Y, family = "binomial", alpha = 1)

# Plot the cross-validation results
plot(cv_model)

# The best lambda from cross-validation
best_lambda <- cv_model$lambda.min
cat("Best lambda:", best_lambda, "\n")

# Fit the model with the best lambda
lasso_best_model <- glmnet(X_scaled, Y, family = "binomial", alpha = 1, lambda = best_lambda)

deviance_diff = lasso_best_model$nulldev - deviance(lasso_best_model)
k = lasso_best_model$df
n = lasso_best_model$nobs
AIC_lasso = -deviance_diff + 2*k+2*k*(k +1)/(n -k - 1)
# Make predictions
pred_probs <- predict(lasso_best_model, X_scaled, type = "response")
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)

# Confusion matrix
conf_matrix <- confusionMatrix(as.factor(pred_classes), as.factor(Y))
print(conf_matrix)

predictions <- predict(cv_model, newdata = test_data, type = "response")

# Step 3: Evaluate the predictions
# For a regression model, you can compute metrics like RMSE, MSE, or R-squared.
actual_values <- test_data$cardio  # Replace target_variable with your actual response variable

# Example: RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((predictions - actual_values)^2))
print(paste("RMSE on Test Set: ", rmse))

# Example: R-squared
r_squared <- 1 - sum((predictions - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
print(paste("R-squared on Test Set: ", r_squared))

>>>>>>> 31318ac0a177a6d717c52c0b0d613f4850324f4b
