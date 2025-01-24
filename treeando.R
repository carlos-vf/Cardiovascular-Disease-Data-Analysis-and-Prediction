# Load necessary libraries
library(rpart)
library(rpart.plot)  # For visualizing trees

# Create a list to store results
results <- list()

# Loop through each explanatory variable
for (variable in names(cardio)[names(cardio) != "cardio"]) {
  
  # Dynamically create a formula using the current variable
  formula <- as.formula(paste("cardio ~", variable))
  
  # Fit a decision tree model using the current variable
  tree_model <- rpart(formula, data = cardio, method = "class")
  
  # Predictions on the training data
  predictions <- predict(tree_model, newdata = cardio, type = "class")
  
  # Calculate accuracy
  accuracy <- mean(predictions == cardio$cardio)
  
  # Save results if accuracy is at least 58%
  if (accuracy >= 0.58) {
    results[[variable]] <- list(
      variable = variable,
      accuracy = accuracy,
      model = tree_model
    )
    
    # Print the results for this variable
    cat("\nVariable:", variable, "\n")
    cat("Accuracy:", accuracy, "\n")
  }
}

# Check if any variables meet the threshold
if (length(results) > 0) {
  cat("\nVariables with Accuracy >= 58%:\n")
  
  for (result in results) {
    cat("Variable:", result$variable, " - Accuracy:", result$accuracy, "\n")
    
    # Visualize the tree for each variable
    rpart.plot(result$model, main = paste("Decision Tree for", result$variable))
  }
} else {
  cat("\nNo variables achieved at least 58% accuracy.\n")
}