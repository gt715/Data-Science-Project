# Load the necessary libraries
library(rpart)
library(rpart.plot)

# This function performs a decision tree analysis on the input data
perform_decision_tree <- function(data, selected_column_for_decision) {
  # Remove the 'warranty' column from the data
  data <- data[, -which(names(data) == "warranty")]
  
  # Remove the first row from the data
  data <- data[-1, , drop = FALSE]  
  
  # Identify the character columns in the data
  char_cols <- sapply(data, is.character)
  
  # Convert the character columns to factors
  data[, char_cols] <- lapply(data[, char_cols], as.factor)

  # Loop through the columns in the data
  for (col in names(data)) {
    # If a column is a factor and has more than 31 levels
    if (is.factor(data[[col]]) && length(levels(data[[col]])) >= 31) {
      # Count the frequency of each level in the column
      freq_levels <- table(data[[col]])
      
      # Identify the infrequent levels (those with a count less than 50)
      infrequent_levels <- names(freq_levels)[freq_levels < 50]  # Adjust the threshold as needed
      
      # Add an 'Other' level to the column
      data[[col]] <- factor(data[[col]], levels = c(levels(data[[col]]), "Other"))
      
      # Replace the infrequent levels with 'Other'
      data[[col]][data[[col]] %in% infrequent_levels] <- "Other"
    }
  }
  
  # Create a formula for the decision tree
  formula <- as.formula(paste(selected_column_for_decision, "~ ."))
  
  # Perform the decision tree analysis
  decision_tree_result <- rpart(formula, data = data, model = TRUE)
  
  # Return the result of the decision tree analysis
  return(decision_tree_result)
}

# This function visualizes the result of a decision tree analysis
visualize_decision_tree <- function(tree_decision_result) {
  # Plot the decision tree
  rpart.plot(tree_decision_result, extra = 300, cex = 0.35, tweak = 1.2, uniform = TRUE, pa = cex - 1)
}
