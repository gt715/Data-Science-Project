library(rpart)
library(rpart.plot)

perform_decision_tree <- function(data, selected_column_for_decision) {

  data <- data[, -which(names(data) == "warranty")]
  data <- data[-1, , drop = FALSE]  
  
  char_cols <- sapply(data, is.character)
  data[, char_cols] <- lapply(data[, char_cols], as.factor)


  for (col in names(data)) {
    if (is.factor(data[[col]]) && length(levels(data[[col]])) >= 31) {
      freq_levels <- table(data[[col]])
      infrequent_levels <- names(freq_levels)[freq_levels < 50]  # Adjust the threshold as needed
      data[[col]] <- factor(data[[col]], levels = c(levels(data[[col]]), "Other"))
      data[[col]][data[[col]] %in% infrequent_levels] <- "Other"
    }
  }
  
  formula <- as.formula(paste(selected_column_for_decision, "~ ."))
  
  decision_tree_result <- rpart(formula, data = data, model = TRUE)
  
  return(decision_tree_result)
}

visualize_decision_tree <- function(tree_decision_result) {
  rpart.plot(tree_decision_result, extra = 300, cex = 0.35, tweak = 1.2, uniform = TRUE, pa = cex - 1)
}
