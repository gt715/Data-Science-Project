# Load the necessary libraries for k-means clustering and visualization
library(factoextra)
library(cluster)

# This function performs a k-means clustering analysis on the input data
perform_kmeans <- function(data, k, selected_column) {
  # Remove the 'warranty' column from the data
  data <- data[, -which(names(data) == "warranty")]
  
  # Remove the first row from the data
  data <- data[-c(1)]
  
  # Identify the numeric columns in the data
  numeric_columns <- sapply(data, is.numeric)

  # Create a subset of the data
  data_subset <- data
  
  # If 'All' is selected, scale all numeric columns
  if (selected_column == "All") {
    data_subset <- data[, numeric_columns, drop = FALSE]
    data_subset <- scale(data_subset)
  } else {
    # Otherwise, select only the specified column
    data_subset <- data[, selected_column, drop = FALSE]
  }

  # Perform the k-means clustering analysis
  kmeans_result <- kmeans(data_subset, centers = k, nstart = 25)
  
  # Return the result of the k-means clustering analysis
  return(kmeans_result)
}

# This function visualizes the result of a k-means clustering analysis
represent_kmeans <- function(kmeans_result, data) {
  # Remove the 'warranty' column from the data
  data <- data[, -which(names(data) == "warranty")]
  
  # Remove the first row from the data
  data <- data[-c(1)]
  
  # Identify the numeric columns in the data
  numeric_columns <- sapply(data, is.numeric)
  
  # Scale the numeric columns
  data <- scale(data[, numeric_columns])
  
  # Visualize the clusters using the 'fviz_cluster' function
  fviz_cluster(kmeans_result, data, palette = "jco", ggtheme = theme_minimal())
}
