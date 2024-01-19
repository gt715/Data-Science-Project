library(factoextra)
library(cluster)


perform_kmeans <- function(data, k, selected_column) {

  data <- data[, -which(names(data) == "warranty")]
  data <- data[-c(1)]
  numeric_columns <- sapply(data, is.numeric)

  data_subset <- data
  
  if (selected_column == "All") {
    data_subset <- data[, numeric_columns, drop = FALSE]
    data_subset <- scale(data_subset)
  } else {
    data_subset <- data[, selected_column, drop = FALSE]
  }

  kmeans_result <- kmeans(data_subset, centers = k, nstart = 25)
  return(kmeans_result)

}

represent_kmeans <- function(kmeans_result, data) {
  data <- data[, -which(names(data) == "warranty")]
  data <- data[-c(1)]
  numeric_columns <- sapply(data, is.numeric)
  data <- scale(data[, numeric_columns])
  fviz_cluster(kmeans_result, data, palette = "jco", ggtheme = theme_minimal())
}
