
limit_rows <- function(data, limit) {
  if (tolower(limit) != "all") {
    data <- data[1:min(as.numeric(limit), nrow(data)), ]
  }
  return(data)
}


determine_numeric_cols <- function(session, cleaned_data) {
  observe({
    cleaned_data <- cleaned_data[-c(1)]
    cleaned_data <- cleaned_data[, -which(names(cleaned_data) == "warranty")]
    numeric_columns <- sapply(cleaned_data, is.numeric)
    updateSelectizeInput(session, "selected_column", choices = c("All", names(cleaned_data)[numeric_columns]), selected = names(cleaned_data)[numeric_columns][1])
  })
}


determine_decision_cols <- function(session, cleaned_data) {
  observe({
    cleaned_data <- cleaned_data[-c(1)]
    numeric_factor_cols <- names(cleaned_data[, sapply(cleaned_data, is.numeric) | sapply(cleaned_data, is.factor)])
    numeric_factor_cols <- setdiff(numeric_factor_cols, "warranty")
    updateSelectizeInput(session, "selected_column_for_decision", choices = numeric_factor_cols)
  })
}

summary_data <- function(data) {
  summary_text <- capture.output(summary(data))
  return(paste(summary_text, collapse = "\n"))
}

choices_for_visual <- function(visual_type) {
  if (is.null(visual_type)) {
      return(NULL)
  }

  if (visual_type == "Histogram") {
    return (c("Overall", "Low Range", "Mid Range", "High Range"))
  } else if(visual_type == "Barplot") {
    return (c("Overall Price"))
  } else if (visual_type == "Boxplot") {
    return (c("Ram Price Range", "Brand Rating Range", "Brand Price Range"))
  } else if (visual_type == "Dot") {
    return (c("Overall Price Rate"))
  } else {
    return (NULL)
  }
}


determine_visual_cols <- function(session, visual_type) {
  observe({
    updateSelectInput(session, "selected_column_for_visual", choices = choices_for_visual(visual_type))
  })
}
