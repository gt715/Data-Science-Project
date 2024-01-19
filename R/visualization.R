# Load the necessary libraries for data visualization
library(gridExtra)
library(ggplot2)

# Source the data cleaning script
source("R/data_cleaning.R")

# Clean the data using the function from the sourced script
DS <- clean_data(read.csv("C:\\Games\\DB project\\datascience-project-main (1)\\datascience-project-main\\data\\Laptop-Dataset.csv"))

# This function performs a specific visualization based on the input parameters
perform_visualization <- function(visual_type, option) {
  # Depending on the visual_type and option, return the corresponding plot
  if(visual_type == "Histogram") {
    if(option == "Overall") {
      return (OR)
    } else if(option == "Low Range") {
      return (L)
    } else if(option == "High Range") {
      return (H)
    } else if(option == "Mid Range") {
      return (M)
    }
  } else if(visual_type == "Barplot") {
    if (option == "Overall Price") {
      return (OP)
    }
  } else if(visual_type == "Boxplot") {
    if (option == "Ram Price Range") {
      return(R)
    } else if(option == "Brand Rating Range") {
      return (BR)
    } else if(option == "Brand Price Range") {
      return (BP)
    }
  } else if(visual_type == "Dot") {
    if (option == "Overall Price Rate") {
      return (OPR)
    }
  }
}

# The rest of the code defines various ggplot objects (OPR, OP, OR, R, BR, BP, L, M, H)
# Each of these objects is a different plot that can be returned by the perform_visualization function
# For example, OPR is a scatter plot of price vs spec_rating, OP is a bar plot of price by ID, etc.
