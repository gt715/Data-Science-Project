# This function cleans the input data
clean_data <- function(main_data) {
  # Remove rows with missing values
  main_data <- na.omit(main_data)
  
  # Remove duplicate rows
  main_data<- unique(main_data)
  
  # Calculate the mean of the 'price' column
  mean_data <- mean(main_data$price)
  
  # Calculate the standard deviation of the 'price' column
  sd_data <- sd(main_data$price)
  
  # Identify outliers in the 'price' column
  outliers <<- main_data$price[abs(main_data$price - mean_data) > 3*sd_data]
  
  # Loop to remove outliers until there are none left
  while (length(outliers)!=0) {
    mean_data <- mean(main_data$price)
    sd_data <- sd(main_data$price)
    outliers <<- main_data$price[abs(main_data$price - mean_data) > 3*sd_data]
    main_data<-main_data[!(main_data$price %in% outliers), ]
  }
  
  # Return the cleaned data
  return(main_data)
}

# This function displays issues in the dataset
display_issues <- function(DS) {
  # Count the number of missing values
  N_data<-sum(is.na(DS))
  
  # Count the number of duplicate rows
  D_data=sum(duplicated(DS))
  
  # Calculate the mean of the 'price' column
  mean_data <- mean(DS$price)
  
  # Calculate the standard deviation of the 'price' column
  sd_data <- sd(DS$price)
  
  # Identify outliers in the 'price' column
  outliers <<- DS$price[abs(DS$price - mean_data) > 3*sd_data]
  
  # Count the number of outliers
  O_data<-length(outliers)
  
  #summarize of  the issues in the dataset
  issues <-  paste("Numbers of null data :",N_data,
                    "\nNumbers of duplicated data :",D_data,
                    "\nNumbers of duplicated data :",D_data,
                    "\nNumbers of outliers data :",O_data,
                    "\nType of (Spec_rating) :",data.class(DS$spec_rating),
                    "\nType of (price column) :",data.class(DS$price)
                  )
  
  # Return the issues
  return (issues)
}
