
clean_data <- function(main_data) {
  main_data <- na.omit(main_data)
  main_data<- unique(main_data)
  mean_data <- mean(main_data$price)
  sd_data <- sd(main_data$price)
  outliers <<- main_data$price[abs(main_data$price - mean_data) > 3*sd_data]
  while (length(outliers)!=0) {
    mean_data <- mean(main_data$price)
    sd_data <- sd(main_data$price)
    outliers <<- main_data$price[abs(main_data$price - mean_data) > 3*sd_data]
    main_data<-main_data[!(main_data$price %in% outliers), ]
  }
  return(main_data)
}


display_issues <- function(DS) {
  N_data<-sum(is.na(DS))
  D_data=sum(duplicated(DS))
  mean_data <- mean(DS$price)
  sd_data <- sd(DS$price)
  outliers <<- DS$price[abs(DS$price - mean_data) > 3*sd_data]
  O_data<-length(outliers)
  issues <-  paste("Numbers of null data :",N_data,
                    "\nNumbers of duplicated data :",D_data,
                    "\nNumbers of duplicated data :",D_data,
                    "\nNumbers of outliers data :",O_data,
                    "\nType of (Spec_rating) :",data.class(DS$spec_rating),
                    "\nType of (price column) :",data.class(DS$price)
                  )
  return (issues)
}
