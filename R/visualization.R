
library(gridExtra)
library(ggplot2)
source("R/data_cleaning.R")

DS <- clean_data(read.csv("C:\\Games\\DB project\\datascience-project-main (1)\\datascience-project-main\\data\\Laptop-Dataset.csv"))

perform_visualization <- function(visual_type, option) {

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



################################################################################


##OVER_ALL##

#OverAll_Price_Rate
OPR<- ggplot(DS, aes(x = price, y=spec_rating))+
  geom_jitter()+
  geom_point()+
  ggtitle("Price_Rating")

################################################################################

#OverAll_Price
OP<-ggplot(DS,aes(ID,price,fill=brand))+
   geom_col()+
   ggtitle("Prices")
 
################################################################################

#Over_All_Rating
OR<- ggplot(DS, aes( spec_rating)) +
  geom_histogram(color="red",fill="black")+
   scale_x_continuous(breaks = seq(60,90, by = 5))+
  ggtitle("Spec_Rating_Freq")


################################################################################

#OvarrAll_Ram
R<-ggplot(DS,aes(Ram,price))+
  geom_boxplot()+
  ggtitle("Ram_Price_Rang")


################################################################################

##BRAND##

#Brands_Rate
BR<- ggplot(DS, aes(brand,spec_rating )) +
  
     geom_boxplot()+
  ggtitle("Brand_Rating_Range")
  
################################################################################  
  
#Brand_Price
BP<- ggplot(DS, aes(brand,price )) +
    geom_boxplot()+
    ggtitle("Brand_Price_Range")
  

################################################################################


##RANGE##

#LowR_Rate
low<-DS[DS$price<=40000,]
L<- ggplot(low, aes(spec_rating)) +
    geom_histogram(color="red",fill="black")+
    scale_x_continuous(breaks = seq(60,90, by = 5))+
    ggtitle("Low_Range_Rating")

################################################################################  
  
#MidR_Rate
mid<-DS[DS$price>40000&DS$price<=90000,]
M<-ggplot(mid, aes(spec_rating)) +
   geom_histogram(color="red",fill="black")+
   scale_x_continuous(breaks = seq(60,90, by = 5))+
   ggtitle("Mid_Range_Rating")
  
################################################################################

#HighR_Rate
high<-DS[DS$price>90000,]
H<- ggplot(high, aes(spec_rating)) +
    geom_histogram(color="red",fill="black")+
    scale_x_continuous(breaks = seq(60,90, by = 5))+
    ggtitle("High_Range_Rating")
  
################################################################################  

#Arrange

#OVERALL

  #grid.arrange(OPR,OP,OR,R,nrow=2)

#BRAND

  #grid.arrange(BR,BP,nrow=2)
  
#RANGE
  
  #grid.arrange(L,M,H,nrow=1)
