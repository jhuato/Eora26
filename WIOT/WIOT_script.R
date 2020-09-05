library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library("rio")

# setwd("C:/Users/User/Documents/GitHub/Eora26/WIOT") #set my working directory


sea<-import("WIOD_SEA_Nov16.xlsx",sheet=2) #It is in a wide form, we need to transform it
                                            # into a long form (year as a single variable)

sea_1 <-sea %>% pivot_longer("2000":"2014","year", values_to="value")

sea_2<- sea_1 %>% arrange(year,country,code)

sea_3<-sea_2 %>% rename("industry_code"="code","industry_description"="description")


