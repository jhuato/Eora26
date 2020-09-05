library(dplyr)
library(tidyr)
library(data.table)
library("readxl")
library(ggplot2)

### Everything for only two years

setwd("C:/Users/User/Documents/2020_Summer/R/Eora26" )


years<-1990:2015

industryi <- c("Agriculture", "Fishing", "Mining and Quarrying", 
               "Food & Beverages", "Textiles and Wearing Apparel", 
               "Wood and Paper", "Petroleum, Chemical and Non-Metallic Mineral Products", 
               "Metal Products", "Electrical and Machinery", "Transport Equipment", 
               "Other Manufacturing", "Recycling", "Electricity, Gas and Water", 
               "Construction", "Maintenance and Repair", "Wholesale Trade", 
               "Retail Trade", "Hotels and Restraurants", "Transport", 
               "Post and Telecommunications", 
               "Financial Intermediation and Business Activities", 
               "Public Administration", "Education, Health and Other Services", 
               "Private Households", "Others", "Re-export & Re-import")

countryi <- c("AFG", "ALB", "DZA", "AND", "AGO", "ATG", "ARG", "ARM", "ABW", "AUS", 
              "AUT", "AZE", "BHS", "BHR", "BGD", "BRB", "BLR", "BEL", "BLZ", "BEN", 
              "BMU", "BTN", "BOL", "BIH", "BWA", "BRA", "VGB", "BRN", "BGR", "BFA", 
              "BDI", "KHM", "CMR", "CAN", "CPV", "CYM", "CAF", "TCD", "CHL", "CHN", 
              "COL", "COG", "CRI", "HRV", "CUB", "CYP", "CZE", "CIV", "PRK", "COD", 
              "DNK", "DJI", "DOM", "ECU", "EGY", "SLV", "ERI", "EST", "ETH", "FJI", 
              "FIN", "FRA", "PYF", "GAB", "GMB", "GEO", "DEU", "GHA", "GRC", "GRL", 
              "GTM", "GIN", "GUY", "HTI", "HND", "HKG", "HUN", "ISL", "IND", "IDN", 
              "IRN", "IRQ", "IRL", "ISR", "ITA", "JAM", "JPN", "JOR", "KAZ", "KEN", 
              "KWT", "KGZ", "LAO", "LVA", "LBN", "LSO", "LBR", "LBY", "LIE", "LTU", 
              "LUX", "MAC", "MDG", "MWI", "MYS", "MDV", "MLI", "MLT", "MRT", "MUS", 
              "MEX", "MCO", "MNG", "MNE", "MAR", "MOZ", "MMR", "NAM", "NPL", "NLD", 
              "ANT", "NCL", "NZL", "NIC", "NER", "NGA", "NOR", "PSE", "OMN", "PAK", 
              "PAN", "PNG", "PRY", "PER", "PHL", "POL", "PRT", "QAT", "KOR", "MDA", 
              "ROU", "RUS", "RWA", "WSM", "SMR", "STP", "SAU", "SEN", "SRB", "SYC", 
              "SLE", "SGP", "SVK", "SVN", "SOM", "ZAF", "SDS", "ESP", "LKA", "SUD", 
              "SUR", "SWZ", "SWE", "CHE", "SYR", "TWN", "TJK", "THA", "MKD", "TGO", 
              "TTO", "TUN", "TUR", "TKM", "USR", "UGA", "UKR", "ARE", "GBR", "TZA", 
              "USA", "URY", "UZB", "VUT", "VEN", "VNM", "YEM", "ZMB", "ZWE")

demandi<-c("household", "nonprofit", "government", "capital","inventories", 
           "acquisitions")

vai <-c("wages", "taxes", "subsidies", "profits", 
        "nmi", "depreciation")

y<-length(years)
c<-length(countryi)
i<-length(industryi)

v<-length(vai)
d<-length(demandi)

###Add classification vectors (with the desired size) ######

classif <- read_excel("country_classif_Eora26.xlsx")

region<-as.factor(rep(rep(classif$wb_region_i, each=i),y))

inc_group<-as.factor(rep(rep(classif$wb_inc_group_i, each=i),y))

cia<-as.factor(rep(rep(classif$cia_rich_poor_i, each=i),y))

productive<-as.factor(rep(rep(classif$productive, each=i),y))

rm(classif)



#### First Value Added

list_of_files <- list.files(path = ".", recursive = TRUE,        
                            pattern = "_bp_VA.txt",                   
                            full.names = TRUE)  



VA<-rbindlist(sapply(list_of_files, fread, simplify = FALSE),
              use.names = TRUE)

### Remove column of statistical discrepancies

VA<-VA[,-4915]

### Transpose the matrix
VA<-t(VA)
VA<-data.frame(VA)


###Name the columns

colnames(VA)<-paste(rep(vai,y),rep(years,each=v), sep=",")

VA <- VA%>% mutate(country=rep(countryi,each=i), industry=rep(industryi,c)) %>% 
  relocate(country,industry) 

VA<-VA %>% pivot_longer(3:(2+v*y), "vai_year","v")

year<-rep(rep(years, each=v),c*i)
VAvar<-rep(vai, c*y*i)



VA<-VA%>% mutate(year,VAvar)

VA<-VA%>% select(-vai_year) 

VA<-VA%>% relocate(year,country,industry,VAvar, value)



VA<-VA %>% pivot_wider(1:3, "VAvar","value")
colnames(VA)<-c("year","country","industry", vai)

VA<-VA %>% arrange(year)

VA<-VA %>% mutate(region,inc_group,cia) %>% 
  relocate(year,country,region, inc_group,cia)


##########################################################
################### Now for final demand #################

country<-rep(rep(countryi, each=i),y)
industry<-rep(rep (industryi, c),y)
year<-rep(years, each=c*i)

###################################################

list_of_files <- list.files(path = ".", recursive = TRUE,       
                            pattern = "_bp_FD.txt",                   
                            full.names = TRUE)         

FD <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),
                use.names = TRUE)




############ Remove statistical discrepancies ###########

#FD<-FD[-c(4915*1,4915*2),-c(1135:1140)]

#for 26 years: 
FD<-FD[-c(4915*1,4915*2,4915*3,4915*4,4915*5,4915*6,4915*7,4915*8,4915*9,4915*10,4915*11,4915*12,4915*13,4915*14,4915*15,4915*16,4915*17,4915*18,4915*19,4915*20,4915*21,4915*22,4915*23,4915*24,4915*25,4915*26),-c(1135:1140)]

colnames(FD)<-paste(rep(countryi,each=d),rep(demandi, c), sep=".")

FD<-data.frame(year, country, region, inc_group,cia,industry, FD)




#############################################################################
############# Now for Transactions with supply as rows ######################

memory.limit(size=100000)

#list_of_files <- list.files(path = ".", recursive = TRUE,        
#                            pattern = "_bp_T.txt",                  
#                            full.names = TRUE)         

#T <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),
#               use.names = TRUE)



############# Remove statistical discrepancies ########################

#T<-T[-c(4915*1,4915*2),-4915]

#for 26 years
#T<-T[-c(4915*1,4915*2,4915*3,4915*4,4915*5,4915*6,4915*7,4915*8,4915*9,4915*10,4915*11,4915*12,4915*13,4915*14,4915*15,4915*16,4915*17,4915*18,4915*19,4915*20,4915*21,4915*22,4915*23,4915*24,4915*25,4915*26),-4915]





####################### names ############

#colnames(T)<-paste(rep(countryi, each=i),rep(industryi,c),sep=".")



#T<-T %>% mutate(year, country, industry) %>% relocate(year,country, industry)

load("~/2020_Summer/R/Eora26/T.Rdata")

#### resume here if loaded te object

T<-T %>% mutate(region, inc_group,cia,industry) %>% 
  relocate(year,country,region, inc_group,cia,industry, industry)


########## Form here: pasting test_made_Data

###Building alternative measure of sv 

VA<-VA%>%mutate(svalue_2=profits+taxes+subsidies) 

### Wages from the demand side

#Sum household and nonprofit by country
W_D_1<-FD %>% 
  select(year,country, industry, ends_with("household"),ends_with("nonprofit") ) %>%
  group_by(year) %>% summarise_if(is.numeric, sum) 

W_D_2<-W_D_1%>% pivot_longer(-year,names_to="var")

country_D<-rep(countryi,2*y)

W_D_3<-W_D_2%>% mutate(country_D)

# Final demand by country and year W_D

W_D<-W_D_3%>%group_by(year,country_D) %>% 
  summarise_if(is.numeric,sum) %>% mutate(wages_D=value) %>% 
  select(-value) %>% arrange(year)

rm(W_D_1,W_D_2,W_D_3)



### Calculate constant capital (summing rows in T dataframe)


variables_industry<-T %>% 
  group_by(year)%>%summarise_if(is.numeric,sum) 

# Transform to a tidy form to manipulate

variables_industry<-variables_industry %>% 
  pivot_longer(-year,"i", values_to="constant") 

#Add year and country (and classif variables)

variables_industry<-variables_industry%>%
  mutate(country,industry, region, inc_group,cia,productive)%>%
  select(year, country,region, inc_group,cia, industry, constant,productive)






# Add Value-Added to our data.frame of variables (problem here)

variables_industry<-variables_industry %>%
  mutate(wages=VA$wages, profits=VA$profits, svalue_2=VA$svalue_2, 
         depreciation=VA$depreciation, c=constant+VA$depreciation, 
         Y_1=VA$wages+VA$profits,Y_2=VA$wages+VA$svalue_2,
         taxes=VA$taxes, subsidies=VA$subsidies, 
         nmi=VA$nmi, depreciation=VA$depreciation)


save(variables_industry, file="variables_industry.Rdata")

#load("~/2020_Summer/R/Eora26/variables_industry.Rdata")
########## Here delete KAZ####

### Generate our marxist variables:

# 1: Using profits as surplus-value | 2: Using profits + taxes + subsidies as surplus value
variables_industry<-variables_industry %>% 
  mutate(h=c/wages, sigma_1=profits/wages,
         gamma_1=Y_1/(c+wages),r_1=profits/(c+wages),
         totalVA=wages+profits+depreciation+nmi+taxes+subsidies)

variables_industry<-variables_industry %>% 
  mutate(sigma_2=svalue_2/wages,
         gamma_2=Y_2/(c+wages), r_2=svalue_2/(c+wages))


### By economic activity
variables_activity<-variables_industry %>%
  group_by(year,industry) %>% summarise_if(is.numeric, sum) 


#1

variables_activity<-variables_activity %>% 
  mutate(h=c/wages, sigma_1=profits/wages,  
         gamma_1=Y_1/(c+wages), r_1=profits/(c+wages))

#2

variables_activity<-variables_activity %>% 
  mutate(sigma_2=svalue_2/wages,  
         gamma_2=Y_2/(c+wages), r_2=svalue_2/(c+wages))



### Now at the country level 

variables_country<-variables_industry %>% 
  group_by(year,country,inc_group,region,cia,productive) %>% 
  summarise_if(is.numeric,sum)

#Add wages from the demand side
w_D<-W_D$wages_D

variables_country<-data.frame(variables_country,w_D) ### Ojo aquí, ¿por qué sí puedo con data.frame y no con mutate?

#Error in data.frame(variables_country, w_D) : 
#arguments imply differing number of rows: 4888, 4914


#should be 
# variables_country_1<-variables_country %>% mutate(w_D)


# Generate the marxist variables at the country level
#Now: D if we use wages from the Demand side


#1

variables_country<-variables_country %>% 
  mutate(h=c/wages, sigma_1=profits/wages, gamma_1=Y_1/(c+wages), 
                                                r_1=profits/(c+wages))

#1_D
variables_country<-variables_country %>% 
  mutate(h_D=c/w_D, sigma_1_D=profits/w_D,gamma_1_D=profits+w_D/(c+w_D), 
                                                r_1=profits/(c+w_D))

#2

variables_country<-variables_country %>% 
  mutate(sigma_2=svalue_2/wages,  gamma_2=Y_2/(c+wages), 
                                                r_2=svalue_2/(c+wages))

#2_D

variables_country<-variables_country %>% 
  mutate(sigma_2_D=svalue_2/w_D,  gamma_2_D=svalue_2+w_D/(c+w_D), 
                                                r_2=svalue_2/(c+w_D))

#### Now at the year level

variables_year<-variables_country %>% 
  group_by(year) %>% summarise_if(is.numeric,sum)

#1

variables_year<-variables_year %>% 
  mutate(h=c/wages, sigma_1=profits/wages,  gamma_1=Y_1/(c+wages), 
                                          r_1=profits/(c+wages))
#1_D
variables_year<-variables_year %>% 
  mutate(h_D=c/w_D, sigma_1_D=profits/w_D,  gamma_1_D=profits+w_D/(c+w_D), 
                                          r_1=profits/(c+w_D))

#2

variables_year<-variables_year %>% 
  mutate(sigma_2=svalue_2/wages,  gamma_2=Y_2/(c+wages), 
                                          r_2=svalue_2/(c+wages))

#2D

variables_year<-variables_year %>% 
  mutate(sigma_2_D=svalue_2/w_D,  gamma_2_D=svalue_2+w_D/(c+w_D), 
                                          r_2=svalue_2/(c+w_D))





### By region

variables_region<-variables_industry %>% 
  group_by(year,region) %>% summarise_if(is.numeric,sum)


#1
variables_region<-variables_region %>% 
  mutate(h=c/wages, sigma_1=profits/wages,  gamma_1=Y_1/(c+wages), 
                                                r_1=profits/(c+wages))


#2

variables_region<-variables_region %>% 
  mutate(sigma_2=svalue_2/wages,  gamma_2=Y_2/(c+wages), 
                                                r_2=svalue_2/(c+wages))








### By inc_group

variables_inc_group<-variables_industry %>% 
  group_by(year,inc_group) %>% summarise_if(is.numeric,sum)

#1
variables_inc_group<-variables_inc_group %>% 
  mutate(h=c/wages, sigma_1=profits/wages,  gamma_1=Y_1/(c+wages), 
                                              r_1=profits/(c+wages))


#2

variables_inc_group<-variables_inc_group %>% 
  mutate(sigma_2=svalue_2/wages,  gamma_2=Y_2/(c+wages), 
                                              r_2=svalue_2/(c+wages))

### By CIA class of rich and poor

variables_cia<-variables_industry %>% group_by(year,cia) %>% 
  summarise_if(is.numeric,sum)


variables_cia<-variables_cia %>% 
  mutate(h=c/wages, sigma_1=profits/wages,  gamma_1=Y_1/(c+wages), 
                                                    r_1=profits/(c+wages))


#2

variables_cia<-variables_cia %>% 
  mutate(sigma_2=svalue_2/wages,  gamma_2=Y_2/(c+wages), 
                                                    r_2=svalue_2/(c+wages))




##### For most productive countries

variables_productive<-variables_industry %>% group_by(year,productive) %>% 
  summarise_if(is.numeric,sum)


variables_productive<-variables_productive %>% 
  mutate(h=c/wages, sigma_1=profits/wages,  gamma_1=Y_1/(c+wages), 
         r_1=profits/(c+wages))


#2

variables_productive<-variables_productive %>% 
  mutate(sigma_2=svalue_2/wages,  gamma_2=Y_2/(c+wages), 
         r_2=svalue_2/(c+wages))

##############################################################################
############## Finding all problems plot c, profits and wages ################

#Plot Constant capital
variables_year %>% ggplot(aes(x=year, y=c))+geom_point()+geom_line()+
  labs(y="Constant capital", x="Year", title="Constant Capital- World")

# Problem 1: Sharp increase from 1990 to 1991, 
#then sharp decline from 1992 to 1993 (unlikely)

#Plot wages
variables_year %>% ggplot(aes(x=year, y=wages))+geom_point()+geom_line()+
  labs(y="Wages", x="Year", title="Total wages - World")

# Problem 2: Sharp decline from 2013 to 2014 and even more from 2014 to 2015

#Plot Profits
variables_year %>% ggplot(aes(x=year, y=profits))+geom_point()+geom_line()+
  labs(y="Profits", x="Year", title="Total Profits- World")

### No problems... but why fall in profits from 2014 to 2015???

###Address problem 1: Plot constant capital by groups

# C by income group:

variables_inc_group %>% ggplot(aes(x=year, y=c, color=inc_group))+
  geom_point()+geom_line()+
  labs(x="Year",y="Constant Capital",title = "Constant Capital by income")

### Two problems: (a) the sharp rise and then decline happens in income group 3
### (b) A sharp decline takes place in group 2

# C by region:

variables_region %>% ggplot(aes(x=year, y=c, color=region))+
  geom_point()+geom_line()+
  labs(x="Year",y="Constant Capital",title = "Constant Capital by region")

# The sharp rise and then decline takes place in region 2

variables_country %>% 
  filter(region=="2" &inc_group=="2") %>% ggplot(aes(x=year, y=c, color=country))+
  geom_point()+geom_line()+
  labs(x="Year",y="Constant Capital",title = "Constant Capital Region 2, inc group 2")

# Problem identified: absurd decline of c for Armenia (ARM)

variables_country %>% 
  filter(region=="2" &inc_group=="3") %>% ggplot(aes(x=year, y=c, color=country))+
  geom_point()+geom_line()+
  labs(x="Year",y="Constant Capital",title = "Constant Capital Region 2, inc group 3")

# 2nd Problem identified: Absurd increase and decline in KAZ

#Inside each country

variables_industry %>%
  filter(country=="ARM") %>% ggplot(aes(x=year, y=c, color=industry))+
  geom_point()+geom_line()+
  labs(x="Year",y="Constant Capital",title = "Constant Capital in ARM")

# Generalized problem with Armenia

variables_industry %>% 
  filter(country=="KAZ") %>% ggplot(aes(x=year, y=c, color=industry))+
  geom_point()+geom_line()+
  labs(x="Year",y="Constant Capital",title = "Constant Capital in KAZ")

# Generalized problem with KAZ


###Address problem 2: why wages fall so sharply???

# Wages by income group:

variables_inc_group %>% 
  ggplot(aes(x=year, y=wages, color=inc_group))+
  geom_point()+geom_line()+
  labs(x="Year",y="Wages",title = "Wages by income")

#The sharp fall happens in groups 3 and 5

# Wages by region:

variables_region %>% 
  ggplot(aes(x=year, y=wages, color=region))+
  geom_point()+geom_line()+
  labs(x="Year",y="Wages",title = "Wages by region")

###Wages fall sharply in regions 1,2 and 5

#Wages group 3 and region 1

variables_country %>% 
  filter(region=="1" &inc_group=="3") %>% ggplot(aes(x=year, y=wages, color=country))+
  geom_point()+geom_line()+
  labs(x="Year",y="Wages",title = "Wages region 1, inc group 3")

#Problem: CHINA (CHN) Sharp decline in wages

#Wages group 3 and region 2
variables_country %>% 
  filter(region=="2" &inc_group=="3") %>% ggplot(aes(x=year, y=wages, color=country))+
  geom_point()+geom_line()+
  labs(x="Year",y="Wages",title = "Wages region 2, inc group 3")

#Normal

#Wages group 3 and region 5
variables_country %>% 
  filter(region=="5" &inc_group=="3") %>% ggplot(aes(x=year, y=wages, color=country))+
  geom_point()+geom_line()+
  labs(x="Year",y="Wages",title = "Wages region 2, inc group 5")

#Nothing

"Wages group 5"

#Wages group 5 and region 1

variables_country %>% 
  filter(region=="1" &inc_group=="5") %>% ggplot(aes(x=year, y=wages, color=country))+
  geom_point()+geom_line()+
  labs(x="Year",y="Wages",title = "Wages region 1, inc group 5")

#Sharp decline in Japan


#Wages group 5 and region 2
variables_country %>% 
  filter(region=="2" &inc_group=="5") %>% ggplot(aes(x=year, y=wages, color=country))+
  geom_point()+geom_line()+
  labs(x="Year",y="Wages",title = "Wages region 2, inc group 5")

#Sharp decline for most countries

#Wages group 5 and region 5
variables_country %>% 
  filter(region=="5" &inc_group=="5") %>% ggplot(aes(x=year, y=wages, color=country))+
  geom_point()+geom_line()+
  labs(x="Year",y="Wages",title = "Wages region 3, inc group 5")

#Absurd decline in the USA


variables_inc_group %>%ggplot(aes(x=year, y=totalVA, color=inc_group))+
  geom_point()+geom_line()+
  labs(x="Year",y="Total Value Added",title = "Value Added by Income Group")
  
  

