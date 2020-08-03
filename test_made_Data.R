### Sample Data

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)


### Made-up data:
## 2 years (1 and 2)
## 2 countries (A and B)
## 2 industries (C and D)
## Value added: wages and profits
## Final demand: household consumption

years<-c(1,2)
countryi<-c("A","B")
industryi<-c("C","D")



y<-length(years)
c<-length(countryi)
i<-length(industryi)



### Generate vectors of indentification


year<-rep(years,each=c*i)
country<-rep(rep(countryi, each=i),y)
industry<-rep(industryi, c*y)

### Generate matrix of transactions | first letter= country, second letter= industry

AC<-c(3,2,1,0,4,3,2,1)
AD<-c(2,3,1,1,3,4,4,2)
BC<-c(1,2,4,3,2,3,5,4)
BD<-c(1,1,3,4,2,2,4,5)

T<-data.frame(year,country,industry, AC,AD,BC,BD)

### Generate VA data

wages<-c(2,1,1,1,3,2,2,2)
taxes<-runif(n = 8, min = 0, max = 6)
subsidies<-runif(n = 8, min = 0, max = 6)
profits<-c(2,2,1,3,3,3,2,4)
nmi<-runif(n = 8, min = 0, max = 6)
depreciation<-runif(n = 8, min = 0, max = 6)

VA<-data.frame(year, country, industry, wages, profits,taxes, subsidies, nmi, depreciation)
### Alternative measure of surplus value adding taxes and subsidies

VA<-VA%>%mutate(svalue_2=profits+taxes+subsidies) 

### Generate final demand data

A.household<-c(2,1,1,3,3,4,2,5)
B.household<-c(1,1,2,2,2,2,1,2)


A.nonprofit<-runif(n = 8, min = 0, max = 6)
B.nonprofit<-runif(n = 8, min = 0, max = 6)

A.government<-runif(n = 8, min = 0, max = 6)
B.government<-runif(n = 8, min = 0, max = 6)

A.capitalformation<-runif(n = 8, min = 0, max = 6)
B.capitalformation<-runif(n = 8, min = 0, max = 6)

A.inventories<-runif(n = 8, min = 0, max = 6)
B.inventories<-runif(n = 8, min = 0, max = 6)

A.acquisitions<-runif(n = 8, min = 0, max = 6)
B.acquisitions<-runif(n = 8, min = 0, max = 6)

FD<-data.frame(year,country,industry, 
               A.household, A.nonprofit,A.government,A.capitalformation,A.inventories,A.acquisitions,
               B.household, B.nonprofit,  B.government,B.capitalformation, B.inventories,B.acquisitions)


### Wages from the demand side

#Sum household and nonprofit by country
W_D_1<-FD %>% select(year,country, industry, ends_with("household"),ends_with("nonprofit") ) %>%group_by(year) %>% summarise_if(is.numeric, sum) 

W_D_2<-W_D_1%>% pivot_longer(-year,names_to="var")

country_D<-rep(countryi,2*y)

W_D_3<-W_D_2%>% mutate(country_D)


# Final demand by country and year W_D


W_D<-W_D_3%>%group_by(year,country_D) %>% summarise_if(is.numeric,sum) %>% mutate(wages_D=value) %>% select(-value) %>% arrange(year)

rm(W_D_1,W_D_2,W_D_3)



### Calculate constant capital (summing rows in T dataframe)


variables_industry<-T %>% group_by(year)%>%summarise_if(is.numeric,sum) #Sum across all columns (by year)

# Transform to a tidy form to manipulate

variables_industry<-variables_industry %>% pivot_longer(-year,"i", values_to="constant") 

#Add year and country

variables_industry<-variables_industry%>%mutate(country,industry)%>%select(year, country, industry, constant)

# Add Value-Added to our data.frame of variables

variables_industry<-variables_industry %>% mutate(wages=VA$wages, profits=VA$profits, svalue_2=VA$svalue_2 ,depreciation=VA$depreciation, c=constant+VA$depreciation,
                                                  Y_1=VA$wages+VA$profits,Y_2=VA$wages+VA$svalue_2)

### Generate our marxist variables:

# 1: Using profits as surplus-value | 2: Using profits + taxes + subsidies as surplus value
variables_industry<-variables_industry %>% mutate(h=c/wages, sigma_1=profits/wages,
                                                  gamma_1=Y_1/(c+wages),r_1=profits/(c+wages))
                                                  
variables_industry<-variables_industry %>% mutate(sigma_2=svalue_2/wages,
                                                  gamma_2=Y_2/(c+wages), r_2=svalue_2/(c+wages))

### By economic activity
variables_activity<-variables_industry %>%group_by(year,industry) %>% summarise_if(is.numeric, sum) 


#1

variables_activity<-variables_activity %>% mutate(h=c/wages, sigma_1=profits/wages,  gamma_1=Y_1/(c+wages), 
                                                  r_1=profits/(c+wages))

#2

variables_activity<-variables_activity %>% mutate(sigma_2=svalue_2/wages,  gamma_2=Y_2/(c+wages), 
                                                  r_2=svalue_2/(c+wages))



### Now at the country level 

variables_country<-variables_industry  %>% group_by(year, country) %>% summarise_if(is.numeric,sum)

#Add wages from the demand side
w_D<-W_D$wages_D

variables_country<-data.frame(variables_country,w_D) ### Ojo aquí, ¿por qué sí puedo con data.frame y no con mutate?

#should be 
# variables_country_1<-variables_country %>% mutate(w_D)


# Generate the marxist variables at the country level
#Now: D if we use wages from the Demand side


#1

variables_country<-variables_country %>% mutate(h=c/wages, sigma_1=profits/wages,  gamma_1=Y_1/(c+wages), 
                                                r_1=profits/(c+wages))
#1_D
variables_country<-variables_country %>% mutate(h_D=c/w_D, sigma_1_D=profits/w_D,  gamma_1_D=profits+w_D/(c+w_D), 
                                                r_1=profits/(c+w_D))

#2

variables_country<-variables_country %>% mutate(sigma_2=svalue_2/wages,  gamma_2=Y_2/(c+wages), 
                                                r_2=svalue_2/(c+wages))

#2_D

variables_country<-variables_country %>% mutate(sigma_2_D=svalue_2/w_D,  gamma_2_D=svalue_2+w_D/(c+w_D), 
                                                r_2=svalue_2/(c+w_D))

#### Now at the year level

variables_year<-variables_country %>% group_by(year) %>% summarise_if(is.numeric,sum)

#1

variables_year<-variables_year %>% mutate(h=c/wages, sigma_1=profits/wages,  gamma_1=Y_1/(c+wages), 
                                                r_1=profits/(c+wages))
#1_D
variables_year<-variables_year %>% mutate(h_D=c/w_D, sigma_1_D=profits/w_D,  gamma_1_D=profits+w_D/(c+w_D), 
                                                r_1=profits/(c+w_D))

#2

variables_year<-variables_year %>% mutate(sigma_2=svalue_2/wages,  gamma_2=Y_2/(c+wages), 
                                                r_2=svalue_2/(c+wages))

#2D

variables_year<-variables_year %>% mutate(sigma_2_D=svalue_2/w_D,  gamma_2_D=svalue_2+w_D/(c+w_D), 
                                                r_2=svalue_2/(c+w_D))



####### Plots of time series


# All industries + countries 
# Impractical to plot all 4914 ind industries, but by using filter() we can plot whatever country and industry we want

variables_industry %>%  ggplot(aes(year,h )) +
  geom_line()


variables_industry %>%  ggplot(aes(year, sigma_1 )) +
  geom_line()

# All countries
#Impractical to plot all 189 countries, but by using filter() we can plot whatever country-set of countries we want


# World

variables_year %>% ggplot(aes(year, sigma_1))+ geom_line()


###economic activity 

