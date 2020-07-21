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

n<-length(years)
m<-length(countryi)
p<-length(industryi)


### Generate vectors of indentification


year<-rep(years,each=m*p)
country<-rep(rep(countryi, each=p),n)
industry<-rep(industryi, m*n)

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

### Generate final demand data

A.household<-c(2,1,1,3,3,4,2,5)
B.household<-c(1,1,2,2,2,2,1,2)


A.non-profit<-runif(n = 8, min = 0, max = 6)
B.non-profit<-runif(n = 8, min = 0, max = 6)

A.government<-runif(n = 8, min = 0, max = 6)
B.government<-runif(n = 8, min = 0, max = 6)

A.capitalformation<-runif(n = 8, min = 0, max = 6)
B.capitalformation<-runif(n = 8, min = 0, max = 6)

A.inventories<-runif(n = 8, min = 0, max = 6)
B.inventories<-runif(n = 8, min = 0, max = 6)

A.acquisitions<-runif(n = 8, min = 0, max = 6)
B.acquisitions<-runif(n = 8, min = 0, max = 6)

FD<-data.frame(year,country,industry, A.household,B.household, A.government, B.government,
               A.capitalformation, B.capitalformation, A.inventories,B.inventories,A.acquisitions,
               B.acquisitions)


### Calculate our relevant variables by industry

### Constant capital




variables_industry<-T %>% group_by(year)%>%summarise_if(is.numeric,sum) #Sum across all columns (by year)

# Transform to a tidy form to manipulate

variables_industry<-variables_industry %>% pivot_longer(-year,"i", values_to="constant") 

###Add year and country

variables_industry<-variables_industry%>%mutate(country,industry)%>%select(year, country, industry, constant)

### Add Value-Added to our data.frame of variables

variables_industry<-variables_industry %>% mutate(wages, profits, depreciation, Y=wages+profits, c=constant+depreciation)

## Generate our marxist variables:

variables_industry<-variables_industry %>% mutate(sigma=profits/wages,h=c/wages,gamma=Y/(c+wages),
                                                  r=profits/(c+wages))


########### Now at the country level 

variables_country<-variables_industry  %>% group_by(year, country) %>% summarise_if(is.numeric,sum)





# Generate the marxist variables at the country level#

variables_country<-variables_country %>% mutate(sigma=profits/wages, h=c/wages, gamma=Y/(c+wages), 
                                                r=profits/(c+wages))


#### Now at the year level

variables_year<-variables_country %>% group_by(year) %>% summarise_if(is.numeric,sum)

names(variables_industry)
#%>%  select(year,country,industry,c, wages, profits, Y)



# Generate the marxist variables at the year level ####

variables_year<-variables_year %>% mutate(sigma=profits/wages,h=c/wages,gamma=Y/(c+wages),
                                          r=profits/(c+wages))

### Missing in this code: 
# Wages from the demand side
# Depreciation included in the calculation of constant capital
# Taxes and subsidies included (or not) into our measure of surplus value


####### Plots of time series



# All industries + countries
# All countries
# World


#####Wages from the demand-side#####

#W_D<-FD %>% group_by(year) %>% summarise_if(is.numeric,sum)

#W_D<-W_D %>%pivot_longer(-year,"country",values_to="W_D")

#Pendiente


rm(list=ls())

