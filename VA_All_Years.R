### Reading and working with the datasets of all the years(preliminary)


library(dplyr)
library(data.table)

list_of_files <- list.files(path = ".", recursive = TRUE,        ###This is the command for reading all the _bp_VA.txt files
                            pattern = "_bp_VA.txt",                   ### However it is not in the scan form but in read.delim
                            full.names = TRUE)         

DT <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),
                use.names = TRUE)



tDT<-t(DT)
dim(tDT)
tDAT<-tDT[-c(4915),]  ###Eliminate the 'row of zeros'
dim(tDAT)


### Same as before###

variablei <-c("Wages", "Prod taxes", "Prod subsidies", "Profits", 
              "Net mixed income", "Depreciation")


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




#### Create the vectors of industry and country###
country<-rep(countryi, each=26)

industry<-rep(industryi,189)


length(industry)
length(country)

###Combine them
VA<-data.frame(country, industry,tDAT)


###The steps below are just to name all the variables so that it is clear the problem we have to solve to have the data in
### the desired form

vars<-rep(variablei, 26)
ys<-rep(years, each=6)
newnames<-paste(vars, ys, sep=" ")
head(newnames)

newnames<-c("country", "industry", newnames)
head(newnames)

names(VA)<-newnames

View(VA)

###Now the columns are the Value Added variables for each year. So the challenge would be to
## (1)Either remain with the 6 value added variables as columns and add an additional variable for year
###(2)Try to do it in the scan form with only one variable of values