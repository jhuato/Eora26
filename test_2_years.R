library(dplyr)
library(tidyr)
library(data.table)


### Everything for only two years

setwd("C:/Users/User/Documents/2020_Summer/R/Test 2 years" )


years<-c(1990,1991)

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

Demandi<-c("household", "non-profit", "government", "capital formation","inventories", 
           "acquisitions less disposals")

vai <-c("Wages", "Prod taxes", "Prod subsidies", "Profits", 
              "Net mixed income", "Depreciation")


country<-rep(rep(countryi, each=26),2)
industry<-rep(rep (industryi, 189),2)
year<-rep(years, each=189*26)


#### First Value Added

list_of_files <- list.files(path = ".", recursive = TRUE,        ###This is the command for reading all the _bp_VA.txt files
                            pattern = "_bp_VA.txt",                   ### However it is not in the scan form but in read.delim
                            full.names = TRUE)  



VA<-rbindlist(sapply(list_of_files, fread, simplify = FALSE),
          use.names = TRUE)

### Remove column of statistical discrepancies

VA<-VA[,-4915]

### Transpose the matrix
VA<-t(VA)
VA<-data.frame(VA)


###Name the columns

nam<-paste(rep(vai,2),rep(years,each=6), sep=",")
colnames(VA)<-nam

VA<-data.frame(rep(countryi,each=26), rep(industryi,189),VA)

VA<-pivot_longer(VA, 3:14, "vai_year","v")

year<-rep(rep(years, each=6),189)
VAvar<-rep(vai, 189*2)

VA<-data.frame(year,VA[,-3], VAvar)

VA<-pivot_wider(VA, 1:3, "VAvar","value")
colnames(VA)<-c("year","country","industry", vai)

VA<-arrange(VA, year)



##########################################################
################### Now for final demand #################


list_of_files <- list.files(path = ".", recursive = TRUE,        ###This is the command for reading all the _bp_VA.txt files
                            pattern = "_bp_FD.txt",                   ### However it is not in the scan form but in read.delim
                            full.names = TRUE)         

FD <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),
                use.names = TRUE)


year<-rep(years, each=189*26)

############ Remove statistical discrepancies ###########

FD<-FD[-c(4915*1,4915*2),-c(1135:1140)]

colnames(FD)<-paste(rep(countryi,each=6),rep(Demandi, 189), sep=",")

FD<-data.frame(year, country, industry, FD)



#############################################################################
############# Now for Transactions with supply as rows ######################


list_of_files <- list.files(path = ".", recursive = TRUE,        ###This is the command for reading all the _bp_VA.txt files
                            pattern = "_bp_T.txt",                   ### However it is not in the scan form but in read.delim
                            full.names = TRUE)         

TS <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),
                use.names = TRUE)


############# Remove statistical discrepancies ########################

TS<-TS[-c(4915*1,4915*2),-4915]

####### Build another data frame where demand of each industry is in the rows (for an easier manipulation)



####################### names ############

colnames(TS)<-paste(rep(countryi, each=26),rep(industryi,189),sep=",")



TS<-data.frame(year, country, industry, TS)

