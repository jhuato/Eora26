#### Final demand


library(dplyr)
library(tidyr)
library(data.table)

Demandi<-c("household", "non-profit", "government", "capital formation","inventories", "acquisitions less disposals")

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


### Test, for one year###

FD1990<-read.delim("C:/Users/User/Documents/2020_Summer/R/Eora26/Eora26_1990_bp_FD.txt", header = FALSE)


FD1990[,1135:1140]
FD1990[c(4915),1:5]

FD1990<-FD1990[-4915,-c(1135:1140)]

country<-rep(countryi, each=26)
industry<-rep(industryi, 189)


countryn<-rep(countryi, each=6)
Demandin<-rep(Demandi, 189)
nam<-paste(countryn, Demandin, sep=",")
nam[1:20]

FD1990<-data.frame(country, industry, FD1990)

names(FD1990)<-c("country", "industry", nam)
names

rm(FD1990)

################### Now for all years

list_of_files <- list.files(path = ".", recursive = TRUE,        ###This is the command for reading all the _bp_VA.txt files
                            pattern = "_bp_FD.txt",                   ### However it is not in the scan form but in read.delim
                            full.names = TRUE)         

DT <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),
                use.names = TRUE)

### Obs=127790 | should be 26 less: 189x26x26

#Identify them:

sum(DT[c(127764:127770),])

sum(DT[4915,])
sum(DT[4915*2,])
sum(DT[4915*3,])

sum(DT[,1135:1140])


#for(i in 1:26){
#i<-DT[c(4915*i),]
#}

#for(i in 1:26){
#DT3<-DT[-((4915*i)-(i-1),]
#}

DT<-DT[-c(4915*1,4915*2,4915*3,4915*4,4915*5,4915*6,4915*7,4915*8,4915*9,4915*10,4915*11,4915*12,4915*13,
          4915*14,4915*15,4915*16,4915*17,4915*18,4915*19,4915*20,4915*21,4915*22,4915*23,4915*24,4915*25,
          4915*26),-c(1135:1140)]

country<-rep(rep(countryi, each=26),26)
industry<-rep(rep(industryi,189),26)
year<-rep(years, each=189*26)

DT<-data.frame(year,country,industry, DT)

countryn<-rep(countryi, each=6)
Demandin<-rep(Demandi, 189)
nam<-paste(countryn, Demandin, sep=",")


names(DT)<-c("year","country", "industry", nam)
FD<-DT

rm(DT)

country
