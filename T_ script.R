### Transactions

T1990<-read.delim("C:/Users/User/Documents/2020_Summer/R/Eora26/Eora26_1990_bp_T.txt", header = FALSE)
T1990<-T1990[-4915,-4915]

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



industry<-rep(industryi, 189)
country<-rep(countryi,each=26)




na<-paste(country, industry, sep=",")
length(na)

names(T1990)<-c(na)


T1990<-data.frame(country, industry, T1990)


#############################

###AFG agriculture

VA_AFG_Agr<-VA6[27,4:9]
FD_AFG_Agr<-FD[1, 4:1137]

Demand_AFG_Agr<-T1990$"AFG.Agriculture"


Supply_AFG_Agr<-T1990[1,3:4916]

###1
sum(Supply_AFG_Agr)+sum(FD_AFG_Agr)

sum(Demand_AFG_Agr)+sum(VA_AFG_Agr)



###Agriculture in Albania

VA_ALB_Agr<-VA6[79,4:9]
FD_ALB_Agr<-FD[27,4:1137]

Demand_ALB_Agr<-T1990$"ALB.Agriculture"
  
Supply_ALB_Agr<-T1990[27,3:4916]


sum(Supply_ALB_Agr)+sum(FD_ALB_Agr)

sum(Demand_ALB_Agr)+sum(VA_ALB_Agr)


### Apparently the matriz is not trasposed, i.e. columns= demand of inputs|rows= supply

### However the magnitudes do not coincide perfectly