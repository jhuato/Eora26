# JH
# Eora26 2015 VA data set

Y <- scan("~/Downloads/Eora26_2015_bp/Eora26_2015_bp_VA copy JH.txt")
Y <- as.numeric(Y) 
Y <- Y[-(29485:29490)] # to remove the statistical discrepancy row


Y <- as.vector(Y)

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



# variable <-rep(variablei, length(Y)/length(variablei))
# country<-rep(countryi, each=length(variablei)*length(industryi))
# industry<-rep(rep(industryi,each=length(variablei)),length((countryi)))
# VA <- data.frame(country, industry, variable, Y) # Error message

variable <-rep( variablei, each = length(Y)/ length(variablei) )
country <- rep( rep(countryi, each = length(industryi), length(variablei) )
industry <- rep( rep( rep(industryi), length(countryi) ), length(variablei) )

VA <- data.frame(country, industry, variable, Y)
names(VA2016) <- c("country", "industry", "variable", "value")

library(tidyverse)
VA_w <- VA %>% spread(variable, value)
