
# Generate Quarterly Statistics Summary/ Market Survey Report from exported Access tables (excel format)
# 
rm(list=ls())
require(readxl)
require(stringi)
require(plyr)
require(dplyr)
require(ggmap)
require(arcgisbinding)


# FIND AND REPLACE THE CURRENT, PASTYEAR, AND PASTQUARTER STRING VALUES TO ADJUST FOR NEW COLLECTION

projws= "D:/Projects/GNAA/Data/3rdQtr2017Data"
setwd(projws)


current<- read.csv("D:/Projects/GNAA/Data/3rdQtr2017Data/2017-09-30ConventionalSubmarketSummaryStatistics.csv", stringsAsFactors = FALSE)
pastyear<- read.csv("D:/Projects/GNAA/Data/3rdQtr2016Data/2016-09-30ConventionalSubmarketSummaryStatistics.csv", stringsAsFactors = FALSE)
pastquarter<- read.csv("D:/Projects/GNAA/Data/2ndQtr2017Data/2017-06-30ConventionalSubmarketSummaryStatistics.csv", stringsAsFactors = FALSE)

newdf<- data.frame('Submarket'= current$SUBMARKET, 
                   'Q32017_Occ'= current$Overall.Occ.Rate, 'Q32017_AvgRent'= current$Overall.Rent, 
                   'Q32016_Occ'=pastyear$Overall.Occ.Rate, 'Q32016_AvgRent' = pastyear$Overall.Rent,
                   'Changepastyear_Occ'= ((current$Overall.Occ.Rate-pastyear$Overall.Occ.Rate)/pastyear$Overall.Occ.Rate),
                   'Changepastyear_AvgRent' = ((current$Overall.Rent-pastyear$Overall.Rent)/pastyear$Overall.Rent),
                   'Q22017_Occ'=pastquarter$Overall.Occ.Rate, 'Q22017_AvgRent'= pastquarter$Overall.Rent,
                   'Changepastquarter_Occ' = ((current$Overall.Occ.Rate-pastquarter$Overall.Occ.Rate)/pastquarter$Overall.Occ.Rate),
                   'Changepastquarter_AvgRent'= ((current$Overall.Rent- pastquarter$Overall.Rent)/pastquarter$Overall.Rent)
                   )

totalunits<- data.frame('Q32017' = sum(current$Total.Units), 'Q32016'=sum(pastyear$Total.Units), 'Q22017'=sum(pastquarter$Total.Units))

write.csv(newdf, 'Comp_Q32017_Q32016_Q22017.csv', row.names = FALSE)
write.csv(totalunits, "TotalUnitsComparison.csv", row.names=FALSE)


