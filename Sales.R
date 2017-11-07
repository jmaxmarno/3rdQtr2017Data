
# 
# SALES TABLE

rm(list=ls())

require(readxl)
require(stringi)
require(plyr)
require(dplyr)

projws= "D:/Projects/GNAA/Data/3rdQtr2017Data"
setwd(projws)
collectiondate<-"2017-09-30"

# Read Data Tables
# Read Apartments feature class with geocoded addresses from ArcGIS Pro project
# APARTMENTS PROPERTY LIST, WITH GEOCODED ADDRESSES AND COORDS FROM PRO (ARCGISBRIDGE)
aptfc.df<- read.csv("2017_09_30_AptsClean.csv", stringsAsFactors = FALSE)
keeps<-c("ID", "NAME", "previous", "Yb", "SUBMARKET", "NUMBER")
AptData.df<-aptfc.df[keeps]
AptData.df$NUMBER<- as.integer(AptData.df$NUMBER)

# READ SALES DATA FROM OWNERSHIP TABLE
Ownership.df<- read_excel("export/Ownership.xlsx")
colnames(Ownership.df)<-gsub(" ", "", colnames(Ownership.df))
#Ownership.df$ClosingDate<- as.Date( as.character(Ownership.df$ClosingDate), "%y-%m-%d")
Ownership.df<- Ownership.df[which(!is.na(Ownership.df$ClosingDate)),]
Ownership.df<- Ownership.df[which(format.POSIXct(Ownership.df$ClosingDate, "%y")=="17"),]
Ownership.df<- merge(Ownership.df, AptData.df, by.x = "ID", by.y = "ID",  all.x = TRUE)

# UNIT MIX AND RENTS
UnitMixRents<- read_excel("export/UnitMixRents.xlsx")
colnames(UnitMixRents)<- gsub(" ", "", colnames(UnitMixRents))
UnitMixRents<- UnitMixRents[which(as.character(UnitMixRents$DATE)==collectiondate),]
pUnitMix.df<- as.data.frame(UnitMixRents)

# REMOVE UNNECESSARY FIELDS
pUnitMix.df<- merge(Ownership.df, pUnitMix.df, by.x = "ID", by.y = "ID",  all.x = TRUE)
# ADD FIELDS TO WEIGHT UNIT RENT AND SIZE BY NUMBER OF UNITS
pUnitMix.df$UnitsXSize<- pUnitMix.df$NumberUnits*pUnitMix.df$UnitSize
ByID.df<- ddply(pUnitMix.df, c("ID"), summarize,
                Num.Units= sum(NumberUnits, na.rm = TRUE),
                DATE= toString(DATE[1]),
                UnitsXSize=sum(UnitsXSize, na.rm = TRUE),
                NUMBER=max(NUMBER),
                Submarket=max(SUBMARKET)
)

Ownership.df<- merge(Ownership.df, ByID.df, by.x = "ID", by.y = "ID",  all.x = TRUE)




Sales.df<- data.frame("Property"=Ownership.df$NAME,
                      "Buyer"=Ownership.df$Grantee,
                      "Closing Date"=Ownership.df$ClosingDate,
                      "Submarket"=Ownership.df$SUBMARKET,
                      "Year Built"=Ownership.df$Yb,
                      "Number of Units"= Ownership.df$NUMBER.x,
                      "Sales Price"=Ownership.df$SalesPrice,
                      "Price per Unit"= round(Ownership.df$SalesPrice/Ownership.df$NUMBER.x,digits = 0),
                      "Price per SqFt" = round(Ownership.df$SalesPrice/Ownership.df$UnitsXSize, digits = 2)
)
Sales.df$Price.per.SqFt[Sales.df$Price.per.SqFt==Inf]<- NA

colnames(Sales.df)<- gsub("\\.", " ", colnames(Sales.df))
write.csv(Sales.df, "SalesTable.csv", row.names = FALSE)

