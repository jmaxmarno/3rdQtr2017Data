#
#
# SPECULATIVE CONSTRUCTION 
# 
rm(list=ls())

require(readxl)
require(stringi)
require(plyr)
require(dplyr)

projws= "D:/Projects/GNAA/Data/3rdQtr2017Data"
setwd(projws)

#######
# CONSTRUCTION STATUS INPUT
cstatus = "Underway"
######

# Read Data Tables
# Read Apartments feature class with geocoded addresses from ArcGIS Pro project
# APARTMENTS PROPERTY LIST, WITH GEOCODED ADDRESSES AND COORDS FROM PRO (ARCGISBRIDGE)
aptfc.df<- read.csv("2017_09_30_AptsClean.csv", stringsAsFactors = FALSE)

SpecCon<- data.frame(aptfc.df[which(aptfc.df$Status==cstatus),])
Construction.df<- SpecCon[,c("NAME", "Developer", "SUBMARKET", "NUMBER", "Financing", "Status", "ConstrStatus", "Location")]
for (i in 1:nrow(Construction.df)){
  rowi<- Construction.df[i,]
  Construction.df$StartDate[i]<- strsplit(Construction.df[i,]$Location, " / |/ | /")[[1]][1]
  Construction.df$Location[i]<- strsplit(Construction.df$Location[i], " / |/ | /")[[1]][2]
  
}
#Construction.df$Location<- unlist(Construction.df$Location)[2]
keeps<-c("NAME", "Developer", "SUBMARKET", "NUMBER", "Financing", "ConstrStatus", "StartDate", "Location")
Construction.df<-Construction.df[keeps]
colnames(Construction.df)<- c("Property", "Developer", "Submarket", "# Units", "Financing", "Status", "Start Date", "Location")
Construction.df$Financing<- gsub("Subsidized", "Affordable", Construction.df$Financing)
head(Construction.df)


write.csv(Construction.df, paste(cstatus, "_Construction.csv", sep = ""), row.names = FALSE )

