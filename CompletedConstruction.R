


# COMPLETED CONSTRUCTION PROJECTS
#
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
cstatus = "Complete"
yearbuilt<-2017
######

# Read Data Tables
# Read Apartments feature class with geocoded addresses from ArcGIS Pro project
# APARTMENTS PROPERTY LIST, WITH GEOCODED ADDRESSES AND COORDS FROM PRO (ARCGISBRIDGE)
# arc.check_product()
# fc<- arc.open("D:\\Projects\\Tennessee\\AGProGNAA\\AGProGNAA.gdb\\c2017_03_31_Apts")
# aptfc.df<- arc.select(fc, names(fc@fields))
aptfc.df<- read.csv("2017_09_30_AptsClean.csv", stringsAsFactors = FALSE)

SpecCon<- data.frame(aptfc.df[which(aptfc.df$Yb==yearbuilt),])
Construction.df<- SpecCon[,c("NAME", "Developer", "SUBMARKET", "NUMBER", "Financing", "ConstrStatus", "Status", "Location")]
for (i in 1:nrow(Construction.df)){
  rowi<- Construction.df[i,]
  Construction.df$StartDate[i]<- strsplit(Construction.df$Location[i], " / |/ | /")[[1]][1]
  Construction.df$Location[i]<- strsplit(Construction.df$Location[i], " / |/ | /")[[1]][2]
}
#Construction.df$Location<- unlist(Construction.df$Location)[2]
keeps<-c("NAME", "Developer", "SUBMARKET", "NUMBER", "Financing", "Status", "StartDate", "Location")
Construction.df<-Construction.df[keeps]
colnames(Construction.df)<- c("Property", "Developer", "Submarket", "# Units", "Financing", "Status", "Complete", "Location")
head(Construction.df)


write.csv(Construction.df, paste(cstatus, "_Construction.csv", sep = ""), row.names = FALSE)

