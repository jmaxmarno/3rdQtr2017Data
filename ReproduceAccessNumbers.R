#
#
#
# THIS IS VERSION 3

# Generate Quarterly Statistics Summary/ Market Survey Report from exported Access tables (excel format)
# 
rm(list=ls())
require(readxl)
require(stringi)
require(plyr)
require(dplyr)
require(ggmap)
require(arcgisbinding)
# FUNCTION RETURNS NA IF ARG is of length 0, ie does not exist

iflen<- function(xx){
  if (length(xx)!=0){
    return(xx)
  }else{
    return(NA)
  }
}

ifn<- function(x){
  if (x=='' | !is.na(x) | !is.null(x)){
    return(x)
  }else{
    return(NA)
  }
}

projws= "D:/Projects/GNAA/Data/3rdQtr2017Data"
setwd(projws)
# CHANGE THIS FOR QUARTERLY, OR EVEN YEARLY 
collectiondate<-"2017-09-30"
Financing<- "Conventional"

Apartments<- read.csv("2017_09_30_AptsClean.csv", stringsAsFactors = FALSE)
colnames(Apartments)<- gsub(" ", "", colnames(Apartments))
# UNIT MIX AND RENTS
UnitMixRents<- read_excel("export/UnitMixRents.xlsx")
colnames(UnitMixRents)<- gsub(" ", "", colnames(UnitMixRents))
UnitMixRents<- UnitMixRents[which(as.character(UnitMixRents$DATE)==collectiondate),]
# OCCUPANCY MANAGEMENT
occupancymanagement<- read_excel("export/OccupancyManagement.xlsx")
colnames(occupancymanagement)<- gsub(" ", "", colnames(occupancymanagement))
OccMan<- occupancymanagement[which(as.character(occupancymanagement$OccupancyDate)=='2017-09-30'),]

# TEST
AptOcc.merge<- merge(Apartments, OccMan, by="ID", all.x = TRUE)






#   THESE LINES REPLICATE THE ACCESS TOTAL UNITS FIGURE
UM_AptMerge<- merge(AptOcc.merge, UnitMixRents, by="ID", all.x = TRUE)
# APPLY SAME CONSTRAINTS AS ACCESS DATABASE USING WHICH()
UMixApt<- UM_AptMerge[which(UM_AptMerge$Financing==Financing & !is.null(UM_AptMerge$NumberUnits)& !is.null(UM_AptMerge$Rent) & UM_AptMerge$Rent!=0),]
sum(UMixApt$NumberUnits) # 90763, SAME AS IN ACCESS REPORT
## THIS WORKS!!^^^


# USING THIS TO TEST FOR NA VALUES IN OTHER FIELDS
NAS<- UM_AptMerge[which(!is.null(UM_AptMerge$NumberUnits)& !is.null(UM_AptMerge$Rent) & UM_AptMerge$Rent!=0),]

# EXAMPLE FOR GNAA
sum(UMixApt[which(!is.na(UMixApt$NumberUnoccupied)),]$NumberUnits) # = 90525
#  1982 Luxe at Indian Lake Village,  2011     Standard at White House THESE TWO HAVE NA NUM UNOCCUPIED
# TEST WITH STATUS COMPLETE
sum(UMixApt[which(!is.na(UMixApt$NumberUnoccupied) & UMixApt$Status=='Complete'),]$NumberUnits)

notincluded<-UMixApt[which(is.na(UMixApt$NumberUnoccupied)),]
sum(UMixApt[which(!is.na(UMixApt$NumberUnoccupied) & !is.na(UMixApt$Rent) & !is.na(UMixApt$UnitSize) & UMixApt$Financing==financing & UMixApt$Status=='Complete'),]$NumberUnits)

getrecords<- UMixApt[which(is.na(UMixApt$NumberUnoccupied)),]




# COMPARE id'S
accessids<-unique(UMixApt$ID) # length is 419 versus 417 when excluding NA NumberUnoccupied 

"
> length(aptids)
[1] 417
> accessids<-unique(UMixApt$ID)
> length(aptids)
[1] 417
"
# These apartment ID's are in my caclulations in the Qtr Report (aptids) but not the Access (accessids): 270 1855
# for conventional
# 270 - GNAAresponse==0, NUMBER=144
# 1855 - Status is RONDO - NUMBER 254


#plyr::count(Apartments, 'Financing')
"
Financing freq
1 Conventional  561
2   Subsidized  113
3   Tax Credit    1
4         <NA>    1
"

tdf<- ddply(UMixApt, "ID", summarize, num=first(NumberUnits))
