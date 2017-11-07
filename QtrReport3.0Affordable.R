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
Financing<- "Affordable"

aptfc.df<- read.csv("2017_09_30_AptsClean.csv", stringsAsFactors = FALSE)
# APPLY CONSTRAITS HERE#####################################################################################################
aptfc.df<- aptfc.df[which(aptfc.df$GNAAnotcollect==FALSE&
                            aptfc.df$Status =="Complete"&
                            aptfc.df$Financing!='Conventional'),]

aptids<- aptfc.df$ID
#############################################################################################################################
# WE REALLY JUST NEED A PIVOT TABLE TO RELATE THE APARTMENT ID TO THE SUBMARKET AS WELL AS THE NUMBER OF UNITS 
# 'NUMBER_' HAS BEEN CHANGED TO 'NUMBER' IN ARCGISPRO
keeps<-c("ID", "SUBMARKET", "NUMBER")
AptsSubm<-aptfc.df[keeps]

# UNIT MIX AND RENTS
UnitMixRents<- read_excel("export/UnitMixRents.xlsx")
colnames(UnitMixRents)<- gsub(" ", "", colnames(UnitMixRents))
UnitMixRents<- UnitMixRents[which(as.character(UnitMixRents$DATE)==collectiondate),]
# OCCUPANCY MANAGEMENT
occupancymanagement<- read_excel("export/OccupancyManagement.xlsx")
colnames(occupancymanagement)<- gsub(" ", "", colnames(occupancymanagement))
OccMan<- occupancymanagement[which(as.character(occupancymanagement$OccupancyDate)==collectiondate),]

# MERGE OCCUPANCY WITH APARTMENT DATA
AptsSubm<- merge(AptsSubm, OccMan,by = "ID",  all.x = TRUE )
#############################################################################

# Table 1 - Summary of Conventional Statistics by Submarket
# include num Units, Occ Rate, Rent PSF, Size, Rent
# READ PERMANENT UNIT MIX, GROUPED ON APT-ID, NUMBER OF BEDROOMS, AND NUMBER OF BATHS

qunitmix.df<- as.data.frame(UnitMixRents)
# REMOVE UNNECESSARY FIELDS
qunitmix.df<- qunitmix.df[,c("ID", "DATE", "NumberBedrooms", "NumberBaths", "UnitStyle", "NumberUnits", "UnitSize", "Rent")]
# ADD FIELDS TO WEIGHT UNIT RENT AND SIZE BY NUMBER OF UNITS
qunitmix.df$UnitsXRent<- qunitmix.df$NumberUnits*qunitmix.df$Rent
qunitmix.df$UnitsXSize<- qunitmix.df$NumberUnits*qunitmix.df$UnitSize

qunitmix.df<- merge(AptsSubm, qunitmix.df, by="ID", all.x = TRUE)
qunitmix.df<- filter(qunitmix.df, !is.na(NumberUnoccupied))
# old join used # qunitmix.df<- join(qunitmix.df, AptsSubm, by="ID", match='all')

# NOW GROUP ON ONLY APT-ID AND NUMBER OF BEDROOMS
ByBdrm.df<- ddply(qunitmix.df, c("ID", "NumberBedrooms"), summarize,
                  Num.Units = sum(NumberUnits, na.rm = TRUE),
                  DATE= toString(DATE[1]),
                  UnitsXSize=sum(UnitsXSize, na.rm = TRUE),
                  UnitsXRent=sum(UnitsXRent, na.rm = TRUE),
                  NumUnocc=max(NumberUnoccupied),
                  NUMBER=max(NUMBER),
                  SUBMARKET=max(SUBMARKET)
)

ByID.df<- ddply(qunitmix.df, c("ID"), summarize,
                Num.Units= sum(NumberUnits, na.rm = TRUE),
                DATE= toString(DATE[1]),
                UnitsXSize=sum(UnitsXSize, na.rm = TRUE),
                UnitsXRent=sum(UnitsXRent, na.rm = TRUE),
                NumUnocc=max(NumberUnoccupied),
                NUMBER=max(NUMBER),
                SUBMARKET=max(SUBMARKET)
)

BySUB.df<- ddply(ByID.df, c("SUBMARKET"), summarize,
                 Num.Units = sum(Num.Units, na.rm = TRUE),
                 DATE= toString(DATE[1]),
                 UnitsXSize=sum(UnitsXSize, na.rm = TRUE),
                 UnitsXRent=sum(UnitsXRent, na.rm = TRUE),
                 NumUnocc=sum(NumUnocc, na.rm = TRUE),
                 NUMBER=sum(NUMBER, na.rm = TRUE),
                 SUBMARKET=max(SUBMARKET)
)


# GET SUBMARKET SUMMARY FOR EACH APARTMENT COMMUNITY and number of units
# qUM_bdrm.df<- merge(qUM_bdrm.df, AptsSubm, all.x = TRUE)

SubBed.df<- ddply(ByBdrm.df, c("SUBMARKET", "NumberBedrooms"), summarize,
                  Num.Units = sum(Num.Units, na.rm = TRUE),
                  DATE= toString(DATE[1]),
                  UnitsXSize=sum(UnitsXSize, na.rm = TRUE),
                  UnitsXRent=sum(UnitsXRent, na.rm = TRUE),
                  NumUnocc=sum(NumUnocc, na.rm = TRUE)
)
# qUM_Submarket<- join(qUM_Submarket, BySUB, by="Submarket", match='all')

## apply calculations
SubBed.df$AvgRent<- SubBed.df$UnitsXRent/SubBed.df$Num.Units
SubBed.df$AvgSize<- SubBed.df$UnitsXSize/SubBed.df$Num.Units
SubBed.df$OccRate<- 100-((SubBed.df$NumUnocc/SubBed.df$Num.Units)*100)
SubBed.df$Rent_PSF<- SubBed.df$AvgRent/SubBed.df$AvgSize


# Summary Statistics by Submarket
SubmarketStatistics<- SubBed.df

sSubmarkets<- unique(SubmarketStatistics$SUBMARKET)
sSubmarkets<- sSubmarkets[!is.na(sSubmarkets)]
SubSumStat<- function(ssubm){
  newdf<- data.frame("SUBMARKET"=ssubm, 
                     "Total.Units"= iflen(sum(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm),]$Num.Units)),
                     "Overall.Occ.Rate" = iflen(round(100-(BySUB.df[which(BySUB.df$SUBMARKET==ssubm),]$NumUnocc/BySUB.df[which(BySUB.df$SUBMARKET==ssubm),]$NUMBER)*100, digits = 2)),
                     "Overall.Rent.PSF" = iflen(round((sum(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm),]$UnitsXRent)/
                                                         sum(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm),]$Num.Units))/
                                                        # Overal Rent above, Overall Size SqFt below
                                                        (sum(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm),]$UnitsXSize)/
                                                           sum(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm),]$Num.Units)
                                                        ), digits = 2)),
                     "Overall.SqFt" = iflen(round(sum(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm),]$UnitsXSize)/
                                                    sum(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm),]$Num.Units), digits = 0)),
                     "Overall.Rent" = iflen(round(sum(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm),]$UnitsXRent)/
                                                    sum(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm),]$Num.Units), digits = 0)),
                     "0bd.Rent.PSF" = iflen(round(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm & SubmarketStatistics$NumberBedrooms== 0),]$Rent_PSF, digits = 2)),
                     "0bd.SqFt" = iflen(round(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm & SubmarketStatistics$NumberBedrooms== 0),]$AvgSize, digits = 0)),
                     "0bd.Rent" = iflen(round(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm & SubmarketStatistics$NumberBedrooms== 0),]$AvgRent, digits = 0)),
                     "1bd.Rent.PSF" = iflen(round(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm & SubmarketStatistics$NumberBedrooms== 1),]$Rent_PSF, digits = 2)),
                     "1bd.SqFt" = iflen(round(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm & SubmarketStatistics$NumberBedrooms== 1),]$AvgSize, digits = 0)),
                     "1bd.Rent" = iflen(round(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm & SubmarketStatistics$NumberBedrooms== 1),]$AvgRent, digits = 0)),
                     "2bd.Rent.PSF" = iflen(round(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm & SubmarketStatistics$NumberBedrooms== 2),]$Rent_PSF, digits = 2)),
                     "2bd.SqFt" = iflen(round(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm & SubmarketStatistics$NumberBedrooms== 2),]$AvgSize, digits = 0)),
                     "2bd.Rent" = iflen(round(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm & SubmarketStatistics$NumberBedrooms== 2),]$AvgRent, digits = 0)),
                     "3bd.Rent.PSF" = iflen(round(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm & SubmarketStatistics$NumberBedrooms== 3),]$Rent_PSF, digits = 2)),
                     "3bd.SqFt" = iflen(round(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm & SubmarketStatistics$NumberBedrooms== 3),]$AvgSize, digits = 0)),
                     "3bd.Rent" = iflen(round(SubmarketStatistics[which(SubmarketStatistics$SUBMARKET==ssubm & SubmarketStatistics$NumberBedrooms== 3),]$AvgRent, digits = 0))
  )
  return(newdf[!duplicated(newdf),])
}


ll<-lapply(sSubmarkets, function(x) SubSumStat(x))
rj<- do.call(rbind, ll)
colnames(rj)<- gsub("X" ,"", colnames(rj))

df<-ByBdrm.df
OverallStats<- data.frame("Total Units"= sum(df$Num.Units, na.rm = TRUE),
                          "Overall Occupancy"= round(100-(sum(df[which(!is.na(df$NUMBER)&!is.na(df$NumUnocc)),]$NumUnocc, na.rm = TRUE)/
                                                            sum(df[which(!is.na(df$NUMBER)&!is.na(df$NumUnocc)),]$NUMBER, na.rm = TRUE)*100), digits = 2),
                          "Overall Average Rent"= round(sum(df$UnitsXRent)/sum(df$Num.Units), digits = 0),
                          "Overall Average Size" = round(sum(df$UnitsXSize)/sum(df$Num.Units), digits = 0),
                          "Overall Average Rent PSF" = round((sum(df$UnitsXRent)/sum(df$Num.Units))/(sum(df$UnitsXSize)/sum(df$Num.Units)), digits = 2)
)
colnames(rj)<- gsub("\\.", " ", colnames(rj))
colnames(OverallStats)<- gsub("\\.", " ", colnames(OverallStats))
write.csv(rj, paste(collectiondate, Financing, "SubmarketSummaryStatistics.csv", sep = ""),row.names = FALSE)
write.csv(OverallStats, paste(collectiondate, Financing, "OverallStats.csv", sep = ""), row.names = FALSE)
OverallStats

