##  Max Marno
# North Line GIS LLC.
#  9/14/2017
# 
#  Clean and if necessary add XY coordinates to the 
# NOTE THAT PART OF THE SCRIPT IS COMMENTED OUT DUE TO RESTRICTIONS ON NUMBER OF QUERIES TO GOOGLE API (GEOCODE)
  #   CHECK APTREF COLNAMES FOR CONSISTENCY IN LINES 89-91
rm(list = ls())
require(readxl)
require(sp)
require(stringi)
require(ggmap)
require(dplyr)
require(arcgisbinding)
arc.check_product()
register_google(key = "AIzaSyDPFQb3Tx854Nn48Gsuc5RKssjSP_janF4")

####   INPUT    ####
workspace <- "D:/Projects/GNAA/Data/3rdQtr2017Data"
setwd(workspace)
quarterdate<-"2017-09-30"
# Feature Class path with verified locations:
AptsLocVer<- "D:\\Projects\\GNAA\\AGProGNAA\\AGProGNAA.gdb\\AptCommsLocVer"
# Input exported apartment property list (from Access)
AptPropList <- "export/ApartmentPropertyList.xlsx"
Apartments.df<- read_excel(AptPropList)
# FUNCTIONS:
#############################################################################################
# 'NOT' IN FUNCTION/OPERATOR
'%!in%' <- function(x,y)!('%in%'(x,y))
# FUNCTION TO WRITE DATAFRAME TO ESRI FEATURE CLASS
toFC <- function(indata, XLON, YLAT, outname){
  pj4 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  spdf<- SpatialPointsDataFrame(coords = indata[,c(XLON, YLAT)], proj4string = pj4, data = indata)
  outpath <- paste0("D:\\Projects\\GNAA\\AGProGNAA\\AGProGNAA.gdb\\", outname)
  arc.write(path = outpath, data = spdf)
}
# ADD COORDINATES TO REFERENCE DATA IF NEEDED
addgeo<- function(inputdf){
  for (i in 1:nrow(inputdf)){
    #Sys.sleep(2)
    row.i<- inputdf[i,]
    if (is.na(row.i$POINT_X) & is.na(row.i$POINT_Y)){
      geostring<- paste(row.i$Address, row.i$CityAddress, "Tennessee", row.i$PostalCode, sep = ", ")
      geostring<- gsub(", NA", "", geostring)
      geocodeQueryCheck()
      #print(geostring)
      #Sys.sleep(1)
      geocoords<- geocode(geostring, source = 'google', override_limit = TRUE)
      #Sys.sleep(1)
      inputdf$POINT_X[i]<- geocoords$lon
      inputdf$POINT_Y[i]<- geocoords$lat
      print(paste0(as.character(i), "/", as.character(nrow(inputdf))," -- ",inputdf$NAME[i]))
      print(geostring)
      
    }
  }
  return(inputdf)
}
# EXTRACT PREVIOUS NAME FROM NAME FIELD
prevname<- function(indf){
  for (i in 1:nrow(indf)){
    rowi<- indf[i,]
    # SMOOTH BRACKET INDICATES PREVIOUS NAME INCLUDED IN NAME STRING
    if (grepl("\\(", rowi$NAME)){
      splitname<- unlist(strsplit(rowi$NAME, split = "\\("))
      keepname<- splitname[1]
      previousname<- unlist(splitname[2])
      indf$NAME[i]<- unlist(keepname)
      indf$previous[i]<- unlist(strsplit(previousname, split = "\\)")[1])
    }else{
      indf$previous[i]<- "NA"
    }
    # REMOVE SPACE FROM END OF NAME STRING
    if (stri_sub(indf$NAME[i], -1, -1)== " "){
      indf$NAME[i]<- unlist(stri_sub(indf$NAME[i], 1, -2))
    }
    # REMOVE 'The' FROM NAME STRING
    if (grepl(", The", indf$NAME[i])==TRUE){
      indf$NAME[i]<- unlist(paste("The", indf$NAME[i]))
      indf$NAME[i]<- unlist(gsub(", The", "", indf$NAME[i]))
    }
  }
  return(indf)
}
# PROCESSING:
#############################################################################################
fc<- arc.open(AptsLocVer)
AptRef.df<- arc.select(fc, names(fc@fields))
AptRef.df<- as.data.frame(AptRef.df)
colnames(AptRef.df)<- gsub("USER_", "", colnames(AptRef.df))
# GET NEW RECORDS FROM NEW APARTMENTCOMMUNITIES TABLE
newrows <- dplyr::filter(Apartments.df, Apartments.df$ID %!in% AptRef.df$ID)
newrows<- dplyr::select(newrows, ID, NAME, Address, CityAddress, State, PostalCode)
# BIND TO REFERENCE DF
AptRef.df<-dplyr::bind_rows(AptRef.df, newrows)
# REPLACE NA VALUES WITH 'NO'
AptRef.df$LocationVerified[is.na(AptRef.df$LocationVerified)]<- 'No'
# RUN ADDGEO FUNCTION TO ADD X, Y COORDINATES IF MISSING (NEW)
AptRef.df<- addgeo(AptRef.df)
# WRITE TO FEATURECLASS IN AGPRO GDB
toFC(AptRef.df, "POINT_X", "POINT_Y", "RefAprts")

# clean the Q1 2017 Apartments.df Property List 
# previous name, ", The" substring fixed, space at end of NAME string, 
# length<=30 character only name field (no spaces or special)
tester<- prevname(Apartments.df)
tester<-as.data.frame(tester)
# REMOVE SPECIAL CHARACTERS FROM NAME STRING
tester$strname<- gsub("\\.|\\_|\\-|\"|\'|&|$|*|,|\\s|!", "", tester$NAME)
tester$strname<- gsub("@", "at", tester$strname)
tester$strname<- gsub("/", "_", tester$strname)
tester$Notes<- sapply(tester$Notes, function(x) substr(x, 1, 254))
# PARE T0 30 CHARACTERS
tester$strname<- stri_sub(tester$strname, 1, 30)
# ADD XY, SUBMARKETS, AND LOCATIONVERIFIED COLUMNS AND VALUES FROM REFERENCE DATASET
GeoApts<- tester
GeoApts$POINT_Y<- unlist(sapply(GeoApts$ID, function(x) AptRef.df[which(AptRef.df$ID==x),]$POINT_Y))
GeoApts$POINT_X<- unlist(sapply(GeoApts$ID, function(x) AptRef.df[which(AptRef.df$ID==x),]$POINT_X))
GeoApts$SUBMARKET<- unlist(sapply(GeoApts$ID, function(x) AptRef.df[which(AptRef.df$ID==x),]$SUBMARKET))
GeoApts$LocationVerified<- unlist(sapply(GeoApts$ID, function(x) AptRef.df[which(AptRef.df$ID==x),]$LocationVerified))

write.csv(GeoApts, paste0(gsub("-", "_",quarterdate), "_AptsClean.csv"), row.names = FALSE)
toFC(indata = GeoApts, XLON = "POINT_X", YLAT = "POINT_Y", outname = "Q42017Apartments")

notes<- dplyr::select(Apartments.df, c(ID, NAME, Notes))
# WRITE NOTES TO SEPARATE TABLE TO JOIN LATER - MAX LENGTH = 255 WHEN WRITING FROM R
write.csv(notes, paste0(gsub("-", "_",quarterdate), "_Notes.csv"), row.names = FALSE)


# pj4 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# spdf<- SpatialPointsDataFrame(coords = GeoApts[,c("LON", "LAT")], proj4string = pj4, data = dplyr::select(GeoApts, -Notes))
# 
# llat <- "POINT_Y"
# llon <- "POINT_X"
# spdf<- SpatialPointsDataFrame(coords = bigtest[,c(llon, llat)], proj4string = pj4, data = bigtest)
# arc.write(path = "D:\\Projects\\GNAA\\AGProGNAA\\AGProGNAA.gdb\\bigtest2", data = spdf)
# 
# 

