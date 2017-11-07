# Max Marno
# North Line GIS LLC
# 11/1/2017

# Add 'New Entry' rows for related tables - AGO

require(readxl)
require(sp)
require(stringi)
require(ggmap)
require(dplyr)
require(arcgisbinding)
arc.check_product()

workspace <- "D:/Projects/GNAA/Data/3rdQtr2017Data"
setwd(workspace)

Ownership <- "export/Ownership.xlsx"
ownership.df<- read_excel(Ownership)

datacols<- dplyr::select(ownership.df, -ID)
datacols$`Closing Date`<- NA
datacols$`Sales Price`<- NA
datacols$Grantee<- "NEW ENTRY"
datacols$Grantor<- "NEW ENTRY"
IDcol<- unique(ownership.df$ID)
datacols<-datacols[1:length(IDcol),]
newrows<- dplyr::bind_cols('ID' = IDcol, datacols)

output<- dplyr::bind_rows(ownership.df, newrows)
output<- dplyr::arrange(output, ID)

write.csv(output, "PermanentOwnership.csv", row.names = FALSE)





