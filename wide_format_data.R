#########################################################################################
# Last Date modified: 08/28/2019
# Author: Katy Torres
# Description: Current Treatments

##########################################################################################

dataset <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_1_2018_data/scored_data_from_eScreening_modules/Current_treatments_Response.csv',header=T,na.strings=c("#N/A",NA))




l.sort <- dataset[order(dataset$vista_lastname),]

demo_wide <- reshape(l.sort,
                     timevar = "visit_number",
                     idvar = c( "vista_lastname"),
                     direction = "wide")
names(demo_wide)


write.csv(demo_wide, "currentTX_longitudinal.csv")