#########################################################################################
# Last Date modified: 08/22/2019
# Author: Katy Torres
# Description: Merge all sample tracking maps
##########################################################################################
setwd("C:/Users/Nievergelt Lab/Documents/Biobank/000_sample_tracking/maps")

# #importing every csv file in folder
# temp = list.files(pattern="*.csv")
# for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))


#merging all files together into one dataset
file_list <- list.files(pattern="*.csv")

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE)
    dataset<-rbind.fill(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

write.csv(dataset, "specimens.csv")