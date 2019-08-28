#########################################################################################
# Last Date modified: 08/23/2019
# Author: Katy Torres
# Description: Count number of specimens per subject
##########################################################################################
# library(tidyverse)
library(plyr)
library("bindrcpp", lib.loc="~/R/win-library/3.5")

setwd('C:/Users/Nievergelt Lab/Documents/Biobank/000_sample_tracking')

#go thought every subject and count number of times it sees a specimen in the column :specimen"
# "LiHep"
# "plasma"
# "plasma-aprotinin"
# "saliva"
# "urine"
# "whole blood"

samples <- read.csv('specimens_renamed_20190822.csv',header=T,na.strings=c("#N/A",NA))
dim(samples)
names(samples)
str(samples)

count(samples, c("ID_visit", "specimen"))

specimenbyid<- count(samples, c("ID_visit", "specimen"))

write.csv(specimenbyid, "specimenbyID.csv")
