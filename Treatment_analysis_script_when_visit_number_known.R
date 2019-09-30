#########################################################################################
# Last Date modified: 3/07/2018
# Author: Katy Torres
# Description: Merge corefile into most up to date full dataset
##########################################################################################

#load required libraries
library(plyr)
library(reshape2)

#Replace this path with location where data is currently stored
setwd('C:/Users/Psychiatry Lab/Documents/Biobank/data')

#READ ALL DATA
#change files each time this is ran!!!
#change files each time this is ran!!!
#change files each time this is ran!!!

#Also, go in corefile and change visits variable to "visit_number" before merge
#Also, ADD recruitment_site to CPRS sheet
#------------------------------------------------------------------------
dataset <- read.csv('joined_data_export_20180222.csv',header=T,na.strings=c(NA,999))
core <- read.csv('biobank_data_corefile_3-01-2018_CPRS.csv',header=T,na.strings=c(NA,NA))
completed <- read.csv('biobank_data_corefile_3_01_2018_study_completion.csv',header=T,na.strings=c(NA,NA))


data_pcl <- read.csv('C:/Users/Psychiatry Lab/Documents/Biobank/20_21_PCL_5/pcl5_current_reduced_data_export_20180222.csv',header=T,na.strings=c(NA,"#N/A"))
data_gad <- read.csv('C:/Users/Psychiatry Lab/Documents/Biobank/14_GAD7/gad7_reduced_data_export_20180222.csv',header=T,na.strings=c(NA,"#N/A"))
data_phq<- read.csv('C:/Users/Psychiatry Lab/Documents/Biobank/23_PHQ9/phq9_reduced_data_export_20180222.csv',header=T,na.strings=c(NA,"#N/A"))


#MERGE
#------------------------------------------------------------------------
#merge CPRS corefile and full dataset by assesstment id % LAST NAME
dat00 <- merge(dataset,completed, by=c("assessment_id"), all = TRUE)
dat0 <- merge(dat00,core, by=c("assessment_id", "vista_lastname"), all = TRUE)


#need to MERGE SCORED variables for GAD, PHQ9, PCL9
dat_with_phq9 <- merge(dat0,data_phq, by=c("assessment_id", "vista_lastname"), all = TRUE)
dat_with_gad7 <- merge(dat_with_phq9, data_gad, by=c("assessment_id", "vista_lastname"), all = TRUE)
dat_with_pcl5 <- merge(dat_with_gad7, data_pcl, by=c("assessment_id", "vista_lastname"), all = TRUE)

#remove data frams that will no longer be used
rm("dat_with_gad7","dat_with_phq9","dat0", "data_gad","data_pcl", "data_phq")

#SUBSET
#------------------------------------------------------------------------
#Only retain relevant variables
data_report<- subset(dat_with_pcl5, 
                 select= c(assessment_id,vista_lastname, visit_number, date_created, recruitment_site,
                           
                           pcl_5_dsm,	
                           pcl_total,
                           
                           gad7_total,
                           score_interpretation_gad7,	
                           gad7_greater_than_cut_off,
                           
                           phq9_total,	
                           score_interpretation_phq9,
                           phq9_treatment,
                           
                           CPRS_Diagnosis,	
                           Treatment
))

#PERFORM CALCULATIONS AND DATA TRANSFORMATION
#------------------------------------------------------------------------
#program change in variable for each measure
#Do this for each vista_lastname


#Calculate change since last visit for PCL
#------------------------------------------
newdf_pcl_by_visit<- dcast(data_report[,1:7], vista_lastname ~ visit_number)
#rename columns as v1_pcl, v2_pcl, v3_pcl
names(newdf_pcl_by_visit) <- c("vista_lastname", "v1_pcl", "v2_pcl", "v3_pcl")
visit_changepcl <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  if((!(is.na(v2_pcl))) & (!(is.na(v1_pcl))))
  {
    v2_change_pcl <- v2_pcl - v1_pcl
  }else {v2_change_pcl <- NA}
  
  if((!(is.na(v2_pcl))) & (!(is.na(v3_pcl))))
  {
    v3_change_pcl <- v3_pcl - v2_pcl
  }else {v3_change_pcl <- NA}

  scores <- data.frame(v2_change_pcl, v3_change_pcl)
  return(scores)
}


#Calculate summary scores in pcl data 
scored_pcl_change <- adply(newdf_pcl_by_visit, 1,visit_changepcl)


#Calculate change since last visit for GAD
#------------------------------------------
newdf_gad_by_visit<- dcast(data_report[,1:8], vista_lastname ~ visit_number)
#rename columns as v1_gad, v2_gad, v3_gad
names(newdf_gad_by_visit) <- c("vista_lastname", "v1_gad", "v2_gad", "v3_gad")

visit_changegad <- function(x)
{

  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  if((!(is.na(v2_gad))) & (!(is.na(v1_gad))))
  {
    v2_change_gad <- v2_gad - v1_gad
  }else {v2_change_gad <- NA}
  
  if((!(is.na(v2_gad))) & (!(is.na(v3_gad))))
  {
    v3_change_gad <- v3_gad - v2_gad
  }else {v3_change_gad <- NA}

  scores <- data.frame(v2_change_gad, v3_change_gad)
  
  return(scores)
}

#Calculate summary scores in gad data 
scored_gad_change <- adply(newdf_gad_by_visit, 1,visit_changegad)

#Calculate change since last visit for phq
#------------------------------------------
newdf_phq_by_visit<- dcast(data_report[,1:11], vista_lastname ~ visit_number)
#rename columns as v1_phq, v2_pcl, v3_pcl
names(newdf_phq_by_visit) <- c("vista_lastname", "v1_phq", "v2_phq", "v3_phq")

visit_changephq <- function(x)
{

  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  if((!(is.na(v2_phq))) & (!(is.na(v1_phq))))
  {
    v2_change_phq <- v2_phq - v1_phq
  }else {v2_change_phq <- NA}
  
  if((!(is.na(v2_phq))) & (!(is.na(v3_phq))))
  {
    v3_change_phq <- v3_phq - v2_phq
  }else {v3_change_phq <- NA}
  
  scores <- data.frame(v2_change_phq, v3_change_phq)
  
  return(scores)
}

#Calculate summary scores in phq data 
scored_phq_change <- adply(newdf_phq_by_visit, 1,visit_changephq)

#merge these files together
#--------------------------
#need to MERGE SCORED variables for GAD, PHQ9, PCL9
dat1and2 <- merge(scored_pcl_change,scored_gad_change, by=("vista_lastname"), all = TRUE)
by_visits <- merge(dat1and2, scored_phq_change, by=("vista_lastname"), all = TRUE)

#Only retain relevant variables
calculated_changes<- subset(by_visits, 
                     select= c(vista_lastname,
                               v2_change_pcl, v3_change_pcl, 
                               v2_change_gad,  v3_change_gad, 
                               v2_change_phq,  v3_change_phq 
                     ))

#remove data frams that will no longer be used
rm ("dat_with_pcl5", "dat1and2","scored_pcl_change", "scored_gad_change", "scored_phq_change", "newdf_gad_by_visit", "newdf_pcl_by_visit","newdf_phq_by_visit")


#merge new variables for changes between visits with original report
#--------------------------
#need to MERGE SCORED variables for GAD, PHQ9, PCL9
data_report_with_changes <- merge(data_report,calculated_changes, by=("vista_lastname"), all = TRUE)

#reorder columns for report
data_report_with_changes[c("vista_lastname","assessment_id","visit_number","date_created","recruitment_site",
       "pcl_5_dsm","pcl_total", "v2_change_pcl", "v3_change_pcl",
       "gad7_total", "v2_change_gad","v3_change_gad","score_interpretation_gad7", "gad7_greater_than_cut_off", 
       "phq9_total", "v2_change_phq","v3_change_phq", "score_interpretation_phq9", "phq9_treatment","CPRS_Diagnosis","Treatment"
       )]

#OUTPUT
#------------------------------------------------------------------------
#Export data
write.csv(data_report_with_changes, "~/Biobank/0_followup_analysis/Biobank_analysis_gad7_phq9_pcl5_20180213.csv",quote=T,row.names=F,na="#N/A")

#Export data
write.csv(by_visits, "~/Biobank/0_followup_analysis/Biobank_analysis_gad7_phq9_pcl5_changesinscore_20180213.csv",quote=T,row.names=F,na="#N/A")


#Export data
write.csv(data_report, "~/Biobank/0_followup_analysis/20180213.csv",quote=T,row.names=F,na="#N/A")


