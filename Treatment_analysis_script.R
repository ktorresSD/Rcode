#########################################################################################
# Last Date modified: 3/23/2018
# Author: Katy Torres
# Description: Merge corefile into most up to date full dataset
##########################################################################################

#load required libraries
library(plyr)
library(reshape2)

#Replace this path with location where data is currently stored
setwd('C:/Users/Psychiatry Lab/Documents/Biobank/data')


#------------------------------------------------------------------------
#READ IN ALL DATA FILES
#------------------------------------------------------------------------
#change file names each time this is run!!!
#Also, go in corefile and change visits variable to "visit_number" before merge, ADD recruitment_site and sompeltion status
dataset <- read.csv('joined_data_export_20180328.csv',header=T,na.strings=c(NA,999))
core <- read.csv('biobank_data_corefile_20180329.csv',header=T,na.strings=c(NA,"#N/A"))

data_tbi<- read.csv('C:/Users/Psychiatry Lab/Documents/Biobank/4_BTBIS/BTBIS_reduced_data_export_20180329.csv',header=T,na.strings=c(NA,"#N/A"))
data_pcl <- read.csv('C:/Users/Psychiatry Lab/Documents/Biobank/20_21_PCL_5/pcl5_current_reduced_data_export_20180329.csv',header=T,na.strings=c(NA,"#N/A"))
data_gad <- read.csv('C:/Users/Psychiatry Lab/Documents/Biobank/14_GAD7/gad7_reduced_data_export_20180329.csv',header=T,na.strings=c(NA,"#N/A"))
data_phq<- read.csv('C:/Users/Psychiatry Lab/Documents/Biobank/23_PHQ9/phq9_reduced_data_export_20180329.csv',header=T,na.strings=c(NA,"#N/A"))



#------------------------------------- -----------------------------------
#MERGE Files together to create master dataset
#------------------------------------------------------------------------
#merge CPRS corefile and full dataset by assesstment id % LAST NAME
#run if completed file sis available
dat0 <- merge(dataset,core, by=c("assessment_id", "vista_lastname"), all = TRUE)

#need to MERGE SCORED variables for GAD, PHQ9, PCL9
dat0_tbi <- merge(dat0, data_tbi, by=c("assessment_id", "vista_lastname"), all = TRUE)
dat_with_phq9 <- merge(dat0_tbi,data_phq, by=c("assessment_id", "vista_lastname"), all = TRUE)
dat_with_gad7 <- merge(dat_with_phq9, data_gad, by=c("assessment_id", "vista_lastname"), all = TRUE)
dat_with_pcl5 <- merge(dat_with_gad7, data_pcl, by=c("assessment_id", "vista_lastname"), all = TRUE)


#------------------------------------------------------------------------------
#DATA MANIPULATION
#------------------------------------------------------------------------------
#remove data frams that will no longer be used
rm("dat0_tbi", "dat_with_phq9", "dat_with_gad7", "data_gad" , "data_tbi","data_pcl", "data_phq")

#Make variable for age
dat_with_pcl5$year_assessed<- format(as.Date(dat_with_pcl5$date_created, format="%d/%m/%Y"),"%Y")
dat_with_pcl5$approx_age<- as.numeric(dat_with_pcl5$year_assessed) - dat_with_pcl5$demo_YOB_r

#Make gender varibale female and male instead of 1 and 2
dat_with_pcl5$demo_gender<- ifelse(dat_with_pcl5$demo_gender_r==2, "Female", "Male")

#Current_Psychotherapy
#Recode psychotherapy treatment variable to be 1 and 0 instead of 1 and 2
dat_with_pcl5$curr_psychor<- ifelse(dat_with_pcl5$CurrentTreatments6_Psycho==1, 1,0)

#if they answered "1" (yes) to recieving any specific treatment, then say "1" (yes) to a new columns called treated_psych
df_psych0<- c("CurrTx6.1_Group", "CurrTx6.1_Ind", "CurrTx6.1_Fam", "CurrTx6.1_Couples", 
              "CurrTx7a_CBTAnx",	"CurrTx7b_CBTDep",	"CurrTx7c_CBTInsom",	"CurrTx7d_IRTNM",	
              "CurrTx7e_CBTBP",	"CurrTx7f_CPT",	"CurrTx7g_Anger",	"CurrTx7h_ACT",	"CurrTx7i_Other",
              "CurrTx8a_CBTAnx",	"CurrTx8b_CBTDep",	"CurrTx8c_CBTInsom",	"CurrTx8d_IRTNM",	
              "CurrTx8e_CPT",	"CurrTx8f_PE",	"CurrTx8g_EMDR",	"CurrTx8h_ACT",	"CurrTx8g_Anger",	"CurrTx8i_Other"
              )

df_psych<- dat_with_pcl5[df_psych0]
treated_psych0<- apply(df_psych, 1, function(r) any(r %in% "1"))
treated_psych.na<- apply(df_psych, 1, function(r) all(r %in% NA))
treated_psych <- 0

# if true in treated_psych0 then treated_psych <- 1
# if true in treated_psych.na then  treated_psych <- NA
for(i in 1:length(treated_psych0)){
if (treated_psych0[i]==TRUE){treated_psych[i] <- 1}
if (treated_psych.na[i] ==TRUE){treated_psych[i] <- NA}
}
#add this column to the larger dataset
dat_with_pcl5 <- cbind(dat_with_pcl5, treated_psych)



#---------------------------------------------------
#CALL IN function that created columns for treatment
#---------------------------------------------------
source("~/Biobank/00_analyses/currently_recieving_treatment.R")
diagnoses_treated0 <- adply(dat_with_pcl5, 1, diag_treated0)

#Check if psychotherapy check worked
View(diagnoses_treated0[,c(1:2, 865:870)])



#any_treat, if they are currently recieving either medications or psychotherapy or said yest to any treatment question
y<-cbind(diagnoses_treated0$curr_treatment,diagnoses_treated0$treated_talk,diagnoses_treated0$treated_medication)
any_treat <- rowSums(y, na.rm= TRUE)
diagnoses_treated <- cbind(diagnoses_treated0,any_treat)

#Check if any_treat worked
View(diagnoses_treated[,c(1:2, 865:871)])

#------------------------------------------------------------------------
#SUBSET FULL DATASET to select variables desired for analysis and report
#------------------------------------------------------------------------
#Only retain relevant variables
data_report<- subset(diagnoses_treated, 
                 select= c(assessment_id,vista_lastname, visit_number, date_created, 
                           recruitment_site,
                           
                           approx_age,
                           demo_gender,
                           CPRS_Diagnosis,
                           
                           #pcl_5_dsm,	
                           pcl_total,
                           
                           phq9_total,	
                           #score_interpretation_phq9,
                           #phq9_treatment,
                           
                           gad7_total,
                           
                           tbi_tot_week_symotoms,

                           Treatment,
                           
                           curr_treatment,
                           treated_medication,
                           treated_talk,
                           any_treat,
                           case_control_at_enrollment,
                           Study_completed
))

#------------------------------------------------------------------------
#PERFORM CALCULATIONS AND DATA TRANSFORMATION
#------------------------------------------------------------------------
#program change in variable for each measure
#Do this for each vista_lastname

#Calculate change since last visit for PCL
newdf_pcl_by_visit<- dcast(data_report[,1:9], vista_lastname ~ visit_number)
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


#Calculate change since last visit for phq
newdf_phq_by_visit<- dcast(data_report[,1:10], vista_lastname ~ visit_number)
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

#Calculate change since last visit for GAD
newdf_gad_by_visit<- dcast(data_report[,1:11], vista_lastname ~ visit_number)
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

#TBI
newdf_tbi<- dcast(data_report[,1:12], vista_lastname ~ visit_number)
#rename columns as v1_phq, v2_pcl, v3_pcl
names(newdf_tbi) <- c("vista_lastname", "v1_tb1", "v2_tbi", "v3_tbi")

#treatment
newdf_treatment_by_visit<- dcast(data_report[,1:14], vista_lastname ~ visit_number)
#rename columns as v1_, v2_, v3_
names(newdf_treatment_by_visit) <- c("vista_lastname", "v1_treat", "v2_treat", "v3_treat")

#medication therapy
newdf_meds_treat_by_visit<- dcast(data_report[,1:15], vista_lastname ~ visit_number)
#rename columns as v1_, v2_, v3_
names(newdf_meds_treat_by_visit) <- c("vista_lastname", "v1_meds_treat", "v2_meds_treat", "v3_meds_treat")

#medication therapy
newdf_talk_treat_by_visit<- dcast(data_report[,1:16], vista_lastname ~ visit_number)
#rename columns as v1_, v2_, v3_
names(newdf_talk_treat_by_visit) <- c("vista_lastname", "v1_talk_treat", "v2_talk_treat", "v3_talk_treat")

newdf_any_treat<- dcast(data_report[,1:17], vista_lastname ~ visit_number)
names(newdf_any_treat) <- c("vista_lastname", "v1_any_treat", "v2_any_treat", "v3_any_treat")

#------------------------------------------------------------------------
#MERGE FILES WITH CALCULATED SCORES
#------------------------------------------------------------------------
#need to MERGE SCORED variables
by_visits000000 <- merge(scored_pcl_change, newdf_talk_treat_by_visit, by=("vista_lastname"), all = TRUE)
by_visits00000 <- merge(by_visits000000, newdf_meds_treat_by_visit, by=("vista_lastname"), all = TRUE)
by_visits0000 <- merge(by_visits00000, newdf_tbi, by=("vista_lastname"), all = TRUE)
by_visits000 <- merge(by_visits0000, newdf_treatment_by_visit, by=("vista_lastname"), all = TRUE)
by_visits00 <- merge(by_visits000, newdf_any_treat, by=("vista_lastname"), all = TRUE)
by_visits0 <- merge(by_visits00,scored_gad_change, by=("vista_lastname"), all = TRUE)
by_visits <- merge(by_visits0, scored_phq_change, by=("vista_lastname"), all = TRUE)

#Only retain relevant variables
calculated_changes<- subset(by_visits, 
                            select= c(vista_lastname,
                                      v2_change_pcl, v3_change_pcl, 
                                      v2_change_gad,  v3_change_gad, 
                                      v2_change_phq,  v3_change_phq,
                                      v1_tb1, v2_tbi, v3_tbi,
                                      v1_treat, v2_treat, v3_treat,
                                      v1_meds_treat, v2_meds_treat, v3_meds_treat,
                                      v1_talk_treat, v2_talk_treat, v3_talk_treat,
                                      v1_any_treat, v2_any_treat, v3_any_treat
                            ))

#remove data frams that will no longer be used
rm ("newdf_treatment_by_visit","newdf_talk_treat_by_visit","newdf_meds_treat_by_visit", "by_visits000000", "by_visits00000","by_visits0000","by_visits000", "by_visits0","scored_pcl_change", "scored_gad_change", "scored_phq_change", "newdf_gad_by_visit", "newdf_pcl_by_visit","newdf_phq_by_visit")


#merge new variables for changes between visits with original report
#need to MERGE SCORED variables for GAD, PHQ9, PCL9
data_report_with_changes <- merge(data_report,calculated_changes, by=("vista_lastname"), all = TRUE)

#extended version of calculated changes
data_report_with_scores_per_person<- merge(data_report,by_visits, by=("vista_lastname"), all = TRUE)

#------------------------------------------------------------------------
#cALL IN FUNCTION TO CREATE ADD/DROP TREATMENT COLUMNS
#------------------------------------------------------------------------
 source("~/Biobank/00_analyses/add_drop_treatment.R")
 report_with_new_cols <- adply(data_report_with_scores_per_person, 1, add_drop)

#------------------------------------------------------------------------
#OUTPUT
#------------------------------------------------------------------------
#Export data report
#write.csv(data_report, "~/Biobank/00_analyses/data_report_2018329.csv",quote=T,row.names=F,na="#N/A")

#Export changes report by visit
#write.csv(by_visits, "~/Biobank/00_analyses/report_with_calculated_changes_by_visits_20180329.csv",quote=T,row.names=F,na="#N/A")

#Export changes report
write.csv(report_with_new_cols, "~/Biobank/00_analyses/report_with_treatment_changes_20180406.csv",quote=T,row.names=F,na="#N/A")
