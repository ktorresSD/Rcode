#########################################################################################
# Last Date modified: 07/09/2019
# Author: Katy Torres
# Description: Combining datasets to provide list of selected emasures in one file
##########################################################################################
setwd('C:/Users/Nievergelt Lab/Documents/Biobank/000_Data_Requests/00_Arpi')

#________________________________________________________________________________________              
# READ IN DATA
#----------------------------------------------------------------------------------------
core <- read.csv('biobank_corefile_2018_Freeze_1.csv',header=T, na.strings=c("",NA))
dim(core)
myvars <- c("assessment_id", "vista_lastname","visit_number","recruitment_site" ,"status")
core1<- core[myvars]
dim(core1)

#REMOVE EXCLUDED SUBJECTS (subject 14 has no PCl and other subject was a duplicating mistake)
core2 <- core1[ ! core1$assessment_id %in% c(8835,17071), ]
dim(core2)


setwd('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_1_2018_data/scored_data_from_eScreening_modules/')

#Merge in BTBIS
#--------------------------------
datbm <- read.csv('BTBIS_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
dim(datbm)
names(datbm)
dat0 <- merge(core2, datbm, by=c("assessment_id", "vista_lastname", "visit_number"), all = TRUE)
dim(dat0)

#Merge in PCL-Monthly data
#--------------------------------
datpcl <- read.csv('pcl5_current_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
dim(datpcl)
names(datpcl)
pclnames<- c("assessment_id", "vista_lastname","visit_number","pcl_m_total", "pcl_m_incomplete", "pcl_m_33", "pcl_m_5_dsm", "pcl_m_5_dsm_infer")
pclsub <- datpcl[pclnames]
dat01 <- merge(dat0, pclsub , by=c("assessment_id", "vista_lastname", "visit_number"), all = TRUE)
dim(dat01)


#Merge in Demographic Data
#--------------------------------
datdemo <- read.csv('Basic_Demographic_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
dim(datdemo)
names(datdemo)
DEMONAMES<-c("assessment_id", "vista_lastname","visit_number", "demo_gender_r", "approx_age", "demo_ethnic_r",
             "demo_racewhite", "demo_race_black", "demo_race_amind", "demo_race_pacisl","demo_race_asian",
             "demo_race_decline", "demo_race_oth", "demo_race_oth_spec")
demosubdem<- datdemo[DEMONAMES]
dat2 <- merge(dat01, demosubdem, by=c("assessment_id", "vista_lastname", "visit_number"), all = TRUE)
dim(dat2)

#Merge in Edcuation
#--------------------------------
datbcb <- read.csv('Demographic_employment_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
dim(datbcb)
names(datbcb)
bcbNAMES<-c("assessment_id", "vista_lastname", "visit_number" , 
            "demo_income_group", "demo_education", "demo_workstatus",             
            "demo_hours" ,                   "demo_occupation",              
            "demo_income_none" ,             "demo_income_wrk" ,             
             "demo_income_unemp"  ,           "demo_income_dis"  ,            
            "demo_income_gi",                "demo_income_retire", "demo_income_other")
bcbsub<- datbcb[bcbNAMES]
dat3 <- merge(dat2,datbcb , by=c("assessment_id", "vista_lastname", "visit_number"), all = TRUE)
dim(dat3)

#Merge in CDDR
#--------------------------------
datcddr <- read.csv('CDDR_reduced_data_export_combined_use.csv',header=T,na.strings=c("#N/A",NA))
dim(datcddr)
names(datcddr)
dat4<- merge(dat3,datcddr , by=c("assessment_id", "vista_lastname", "visit_number"), all = TRUE)
dim(dat4)

#Merge in GAD
#--------------------------------
datgad <- read.csv('gad7_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
dim(datgad)
names(datgad)
DEMONAMES<-c("assessment_id", "vista_lastname","visit_number", "gad7_total", "gad7_incomplete",
             "score_interpretation_gad7")
gadsub<- datgad[DEMONAMES]
dat5 <- merge(dat4, gadsub , by=c("assessment_id", "vista_lastname", "visit_number"), all = TRUE)
dim(dat5)


#Merge in PHQ9
#--------------------------------
datphq9<- read.csv('phq9_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
dim(datphq9)
names(datphq9)
DEMONAMES<-c("assessment_id", "vista_lastname","visit_number", "phq9_total", "score_interpretation_phq9")
demosub9<- datphq9[DEMONAMES]
dat6 <- merge(dat5, demosub9 , by=c("assessment_id", "vista_lastname", "visit_number"), all = TRUE)
dim(dat6)


#Merge in PANAS
#--------------------------------
datpanas<- read.csv('PANAS_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
dim(datpanas)
names(datpanas)
DEMONAMES<-c("assessment_id", "vista_lastname","visit_number", "Negative_Affect_Score", 
             "Positive_Affect_Score")
demopanasub<- datpanas[DEMONAMES]
dat7 <- merge(dat6, demopanasub , by=c("assessment_id", "vista_lastname", "visit_number"), all = TRUE)
dim(dat7)



#Merge in WHODAS
#--------------------------------
dawho<- read.csv('WHODAS_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
dim(dawho)
names(dawho)
DEMONAMES<-c("assessment_id", "vista_lastname","visit_number", "score_sum_imputed", 
             "summaryscore_imputed")
demosubw<- dawho[DEMONAMES]
dat8 <- merge(dat7, demosubw, by=c("assessment_id", "vista_lastname", "visit_number"), all = TRUE)
dim(dat8)


#Merge in Social Environnent 
#--------------------------------
datsocial<- read.csv('Demographic_social_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
dim(datsocial)
names(datsocial)
dat9 <- merge(dat8, datsocial , by=c("assessment_id", "vista_lastname", "visit_number"), all = TRUE)
dim(dat8)





write.csv(dat9, "C:/Users/Nievergelt Lab/Documents/Biobank/000_Data_Requests/00_Arpi/output_for_subject_selection_freeze1_subjects.csv" ,quote=T, row.names=F,na="#N/A")
