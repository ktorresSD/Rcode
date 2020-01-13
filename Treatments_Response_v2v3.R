#########################################################################################
# Last Date modified: 08/28/2019
# Author: Katy Torres
# Description: Current Treatments - Looking at treatment response longitudinally
##########################################################################################
library(ggplot2)

#READ THIS !

#BEFORE RUNNING THIS CODE I manually:
#merge in recruitment site column from corefile and remove all perinatal subjects


# Erase the 1st row so the 2nd row becomes the headers
# replaced all the cells containing "Y" entries with "1" and cells containg "N" with "0" in Excel
# Also, I made all missing values for the specific treatments 0's (I.E. QUESTION 2,5,6,7, and 8)
# because we are only interested in the 1's for this varibale making


#PART 1: data manipulation to establish what therapy (if any) was the subject taking in this assessment
#--------------------------------------------------------------------------------------------------
current <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_analyses/Treatment_Response/Current_treatments_201912.csv',header=T,na.strings=c("#N/A",NA, "" ))
#pcl <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/21_PCL_5_monthly/pcl5_current_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
pcl <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_analyses/Treatment_Response/pcl5_current_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))

myvars <- c("assessment_id", "vista_lastname","visit_number","pcl_total", "pcl_33", "pcl_5_dsm","pcl_5_dsm_infer")
pclsub<- pcl[myvars]
dim(current)
dim(pcl)
dim(pclsub)

#MERGE DATASETS TOGETHER
currpcl<-  merge(current, pclsub, by=c("assessment_id","vista_lastname", "visit_number"), all = TRUE)
dim(currpcl)

#currpcl<- currpcl[ ! currpcl$assessment_id %in% c(29028), ]
#dim(currpcl)

write.csv(currpcl, "C:/Users/Nievergelt Lab/Documents/Biobank/00_analyses/Treatment_Response/mergedv2v3.csv")


#Does this subject have a followup?
#----------------------------------
#1. Make a column that indicates if that subject has a 2nd visit 
#and another one that indicates if the subject has a 3rd visit

f <- function(vista_lastname, visit_number, num) {
  ave(visit_number, vista_lastname, FUN = function(x) if (max(
    x) >= num) 'yes' else 'no')
}

idx <- 2:3

currpcl[, paste0('followup', idx)] <- lapply(idx, function(x)
  f(currpcl$vista_lastname, currpcl$visit_number, x))

#2. subset so it only looks at the entries with a 3rd visit, DELETE 1st VISIT ENTRIES, NOT NEEDED NOW
currpcl <- currpcl[which(currpcl$followup3 =='yes'), ]
currpcl <- currpcl[which(currpcl$visit_number != 1), ]

#3a. creates a column that shows all subjects with DSM-5 case status at first of 2 visits
currpcl$dsm_at_fist<- ifelse(currpcl$pcl_5_dsm_infer ==1 & currpcl$visit_number == 2,1,0)

#3b. creates a column that shows all subjects with DSM-5 case status at first of 2 visits
currpcl$pcl33_at_fist<- ifelse(currpcl$pcl_33 ==1 & currpcl$visit_number == 2,1,0)


#4. Subjects who said they were in PTSD treatment at first of 2 visits
currpcl$PTSD_at_fist<- ifelse(currpcl$CurrTx2c_PTSD_p2 ==1 & currpcl$visit_number == 2,1,0)


#5. Criteria met: Has a followup visit, PCL-5 DX and said PTSD in treatment at first of 2 visits
currpcl$Criteria_met<- ifelse(currpcl$followup3 =='yes' & currpcl$pcl33_at_fist ==1 & currpcl$dsm_at_fist ==1 & currpcl$PTSD_at_fist== 1,1,0)


#6. If subject met criteria in first visit, then also keep their 2nd visit and remove all other
#if criteria met, put vista_lastname in a list. Then go through and only keep the IDs in the list.

criterialist <- currpcl$vista_lastname[which(currpcl$Criteria_met==1)]

#subsets to only keep the subjects who met criteria
currpcl<- currpcl[currpcl$vista_lastname %in% criterialist, ]



#MAKING GROUP VARIABLES
#------------------------


#make any NA's 0. we are only interested in seeing the number of assessment that said yes, this simplifies coding
currpcl$CurrentTreatmentsY_MHTx_p2[is.na(currpcl$CurrentTreatmentsY_MHTx_p2)] <- 0


#if any assessment said yes to being treated or being treated for a specific disorder, then anytreat will equal 1
for(i in 1:dim(currpcl)[1]) {

if(currpcl$CurrentTreatmentsY_MHTx_p2[i] == 1) {currpcl$anytreat[i] <- 1
}else if(currpcl$CurrTx2a_Dep_p2[i] == 1 )   {currpcl$anytreat[i] <- 1
}else if(currpcl$CurrTx2b_Anx_p2[i] == 1 )  {currpcl$anytreat[i] <- 1
}else if(currpcl$CurrTx2c_PTSD_p2[i] == 1)  {currpcl$anytreat[i] <- 1
}else if(currpcl$CurrTx2d_Schiz_p2[i] == 1) {currpcl$anytreat[i] <- 1
}else if(currpcl$CurrTx2e_BP_p2[i] == 1)    {currpcl$anytreat[i] <- 1
}else if(currpcl$CurrTx2f_Subst_p2[i] == 1) {currpcl$anytreat[i] <- 1
}else if(currpcl$CurrTx2g_Other_p2[i] == 1) {currpcl$anytreat[i] <- 1
}else{currpcl$anytreat[i] <- 0}

}

#MEDS

#make any NA's 0. we are only interested in seeing the number of assessment that said yes, this simplifies coding
currpcl$CurrTx5_Meds_p2[is.na(currpcl$CurrTx5_Meds_p2)] <- 0

#if any assessment said yes to taking meds or said a specific med, then anymeds will equal 1
for(i in 1:dim(currpcl)[1]) {
  
  if(currpcl$CurrTx5_Meds_p2[i] == 1) {currpcl$anymeds[i] <- 1
  }else if(currpcl$CurrTx5a_AntiD_p2[i] == 1)   {currpcl$anymeds[i] <- 1
  }else if(currpcl$CurrTx5b_Mood_p2[i] == 1 )  {currpcl$anymeds[i] <- 1
  }else if(currpcl$CurrTx5c_Stim_p2[i] == 1)  {currpcl$anymeds[i] <- 1
  }else if(currpcl$CurrTx5d_Sleep_p2[i] == 1) {currpcl$anymeds[i] <- 1
  }else if(currpcl$CurrTx5e_Benzo_p2[i] == 1)    {currpcl$anymeds[i] <- 1
  }else if(currpcl$CurrTx5f_AntiPsy_p2[i] == 1) {currpcl$anymeds[i] <- 1
  }else if(currpcl$CurrTx5g_Adren_p2[i] == 1) {currpcl$anymeds[i] <- 1
  }else if(currpcl$CurrTx5h_Other_p2[i] == 1) {currpcl$anymeds[i] <- 1
  }else{currpcl$anymeds[i] <- 0}
  
}

#TALK THERAPY


#if any assessment said yes to any of the talk therpy optins, then anytalk will equal 1
for(i in 1:dim(currpcl)[1]) {
  
  if(currpcl$CurrentTreatments6_Psycho_p2[i] > 0) {currpcl$anytalk[i] <- 1
  
  }else if(currpcl$CurrTx6.1_Group_p2[i] == 1)   {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx6.1_Ind_p2[i] == 1 )  {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx6.1_Fam_p2[i] == 1)  {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx6.1_Couples_p2[i] == 1) {currpcl$anytalk[i] <- 1
  
  }else if(currpcl$CurrTx7a_CBTAnx_p2[i] == 1)    {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx7b_CBTDep_p2[i] == 1) {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx7c_CBTInsom_p2[i] == 1) {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx7d_IRTNM_p2[i] == 1) {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx7e_CBTBP_p2[i] == 1)   {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx7f_CPT_p2[i] == 1 )  {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx7g_Anger_p2[i] == 1)  {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx7h_ACT_p2[i] == 1) {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx7i_Other_p2[i] == 1)    {currpcl$anytalk[i] <- 1
  
  }else if(currpcl$CurrTx8a_CBTAnx_p2[i] == 1) {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx8b_CBTDep_p2[i] == 1) {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx8c_CBTInsom_p2[i] == 1) {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx8d_IRTNM_p2[i] == 1)   {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx8e_CPT_p2[i] == 1 )  {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx8f_PE_p2[i] == 1)  {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx8g_EMDR_p2[i] == 1) {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx8h_ACT_p2[i] == 1)    {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx8g_Anger_p2[i] == 1) {currpcl$anytalk[i] <- 1
  }else if(currpcl$CurrTx8i_Other_p2[i] == 1) {currpcl$anytalk[i] <- 1
  }else{currpcl$anytalk[i] <- 0}
  
}

#variables for talk, meds, both
for(i in 1:dim(currpcl)[1]) {
  
  if((currpcl$anytalk[i] > 0) & (currpcl$anymeds[i] > 0)) {currpcl$both[i] <- 1
  }else{currpcl$both[i] <- 0}
  
}

for(i in 1:dim(currpcl)[1]) {
  
  if((currpcl$anytreat[i] > 0) |(currpcl$anytalk[i] > 0) | (currpcl$anymeds[i] > 0) | (currpcl$both[i] > 0) ) {currpcl$treated[i] <- 1
  }else{currpcl$treated[i] <- 0}
  
}

#making a group variable that classifies each person as one only
for(i in 1:dim(currpcl)[1]) {
  

  if(currpcl$both[i]==1){ currpcl$group[i] <- "both"
    }else if(currpcl$anymeds[i]==1){ currpcl$group[i] <- "meds"
    }else if(currpcl$anytalk[i]==1){ currpcl$group[i] <- "talk"
  }else{currpcl$group[i]<-"No specified treatment"}
}





#PLOT (needs work)
#--------------------------------------------------------------------------------------------------
#Plot individual PCL curves by visit
p <- ggplot(data = currpcl, aes(x = visit_number, y = pcl_total, group = as.factor(vista_lastname), colour=group))
p + geom_line() + facet_grid(cols = vars(group))

#EXPORT SUBSET
#--------------------------------------------------------------------------------------------------
write.csv(currpcl, "C:/Users/Nievergelt Lab/Documents/Biobank/00_analyses/Treatment_Response/subset_v2v3.csv")




#PART 2: #Now we will work with a subset of the file form part 1 to look at things longitudinally
#--------------------------------------------------------------------------------------------------

#only want to retain these variables
dim(currpcl)
myvars <- c("assessment_id","vista_lastname", "visit_number", "CurrTx2c_PTSD_p2",  
            "CurrentTreatmentsY_MHTx_p2","group","treated", "anymeds" , "anytalk", "both",
            "pcl_total", "pcl_5_dsm","pcl_5_dsm_infer")               

SUBcurr<- currpcl[,myvars]
dim(SUBcurr)
names(SUBcurr)


#Transpose data to wide format

l.sort <- SUBcurr[order(SUBcurr$vista_lastname),]
curr_wide <- reshape(l.sort,
                     timevar = "visit_number",
                     idvar = c( "vista_lastname"),
                     direction = "wide")
names(curr_wide)

#LOOK AT PCL SCORES BETWEEN VISIT 1 AND 2

#make a new variable that looks at PCL change between visits 2 and 3
curr_wide$v2v3 <- curr_wide$pcl_total.3 - curr_wide$pcl_total.2

#if positive (by more than 10 points)  then the person got worse
#if positive (by more than 15 points)  then the person got significantly worse
# if change is between -10 and 10 we say no change.
#if negative (by more than 10 points)  then the person got better
#if negative (by more than 15 points)  then the person got significantly better

for(i in 1:dim(curr_wide)[1]) {

if(!(is.na(curr_wide$v2v3[i]))){
  
  if(curr_wide$v2v3[i] >= 15) {curr_wide$change_v2_v3[i] <- "significantly worse"
    }else if(curr_wide$v2v3[i] >= 10 & curr_wide$v2v3[i] < 15){curr_wide$change_v2_v3[i] <- "worse"
    }else if(curr_wide$v2v3[i] <= -10 & curr_wide$v2v3[i] > -15) {curr_wide$change_v2_v3[i] <- "better"
    }else if(curr_wide$v2v3[i] <= -15) {curr_wide$change_v2_v3[i] <- "significantly better"
    }else{curr_wide$change_v2_v3[i] <- "no change"}
  
}else{curr_wide$change_v2_v3[i] <-NA}

}


#check if logic was correct
cbind(curr_wide$v2v3, curr_wide$change_v2_v3)
names(curr_wide)


#EXPORT SUBSET
#--------------------------------------------------------------------------------------------------
myvars2 <- c("vista_lastname", "assessment_id.2", "assessment_id.3", 
             "group.2","group.3", "treated.2", "treated.3", "pcl_total.2", "pcl_total.3", "v2v3", "change_v2_v3")
SUBcurr2<- curr_wide[,myvars2]
write.csv(SUBcurr2, "C:/Users/Nievergelt Lab/Documents/Biobank/00_analyses/Treatment_Response/0_currentTX_longitudinal_followup_v2v3.csv")




