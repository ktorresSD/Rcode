#########################################################################################
# Last Date modified: 2/16/2018
# Author: Katy Torres
# Description: Age significantly different between cases and controls?
##########################################################################################

#load required libraries
library(plyr)
#Replace this path with location where data is currently stored
setwd('C:/Users/Psychiatry Lab/Documents/GWAS_Freeze2_CURRENT/0_GWAS PAPER TABLE 1 PROJECT')


#READ DATA
#------------------------------------------------------------------------
dataset0 <- read.csv('Table1_master_without_hatmap.csv',header=T,na.strings=c(NA,999))


#DATA CLEANING PRIOR TO TEST
#--------------------------------------------------------------------------------------
table(dataset0$DX)

#exclude all entries where DX is missing == -9
datasetwithdx <- dataset0[which(dataset0$DX=='1' | dataset0$DX=='2'), ]
#check exclusion worked
table(datasetwithdx$DX)

##EXCLUDE ALL STUDIES THAT HAVE NO CONTROLS or AGES (THEY WILL PRODUCE ERROR IN T.TEST FUCNTION)
 dataset <- datasetwithdx[!datasetwithdx$Study %in% c("FSCD","COGA", "COGB", "Bryant IVS and fire", "VETSA", "CHOICE AND OPT", "DCS Rothbaum", "Risborough" ), ]
 table(dataset$Study)
 dataset$Study <- droplevels(dataset$Study)
 
 

#PERFORM T.TEST TO SEE IF SIGNIFICANT DIFFERENCE BETWEEN AGE IN CASES AND CONTROLS
#--------------------------------------------------------------------------------------

#ONE STUDY DONE FOR VALIDATING FUNCTION
#------------------------------------
#t test for one as validation of function
baker <- dataset[which(dataset$Study=="Baker"),]
ttest0<- t.test(baker$AGE, baker$DX)
ttest0
ttest0$p.value

#Double check mean age for controls and cases
bakercontrols <- baker[which(baker$DX==1),]
bakercases <- baker[which(baker$DX==2),]
mean(bakercontrols$AGE,  na.rm=TRUE)
mean(bakercases$AGE,  na.rm=TRUE)

#FUNCTION TO RUN THROUGH ALL STUDIES
#------------------------------------

#split by study first
split.dataset<- split(dataset,dataset$Study, drop=FALSE)


df<-sapply(split.dataset, function(x) {
#want to see if age is significantly different
ttest<- t.test(x$AGE,x$DX)
return(ttest$p.value)
})

# #OR DO THIS INSTEAD OF SPLIT FUNCTION

ttestfun <- function(df)
{
  t.test(df$AGE,df$DX)$p.value
}
DF2<-ddply(dataset, ~Study,ttestfun)

write.csv(df,  "C:/Users/Psychiatry Lab/Documents/GWAS_Freeze2_CURRENT/0_GWAS PAPER TABLE 1 PROJECT/age_difference_test_20180216.csv",quote=T,row.names=TRUE,na="#N/A")

write.csv(DF2,  "C:/Users/Psychiatry Lab/Documents/GWAS_Freeze2_CURRENT/0_GWAS PAPER TABLE 1 PROJECT/age_difference_test_20180216_v2.csv",quote=T,row.names=TRUE,na="#N/A")
