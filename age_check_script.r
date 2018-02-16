#########################################################################################
# Last Date modified: 01/08/2018
# Author: Katy Torres
# Description: age check
##########################################################################################
#importing data
dat <- read.csv('C:/Users/Psychiatry Lab/Documents/GWAS_Freeze2_CURRENT/0_GWAS PAPER TABLE 1 PROJECT/master_without_hapmap_used_for_Table_1.csv',header=T,na.strings=c(NA,"na"))
colnames(dat)

#CHECKING ONG AGE
#-------------------------------------------------------------------
#subsetting by study
DAT.ONG<- dat[which(dat$Study=="ONG"),]

#subsetting by case or control status
cases.ong<- DAT.ONG[which(DAT.ONG$DX==1),]
controls.ong<- DAT.ONG[which(DAT.ONG$DX==2),]

mean(cases.ong$AGE, na.rm=TRUE)
mean(controls.ong$AGE, na.rm=TRUE)

#CHECKING MRS AGE
#-------------------------------------------------------------------
#subsetting by study
dat0<- dat[which(dat$Study=="MRS"),]
cases.mrs<- dat0[which(dat0$DX==1),]
controls.mrs<- dat0[which(dat0$DX==2),]

mean(cases.mrs$AGE, na.rm=TRUE)
mean(controls.mrs$AGE, na.rm=TRUE)

#CHECKING KSUD AGE
#-------------------------------------------------------------------
#subsetting by study
DAT.KSUD<- dat[which(dat$Study=="KSUD"),]

#subsetting by case or control status
cases.KSUD<- DAT.KSUD[which(DAT.KSUD$DX==1),]
controls.KSUD<- DAT.KSUD[which(DAT.KSUD$DX==2),]

mean(cases.KSUD$AGE, na.rm=TRUE)
mean(controls.KSUD$AGE, na.rm=TRUE)
