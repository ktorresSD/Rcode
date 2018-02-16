#########################################################################################
# Last Date modified: 02/15/2018
# Author: Katy Torres
# Description: KMCT Pheno 3 data
##########################################################################################

library(foreign)
file.choose()
dataset = read.spss("C:/Users/Psychiatry Lab/Documents/GWAS_Freeze2_CURRENT/19_KMCT/pheno_3/MT_BASELINE_CAPS_SCORED.sav", to.data.frame=TRUE)
write.csv(dataset, "C:/Users/Psychiatry Lab/Documents/GWAS_Freeze2_CURRENT/19_KMCT/pheno_3/MT_BASELINE_CAPS_SCORED.csv",quote=T,row.names=F,na="#N/A")
