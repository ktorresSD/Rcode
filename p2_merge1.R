#########################################################################################
# Last Date modified: 08/08/2018
# Author: Katy Torres
# Description: Compile all p2 files into one large file
              #read in all p2 files
              #take only relevant columns
              #if they dont exist, create them and fill them with -9
              #rbing columns together for a full p2 list
##########################################################################################

#STEPS TO TAKE:
#read in data
#select only specified columns
#if columns are not there fill them with -9 so everything is the same size and can be rbound together

setwd('C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/p2_files/')

#READ IN DATA
file.names <- list.files(path = 'C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/p2_files/')

# read in each file in the directory, store them as individual dataframes
for (i in 1:length(file.names)){
  X.i<- fread(file.names[i], select = c("study_number", "study_abbrev","IID","FID","Self_report_ancestry",
                                  "Sex","Case","Age","Lifetime_PTSD_Dx","Current_PTSD_Dx","Lifetime_PTSD_Continuous",
                                  "Current_PTSD_Continuous","Exposure","CT_Count","LT_Count"), na.strings=c("-9", -9, "#N/A", NA))
  assign(x = file.names[i], value = X.i)
  rm(list = c('X.i'))
}
rm(i)
ls()   


#make list of dataframes
#NOT INCLUDING p2_bya.csv, p2_nss.csv,  p2_ppds.csv
list1<- list(p2_adnh.csv,                 p2_bake.csv,        
p2_betr.csv,         p2_betr_control.csv, p2_boba.csv,        
p2_bry2.csv ,        p2_coga.csv,         p2_cogb.csv,        
p2_com1.csv,         p2_dcsr.csv,         p2_defe.csv,        
p2_dnhs.csv,         p2_eacr.csv,         p2_eghs.csv ,       
p2_feen.csv,         p2_fscd.csv,         p2_ftcb.csv,        
p2_gali.csv,         p2_grac.csv,         p2_gsdc.csv,        
p2_gtpc.csv,         p2_guts.csv,         p2_kmct.csv ,       
p2_ksud.csv,         p2_mrsc.csv,         p2_ncmh.csv ,       
p2_nhrv.csv,         p2_nhs2.csv,         p2_nhsy.csv ,       
p2_niut.csv,         p2_onga.csv ,       
p2_port.csv,         p2_pris.csv ,       
p2_prom.csv,         p2_ring.csv,         p2_safr.csv ,       
p2_satu.csv,         p2_seep.csv ,        p2_shrs.csv ,       
p2_stro.csv,         p2_teic.csv ,        p2_vets.csv ,       
p2_vris.csv,         p2_wach.csv,         p2_wang.csv ,       
p2_yehu.csv)

#COMBINES LIST OF DATAFRAMES INTO 1 
p2list<- rbindlist(list1, fill=TRUE)
p2list$phenotype_id<- p2list$IID

dim(p2list) #65889
names(p2list)

#the following shold be dichotomous
table(p2list$Sex) #STRONGSTAR has some missing SEX info
table(p2list$Case) 
table(p2list$Current_PTSD_Dx)
table(p2list$Lifetime_PTSD_Dx) #not, recode FTCB

#check what study has Lifetime_PTSD_Dx issues
ltptsdfix<-p2list[p2list$Lifetime_PTSD_Dx==0,]
table(ltptsdfix$study_abbrev)

# #read in and binds together all files but only works with equal number of columns
# do.call(rbind, lapply(list.files(path='.', pattern="p2_"), read.csv, header=TRUE))
