#########################################################################################
# Last Date modified: 08/08/2018
# Author: Katy Torres
# Description: 1.Fam files have been previosuly merged together, a ID linker list has been previosuly compiled.
#              2.Merge fam files with ID linker list. 
#              3.Manipulate phenotype2 files so they can all be merged together. 
#              4.Merge phenotype 2 files with dataset (fams and id linker).
#              5.Check PTSD DX and sex concordance between GWAS information (fam files) and Phenotype information (p2 files)
##########################################################################################

#Replace this path with location where data is currently stored
setwd('C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project')

#________________________________________________________________________________________              
# 1.READ IN DATA
# CHANGE FILE NAMES AND EXPORT DATE
#----------------------------------------------------------------------------------------
fams <- read.csv('~/GWAS_Freeze2_CURRENT/000_GxE_Project/fam_files/pgc_ptsd_freeze2_famfiles_withvetsa.csv',header=T,na.strings=c("#N/A", "-9",NA))
linker <- read.csv('~/GWAS_Freeze2_CURRENT/000_GxE_Project/record_linker/record_linker_file_20180808.csv',header=T, na.strings=c("",NA))

#NOTE: fams file has 73 HATMAP ID'S (all form pts1), linker file does not contain HATMAP ID's

dim(fams)   #72426     6
dim(linker) #72364     5
# there are 62 more in the fam file
#________________________________________________________________________________________  
# 2. MERGE DATASETS TOGETHER
#------------------------------------------------------------------------
#merge CPRS corefile and full dataset by assesstment id % LAST NAME
dat0 <- merge(fams, linker, by=c("gwas_fid", "gwas_iid"), all = TRUE)

#TAKE A LOOK AT MERGED ITEMS, DATA CLEAN EACH FILE TO CORRECT ANY AISING MERGING ISSUES AND RE-MERGE

#merge checks
dim(dat0)   #72437     9
names(dat0) #"gwas_fid"     "gwas_iid"     "parent1"      "parent2"      "gwas_sex"    "gwas_dx"      "study_number" "study_abbrev" "phenotype_id"
View(dat0)
nrow(subset(dat0, parent1== "NA12891"))

#NOTE: linker file has 11 more ONGA subjects than fams file.
#NOTE: fams file has 122 more HATMAP ID's than linker file
#con_pts_wrby_mix_am_PSYC*3495        3495_2 NOT IN LINKER FILE (now added)

#________________________________________________________________________________________ 
#Export
# #----------------------------------------------------------------------------------------
# filename <- paste("C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/famswithlinker.csv", sep="")
# write.csv(dat0, filename,quote=T,row.names=F,na="#N/A")

#________________________________________________________________________________________  
# 3. RUN P2_MERGE1 CODE!
#------------------------------------------------------------------------
View(p2list)


#________________________________________________________________________________________  
# 4. MERGE DATASETS TOGETHER
#------------------------------------------------------------------------
#merge CPRS corefile and full dataset by assesstment id % LAST NAME
data_merged <- merge(dat0, p2list, by=c("phenotype_id", "study_number"), all = TRUE)

dim(p2list) #65889
dim(dat0) #72437
dim(data_merged) #88955


# 
# defenddata <- data_merged[which(data_merged$study_number==12),]
# saturndata <- data_merged[which(data_merged$study_number==11),]
# feenydata <- data_merged[which(data_merged$study_number==37),]


noGWASinfo<- data_merged[is.na(data_merged$gwas_iid),]
dim(noGWASinfo) #16514
noGWASinfo_n<- table(noGWASinfo$study_number)
noGWASinfo_n

matched_only<- data_merged[!(is.na(data_merged$gwas_iid)),]

dim(matched_only) #72441

no_case_status<- data_merged[is.na(data_merged$Case),]
table(no_case_status$study_number)
#write.csv(noGWASinfo, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/noGWASinfo.csv",quote=T,row.names=F,na="#N/A")

fam_n<- table(dat0$study_number)
matched_n<- table(matched_only$study_number)
Nsmatched<- cbind(fam_n,matched_n)
Nsmatched


defenddata <- matched_only[which(matched_only$study_number==12),]
saturndata <- matched_only[which(matched_only$study_number==11),]
feenydata <- matched_only[which(matched_only$study_number==37),] #share now
data41 <- matched_only[which(matched_only$study_number==41),] #share now
data42 <- matched_only[which(matched_only$study_number==42),] #share now
data43 <- matched_only[which(matched_only$study_number==43),] #share now
data46 <- matched_only[which(matched_only$study_number==46),] #share now


# x_n<- table(matched_only$study_abbrev.x)
# y_n<- table(matched_only$study_abbrev.y)
# Ns<- cbind(x_n,y_n)
# Ns

#________________________________________________________________________________________  
# 5. Check concordance
#------------------------------------------------------------------------
# work with matched_only dataset

#CHECKING SEX N's
table(matched_only$gwas_sex) #from gwas
table(matched_only$Sex) #from phenotype

#CHECKING Case N's
table(matched_only$gwas_dx) #from gwas
table(matched_only$Case) #from phenotype

#2609 dont have a case status in gwas
#39262 don't have a case status in phentoype


#check if DX is the same in gwas and phenotype data

for (i in 1:nrow(matched_only)) {
  #first ask if they are not both NA
  if(identical(matched_only$Case[i], matched_only$gwas_dx[i])){ matched_only$dxmatch[i] <- "match"
  }else if (is.na(matched_only$Case[i])& is.na(matched_only$gwas_dx[i])){matched_only$dxmatch[i] <- "missing_dx"
  }else if (is.na(matched_only$Case[i])){matched_only$dxmatch[i] <- "missing_pheno_dx"
  }else if (is.na(matched_only$gwas_dx[i])){matched_only$dxmatch[i] <- "missing_gwas_dx"
  }else {matched_only$dxmatch[i] <- "no_match"}
}
table(matched_only$dxmatch)

#export table of case/control status match by study
library(pivottabler)
pt2 <- PivotTable$new()
pt2$addData(matched_only)
pt2$addColumnDataGroups("dxmatch")
pt2$addRowDataGroups("study_number")
pt2$defineCalculation(calculationName="TotalN", summariseExpression="n()")
pt2$evaluatePivot()
pt2save<-pt2$asMatrix(includeHeaders=TRUE, rawValue=TRUE)
pt2save
#write.csv(pt2save, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/dxmatch_bystudy.csv",quote=T,row.names=F,na="#N/A")


#check if SEX is the same in gwas and phenotype data

for (i in 1:nrow(matched_only)) {
  #first ask if they are not both NA
  if(identical(matched_only$Sex[i], matched_only$gwas_sex[i])){ matched_only$sexmatch[i] <- "match"
  }else if (is.na(matched_only$Sex[i])& is.na(matched_only$gwas_sex[i])){matched_only$sexmatch[i] <- "missing_sex"
  }else if (is.na(matched_only$Sex[i])){matched_only$sexmatch[i] <- "missing_pheno_sex"
  }else if (is.na(matched_only$gwas_sex[i])){matched_only$sexmatch[i] <- "missing_gwas_sex"
  }else {matched_only$sexmatch[i] <- "no_match"}
}
table(matched_only$sexmatch)

#export table of sex status match by study
library(pivottabler)
pt <- PivotTable$new()
pt$addData(matched_only)
pt$addColumnDataGroups("sexmatch")
pt$addRowDataGroups("study_number")
pt$defineCalculation(calculationName="TotalN", summariseExpression="n()")
pt$evaluatePivot()
ptsave<-pt$asMatrix(includeHeaders=TRUE, rawValue=TRUE)
ptsave
#write.csv(ptsave, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/sexmatch_bystudy.csv",quote=T,row.names=F,na="#N/A")

#SUBJECTS THAT DO NOT MATCH, INVESTIGATE FURTHER
#check subjects whose DX does not match
no_match_subjects<- matched_only[matched_only$dxmatch=="no_match",]

#check subjects whose DX does not match
no_match_sex_subjects<- matched_only[matched_only$sexmatch=="no_match",]

subjects_who_do_not_match<- rbind(no_match_subjects, no_match_sex_subjects)
#write.csv(subjects_who_do_not_match, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/subjects_with_match_issues.csv",quote=T,row.names=F,na="#N/A")

#cHECK IF THE NUMBER OF CASES MATCHED WHAT IS ON TABLE 1
p2case<- table(matched_only$study_number, by= matched_only$Case )
p1case<- table(matched_only$study_number, by= matched_only$gwas_dx)
cases<- cbind(p2case, p1case)

#Subset and export files where all items match
mrsc_1<- matched_only[which(matched_only$study_number==1),]
onga_2<- matched_only[which(matched_only$study_number==2),]
safr_3<- matched_only[which(matched_only$study_number==3),]
dnhs_4<- matched_only[which(matched_only$study_number==4),]
nhs2_5<- matched_only[which(matched_only$study_number==5),]
gsdc_6<- matched_only[which(matched_only$study_number==6),]
fscd_7<- matched_only[which(matched_only$study_number==7),]
coga_8<- matched_only[which(matched_only$study_number==8),]
cogb_9<- matched_only[which(matched_only$study_number==9),]
satu_11<- matched_only[which(matched_only$study_number==11),] #dont match table 1 (controls: 58,cases: 85)
defe_12<- matched_only[which(matched_only$study_number==12),] #dont match table 1 (controls: 48,cases: 78)
nhrv_13<- matched_only[which(matched_only$study_number==13),]

ksud_17<- matched_only[which(matched_only$study_number==17),]
boba_18<- matched_only[which(matched_only$study_number==18),]
kmct_19<- matched_only[which(matched_only$study_number==19),]
port_20<- matched_only[which(matched_only$study_number==20),]
guts_21<- matched_only[which(matched_only$study_number==21),]
nrv2_22<- matched_only[which(matched_only$study_number==22),]
prom_23<- matched_only[which(matched_only$study_number==23),]

pris_25<- matched_only[which(matched_only$study_number==25),]

ring_33<- matched_only[which(matched_only$study_number==33),]

bry2_36<- matched_only[which(matched_only$study_number==36),]
feen_37<- matched_only[which(matched_only$study_number==37),]
dcsr_38<- matched_only[which(matched_only$study_number==38),]

teic_39<- matched_only[which(matched_only$study_number==39),]

niut_40<- matched_only[which(matched_only$study_number==40),]
ncmh_41<- matched_only[which(matched_only$study_number==41),]
earc_42<- matched_only[which(matched_only$study_number==42),]
wach_43<- matched_only[which(matched_only$study_number==43),]
eghs_44<- matched_only[which(matched_only$study_number==44),]
shrs_46<- matched_only[which(matched_only$study_number==46),]
adnh_45<- matched_only[which(matched_only$study_number==45),]
gtpc_47<- matched_only[which(matched_only$study_number==47),]
betr_48<- matched_only[which(matched_only$study_number==48),]
seep_49<- matched_only[which(matched_only$study_number==49),]
com1_50<- matched_only[which(matched_only$study_number==50),]
ftcb_52<- matched_only[which(matched_only$study_number==52),]
grac_54<- matched_only[which(matched_only$study_number==54),]
gmrf_55<- matched_only[which(matched_only$study_number==55),]
yehu_56<- matched_only[which(matched_only$study_number==56),]
bake_57<- matched_only[which(matched_only$study_number==57),]
vris_58<- matched_only[which(matched_only$study_number==58),]
wang_59<- matched_only[which(matched_only$study_number==59),]



write.csv(mrsc_1, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/1_mrsc.csv",quote=T,row.names=F,na="-9")
write.csv(onga_2, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/2_onga.csv",quote=T,row.names=F,na="-9")
write.csv(safr_3, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/3_safr.csv",quote=T,row.names=F,na="-9")
write.csv(dnhs_4, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/4_dnhs.csv",quote=T,row.names=F,na="-9")
write.csv(nhs2_5, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/5_nhs2.csv",quote=T,row.names=F,na="-9")
write.csv(gsdc_6, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/6_gsdc.csv",quote=T,row.names=F,na="-9")
write.csv(fscd_7, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/7_fscd.csv",quote=T,row.names=F,na="-9")
write.csv(coga_8, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/8_coga.csv",quote=T,row.names=F,na="-9")
write.csv(cogb_9, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/9_cogb.csv",quote=T,row.names=F,na="-9")
write.csv(satu_11, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/11_satu.csv",quote=T,row.names=F,na="-9")
write.csv(defe_12, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/12_defe.csv",quote=T,row.names=F,na="-9")
write.csv(nhrv_13, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/13_nhrv.csv",quote=T,row.names=F,na="-9")
write.csv(ksud_17, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/17_ksud.csv",quote=T,row.names=F,na="-9")
write.csv(boba_18, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/18_boba.csv",quote=T,row.names=F,na="-9")
write.csv(kmct_19, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/19_kmct.csv",quote=T,row.names=F,na="-9")
write.csv(port_20, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/20_port.csv",quote=T,row.names=F,na="-9")
write.csv(guts_21, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/21_guts.csv",quote=T,row.names=F,na="-9")
write.csv(nrv2_22, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/22_nrv2.csv",quote=T,row.names=F,na="-9")
write.csv(prom_23, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/23_prom.csv",quote=T,row.names=F,na="-9")
write.csv(pris_25, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/25_pris.csv",quote=T,row.names=F,na="-9")
write.csv(ring_33, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/33_ring.csv",quote=T,row.names=F,na="-9")
write.csv(bry2_36, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/36_bry2.csv",quote=T,row.names=F,na="-9")
write.csv(feen_37, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/37_feen.csv",quote=T,row.names=F,na="-9")
write.csv(dcsr_38, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/38_dcsr.csv",quote=T,row.names=F,na="-9")
write.csv(teic_39, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/39_teic_1.csv",quote=T,row.names=F,na="-9")


write.csv(niut_40, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/40_niut.csv",quote=T,row.names=F,na="-9")
write.csv(ncmh_41, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/41_ncmh.csv",quote=T,row.names=F,na="-9")
write.csv(earc_42, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/42_earc.csv",quote=T,row.names=F,na="-9")
write.csv(wach_43, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/43_wach.csv",quote=T,row.names=F,na="-9")
write.csv(eghs_44, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/44_eghs.csv",quote=T,row.names=F,na="-9")
write.csv(adnh_45, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/45_adnh.csv",quote=T,row.names=F,na="-9")
write.csv(shrs_46, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/46_shrs.csv",quote=T,row.names=F,na="-9")
write.csv(gtpc_47, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/47_gtpc.csv",quote=T,row.names=F,na="-9")
write.csv(betr_48, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/48_betr.csv",quote=T,row.names=F,na="-9")
write.csv(seep_49, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/49_seep.csv",quote=T,row.names=F,na="-9")
write.csv(com1_50, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/50_com1.csv",quote=T,row.names=F,na="-9")
write.csv(ftcb_52, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/52_ftcb.csv",quote=T,row.names=F,na="-9")
write.csv(grac_54, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/54_grac.csv",quote=T,row.names=F,na="-9")
write.csv(gmrf_55, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/55_gmrf.csv",quote=T,row.names=F,na="-9")
write.csv(yehu_56, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/56_yehu.csv",quote=T,row.names=F,na="-9")
write.csv(bake_57, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/57_bake.csv",quote=T,row.names=F,na="-9")
write.csv(vris_58, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/58_vris.csv",quote=T,row.names=F,na="-9")
write.csv(wang_59, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/59_wang.csv",quote=T,row.names=F,na="-9")



# 
# # plot(nhs2$CT_Count,nhs2$gwas_dx,pch=8)
# lmfit   <- lm(gwas_dx~CT_Count,nhs2)
# plot(jitter(nhs2$gwas_dx),jitter(nhs2$CT_Count))
# 
# plot(nhs2$gwas_dx,nhs2$CT_Count)
# 
# cor(nhs2$LT_Count,nhs2$CT_Count, use= "pairwise.complete.obs")
# cor(nhs2$gwas_dx,nhs2$CT_Count, use= "pairwise.complete.obs")
# cor(nhs2$gwas_dx,nhs2$LT_Count, use= "pairwise.complete.obs")