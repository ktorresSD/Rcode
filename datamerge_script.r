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

noGWASinfo<- data_merged[is.na(data_merged$gwas_iid),]
dim(noGWASinfo) #16514
noGWASinfo_n<- table(noGWASinfo$study_number)
noGWASinfo_n

matched_only<- data_merged[!(is.na(data_merged$gwas_iid)),]

dim(matched_only) #72441

no_case_status<- data_merged[is.na(data_merged$Case),]
#write.csv(noGWASinfo, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/noGWASinfo.csv",quote=T,row.names=F,na="#N/A")

fam_n<- table(dat0$study_number)
matched_n<- table(matched_only$study_number)
Nsmatched<- cbind(fam_n,matched_n)
Nsmatched

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

#SUBJECTS THAT DO NOT MATCH, INVESTIGATE FURTHER
#check subjects whose DX does not match
no_match_subjects<- matched_only[matched_only$dxmatch=="no_match",]

#check subjects whose DX does not match
no_match_sex_subjects<- matched_only[matched_only$sexmatch=="no_match",]

subjects_who_do_not_match<- rbind(no_match_subjects, no_match_sex_subjects)
write.csv(subjects_who_do_not_match, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/subjects_with_match_issues.csv",quote=T,row.names=F,na="#N/A")




#Subset and export files where all items match
nhs2_5<- matched_only[matched_only$study_number==5,]
satu_11<- matched_only[matched_only$study_number==11,]
defe_12<- matched_only[matched_only$study_number==12,]
guts_21<- matched_only[matched_only$study_number==21,]
nrv2_22<- matched_only[matched_only$study_number==22,]
bry2_36<- matched_only[matched_only$study_number==36,]
feen_37<- matched_only[matched_only$study_number==37,]
ncmh_41<- matched_only[matched_only$study_number==41,]
earc_42<- matched_only[matched_only$study_number==42,]
wach_43<- matched_only[matched_only$study_number==43,]
shrs_46<- matched_only[matched_only$study_number==46,]

write.csv(nhs2_5, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/nhs2_2.csv",quote=T,row.names=F,na="-9")
write.csv(satu_11, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/satu_11.csv",quote=T,row.names=F,na="-9")
write.csv(defe_12, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/defe_12.csv",quote=T,row.names=F,na="-9")
write.csv(guts_21, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/guts_21.csv",quote=T,row.names=F,na="-9")
write.csv(nrv2_22, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/nrv2_22.csv",quote=T,row.names=F,na="-9")
write.csv(bry2_36, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/bry2_36.csv",quote=T,row.names=F,na="-9")
write.csv(feen_37, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/feen_37.csv",quote=T,row.names=F,na="-9")
write.csv(ncmh_41, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/ncmh_41.csv",quote=T,row.names=F,na="-9")
write.csv(earc_42, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/earc_42.csv",quote=T,row.names=F,na="-9")
write.csv(wach_43, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/wach_43.csv",quote=T,row.names=F,na="-9")
write.csv(shrs_46, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/output/shrs_46.csv",quote=T,row.names=F,na="-9")

# plot(nhs2$CT_Count,nhs2$gwas_dx,pch=8)
lmfit   <- lm(gwas_dx~CT_Count,nhs2)
plot(jitter(nhs2$gwas_dx),jitter(nhs2$CT_Count))

plot(nhs2$gwas_dx,nhs2$CT_Count)

cor(nhs2$LT_Count,nhs2$CT_Count, use= "pairwise.complete.obs")
cor(nhs2$gwas_dx,nhs2$CT_Count, use= "pairwise.complete.obs")
cor(nhs2$gwas_dx,nhs2$LT_Count, use= "pairwise.complete.obs")