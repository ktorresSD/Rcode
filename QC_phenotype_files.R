setwd("~/GWAS_Freeze2_CURRENT/000_p2_files")
args = commandArgs(trailingOnly=TRUE)

study <- args[1] 

#study <- 'betr_updated_20190424' 


#study='mrsc'

library(psych)
dat <- read.csv(paste('p2_', study,'.csv',sep=''),stringsAsFactors=F,header=T,na.strings=c(NA,"#N/A","-9"))

print("Data dimension:")
 dim(dat) #Check that number of rows is approximately correct

##Check Score ranges 
#If any of these don't work, headers aren't uniform across files! Should also be chagned

print("Score ranges for dichotomous variables are:")
print("Case Status")
 try(table(dat$Case,useNA="ifany"),silent=TRUE)
print("Current PTSD Status")
 try(table(dat$Current_PTSD_Dx,useNA="ifany"),silent=TRUE)
print("Lifetime PTSD Status")
 try(table(dat$Lifetime_PTSD_Dx,useNA="ifany"),silent=TRUE)
print("Exposure")
 try(table(dat$Exposure,useNA="ifany"),silent=TRUE)
print("Sex")
 try(table(dat$Sex,useNA="ifany"),silent=TRUE)
print("Ancestry")
 try(table(dat$Self_report_ancestry,useNA="ifany"),silent=TRUE)

print("Descriptions of Continuous variables")
 describe(subset(dat,select=c(Age,CT_Count,LT_Count,IT_Count,Lifetime_PTSD_Continuous,Current_PTSD_Continuous)))

#Compare case variable to lifetime PTSD and current PTSD
print("Compare case dx variable to current PTSD dx variable")
 try(table(dat$Case,dat$Current_PTSD_Dx,useNA="ifany"),silent=TRUE)

print("Compare case dx variable to lifetime PTSD dx variable")
 try(table(dat$Case,dat$Lifetime_PTSD_Dx,useNA="ifany"),silent=TRUE)


setwd("~/GWAS_Freeze2_CURRENT/000-Phenotype_2_QC_Adam")


#________________________________________________________________________________________ 
##Histograms of data
#----------------------------------------------------------------------------------------

#Current PTSD cont stratified by current DX
if(!all(is.na(dat$Current_PTSD_Continuous)) & !all(is.na(dat$Current_PTSD_Dx) ))
{
pdf(paste(study,'_hist_Current_PTSD_Continuous.pdf'),7,7)
 p0 <- hist(subset(dat, Current_PTSD_Dx == 1)$Current_PTSD_Continuous, breaks = 6, plot=FALSE)
 p1 <- hist(subset(dat, Current_PTSD_Dx == 2)$Current_PTSD_Continuous, breaks = 6, plot=FALSE)
 
 yli <- max(p0$density, p1$density  ) #p3$density,p4$density 
 transparency_level=0.5
 par(mar=c(5, 4, 4, 2) + 0.5)
 plot(p0, col=rgb(1,0,0,transparency_level),freq=FALSE,xlim=range(dat$Current_PTSD_Continuous,na.rm=T),ylim=c(0,yli),ylab="Frequency", xlab="Current_PTSD_Continuous",main="",cex.axis=1.45,cex.lab=1.6) 
 plot(p1, col=rgb(0,0,1,transparency_level),freq=FALSE,xlim=range(dat$Current_PTSD_Continuous,na.rm=T),ylim=c(0,yli), add=T)  # second
 legend('topright',legend=c("Control","Case"),col=c(rgb(1,0,0,transparency_level),rgb(0,0,1,transparency_level)),pch=c(19,19))
dev.off()
 
 }
#Lifetime stratified by lifetime dx
if(!all(is.na(dat$Lifetime_PTSD_Continuous)) & !all(is.na(dat$Lifetime_PTSD_Dx)))
{
 pdf(paste(study,'_hist_Lifetime_PTSD_Continuous.pdf'),7,7)

 p0 <- hist(subset(dat, Lifetime_PTSD_Dx == 1)$Lifetime_PTSD_Continuous,plot=FALSE)
 p1 <- hist(subset(dat, Lifetime_PTSD_Dx == 2)$Lifetime_PTSD_Continuous,plot=FALSE)
 
 yli <- max(p0$density, p1$density  ) #p3$density,p4$density 
 transparency_level=0.5
 par(mar=c(5, 4, 4, 2) + 0.5)
 plot(p0, col=rgb(1,0,0,transparency_level),freq=FALSE,xlim=range(dat$Lifetime_PTSD_Continuous,na.rm=T),ylim=c(0,yli),ylab="Frequency", xlab="Lifetime_PTSD_Continuous",main="",cex.axis=1.45,cex.lab=1.6) 
 plot(p1, col=rgb(0,0,1,transparency_level),freq=FALSE,add=T)  # second
 legend('topright',legend=c("Control","Case"),col=c(rgb(1,0,0,transparency_level),rgb(0,0,1,transparency_level)),pch=c(19,19))
dev.off()
}

#Trauma stratified by case status
if(!all(is.na(dat$CT_Count)) )
{
pdf(paste(study,'_hist_CT_by_case.pdf'),7,7)
 p0 <- hist(subset(dat, Case == 1)$CT_Count,plot=FALSE, breaks=6)
 p1 <- hist(subset(dat, Case == 2)$CT_Count,plot=FALSE, breaks=6)
 
 yli <- max(p0$density, p1$density  ) #p3$density,p4$density 
 transparency_level=0.5
 par(mar=c(5, 4, 4, 2) + 0.5)
 plot(p0, col=rgb(1,0,0,transparency_level), freq=FALSE,xlim=range(dat$CT_Count,na.rm=T),ylim=c(0,yli),ylab="Frequency", xlab="CT_Count",main="",cex.axis=1.45,cex.lab=1.6) 
 plot(p1, col=rgb(0,0,1,transparency_level), freq=FALSE,xlim=range(dat$CT_Count,na.rm=T),ylim=c(0,yli), add=T)  # second
 legend('topright',legend=c("Control","Case"),col=c(rgb(1,0,0,transparency_level),rgb(0,0,1,transparency_level)),pch=c(19,19))
 dev.off() 
} else print("No CT variable, won't plot histograms")

 if(!all(is.na(dat$IT_Count)) )
{
pdf(paste(study,'_hist_IT_by_case.pdf'),7,7)
 p0 <- hist(subset(dat,Case == 1)$IT_Count,plot=FALSE)
 p1 <- hist(subset(dat, Case == 2)$IT_Count,plot=FALSE)
 
 yli <- max(p0$density, p1$density  ) #p3$density,p4$density 
 transparency_level=0.5
 par(mar=c(5, 4, 4, 2) + 0.5)
 plot(p0, col=rgb(1,0,0,transparency_level),freq=FALSE,xlim=range(dat$IT_Count,na.rm=T),ylim=c(0,yli),ylab="Frequency", xlab="IT_Count",main="",cex.axis=1.45,cex.lab=1.6) 
 plot(p1, col=rgb(0,0,1,transparency_level),freq=FALSE,add=T)  # second
 legend('topright',legend=c("Control","Case"),col=c(rgb(1,0,0,transparency_level),rgb(0,0,1,transparency_level)),pch=c(19,19))
 dev.off() 
}else print("No IT variable, won't plot histograms")

if(!all(is.na(dat$LT_Count)) )
{
pdf(paste(study,'_hist_LT_by_case.pdf'),7,7)
 p0 <- hist(subset(dat,Case == 1)$LT_Count,plot=FALSE)
 p1 <- hist(subset(dat, Case == 2)$LT_Count,plot=FALSE)
 
 yli <- max(p0$density, p1$density  ) #p3$density,p4$density 
 transparency_level=0.5
 par(mar=c(5, 4, 4, 2) + 0.5)
 plot(p0, col=rgb(1,0,0,transparency_level),freq=FALSE,xlim=range(dat$LT_Count,na.rm=T),ylim=c(0,yli),ylab="Frequency", xlab="LT_Count",main="",cex.axis=1.45,cex.lab=1.6) 
 plot(p1, col=rgb(0,0,1,transparency_level),freq=FALSE,add=T)  # second
 legend('topright',legend=c("Control","Case"),col=c(rgb(1,0,0,transparency_level),rgb(0,0,1,transparency_level)),pch=c(19,19))
 dev.off()
 } else print("No LT variable, won't plot histograms")
 
#Mean trauma per case status

print("Compare LT across case/control")
try(t.test(subset(dat,Case == 1)$LT_Count,subset(dat,Case == 2)$LT_Count,paired=F),silent=TRUE)
print("Compare CT across case/control")
try(t.test(subset(dat,Case == 1)$CT_Count,subset(dat,Case == 2)$CT_Count,paired=F),silent=TRUE)
print("Compare IT across case/control")
try(t.test(subset(dat,Case == 1)$IT_Count,subset(dat,Case == 2)$IT_Count,paired=F),silent=TRUE)




#________________________________________________________________________________________ 
#Regression of PTSD on trauma measures

#----------------------------------------------------------------------------------------

print("Regression of Current PTSD on LT")
if(!all(is.na(dat$Current_PTSD_Continuous)) & !all(is.na(dat$LT_Count) ) )
{
 summary(lm(Current_PTSD_Continuous ~ LT_Count,data=dat))
} else print ("Missing either current PTSD or LT. No regression!")

print("Regression of Current PTSD on CT")
if(!all(is.na(dat$Current_PTSD_Continuous)) & !all(is.na(dat$CT_Count) ) )
{
 summary(lm(Current_PTSD_Continuous ~ CT_Count,data=dat))
} else print ("Missing either current PTSD or CT. No regression!")


print("Regression of Current PTSD on IT")
if(!all(is.na(dat$Current_PTSD_Continuous)) & !all(is.na(dat$IT_Count) ) )
{
 summary(lm(Current_PTSD_Continuous ~ IT_Count,data=dat))
} else print ("Missing either current PTSD or IT. No regression!")


print("Regression of Lifetime PTSD on LT")
if(!all(is.na(dat$Lifetime_PTSD_Continuous)) & !all(is.na(dat$LT_Count) ) )
{
 summary(lm(Lifetime_PTSD_Continuous ~ LT_Count,data=dat))
} else print ("Missing either Lifetime PTSD or LT. No regression!")

print("Regression of Lifetime PTSD on CT")
if(!all(is.na(dat$Lifetime_PTSD_Continuous)) & !all(is.na(dat$CT_Count) ) )
{
 summary(lm(Lifetime_PTSD_Continuous ~ CT_Count,data=dat))
} else print ("Missing either Lifetime PTSD or CT. No regression!")


print("Regression of Lifetime PTSD on IT")
if(!all(is.na(dat$Lifetime_PTSD_Continuous)) & !all(is.na(dat$IT_Count)) )
{
 summary(lm(Lifetime_PTSD_Continuous ~ IT_Count,data=dat))
} else print ("Missing either Lifetime PTSD or IT. No regression!")



##Dx variable


print("Regression of Current PTSD DX on LT")
if(!all(is.na(dat$Current_PTSD_Dx)) & !all(is.na(dat$LT_Count) ) )
{
 summary(glm(as.factor(Current_PTSD_Dx) ~ LT_Count,data=dat,family=binomial()))
} else print ("Missing either current PTSD or LT. No regression!")

print("Regression of Current PTSD DX on CT")
if(!all(is.na(dat$Current_PTSD_Dx)) & !all(is.na(dat$CT_Count) ) )
{
 summary(glm(as.factor(Current_PTSD_Dx )~ CT_Count,data=dat,family=binomial()))
} else print ("Missing either current PTSD or CT. No regression!")


print("Regression of Current PTSD DX on IT")
if(!all(is.na(dat$Current_PTSD_Dx)) & !all(is.na(dat$IT_Count) ) )
{
 summary(glm(as.factor(Current_PTSD_Dx) ~ IT_Count,data=dat,family=binomial()))
} else print ("Missing either current PTSD or IT. No regression!")


print("Regression of Lifetime PTSD  DX on LT")
if(!all(is.na(dat$Lifetime_PTSD_Dx)) & !all(is.na(dat$LT_Count) ) )
{
 summary(glm(as.factor(Lifetime_PTSD_Dx) ~ LT_Count,data=dat,family=binomial()))
} else print ("Missing either Lifetime PTSD or LT. No regression!")

print("Regression of Lifetime PTSD DX on CT")
if(!all(is.na(dat$Lifetime_PTSD_Dx)) & !all(is.na(dat$CT_Count) ) )
{
 summary(glm(as.factor(Lifetime_PTSD_Dx) ~ CT_Count,data=dat,family=binomial()))
} else print ("Missing either Lifetime PTSD or CT. No regression!")


print("Regression of Lifetime PTSD DX on IT")
if(!all(is.na(dat$Lifetime_PTSD_Dx)) & !all(is.na(dat$IT_Count)) )
{
 summary(glm(as.factor(Lifetime_PTSD_Dx) ~ IT_Count,data=dat,family=binomial()))
} else print ("Missing either Lifetime PTSD or IT. No regression!")



print("Regression of Case  DX on LT")
if(!all(is.na(dat$Case)) & !all(is.na(dat$LT_Count) ) )
{
 summary(glm(as.factor(Case) ~ LT_Count,data=dat,family=binomial()))
} else print ("Missing either Case or LT. No regression!")

print("Regression of Case DX on CT")
if(!all(is.na(dat$Case)) & !all(is.na(dat$CT_Count) ) )
{
 summary(glm(as.factor(Case) ~ CT_Count,data=dat,family=binomial()))
} else print ("Missing either Case or CT. No regression!")


print("Regression of Case DX on IT")
if(!all(is.na(dat$Case)) & !all(is.na(dat$IT_Count)) )
{
 summary(glm(as.factor(Case) ~ IT_Count,data=dat,family=binomial()))
} else print ("Missing either Case or IT. No regression!")




