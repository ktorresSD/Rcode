curr <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_2_2019_data/Scored_measures/pcl5_current_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))
life <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_2_2019_data/Scored_measures/pcl5_lifetime_scored_data_export.csv',header=T, na.strings=c("#N/A",NA))
audit <- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_2_2019_data/Scored_measures/AUDIT_scored_data_export.csv',header=T,na.strings=c("#N/A",NA))


par(mfrow=c(1,1))
#________________________________________________________________________________________  
# MERGE DATASETS TOGETHER
#------------------------------------------------------------------------
#merge CPRS corefile and full dataset by assesstment id % LAST NAME
dat1 <- merge(curr, life, by="assessment_id", all = TRUE)
#newdata <- na.omit(dat)
dim(dat1) 


plot(dat1$pcl_total.x, dat1$pcl_total.y, xlab= "Current PCL", ylab= "Lifetime PCL", main = "PCL Total Scores",  pch=19 , col = "blue")

#regression of lifetime and current
reg <- lm(dat1$pcl_total.x~ dat1$pcl_total.y)
summary(reg)
abline(reg)

#correlation among these two
cor.test(dat1$pcl_total.x, dat1$pcl_total.y, na.rm= TRUE)

#________________________________________________________________________________________  
# AUDIT
#----------------------------------------------------------------------------------------
dat000au <- merge(dat1, audit, by="assessment_id", all = FALSE)
dim(dat000au) 



#PCL Lifetime and AUDIT
plot(dat000au$pcl_total.y, dat000au$audit_total_score_use, xlab= "Lifetime PCL", ylab= "AUDIT", main = "Lifetime PCL and AUDIT Total Scores", col = "black")
abline(h = 8, lty = 2, lwd=2, col="red")
abline(v = 33, lty = 2, lwd=2, col= "blue")
legend('topleft',legend=c("AUDIT Cut-off", "PCL Cuf-off"), col = c("red", "blue"),lty = 2, lwd=2)

#PCL Lifetime and AUDIT
plot(dat000au$pcl_total.x, dat000au$audit_total_score_use, xlab= "Current PCL", ylab= "AUDIT", main = "Current PCL and AUDIT Total Scores", col = "black")
abline(h = 8, lty = 2, lwd=2, col="red")
abline(v = 33, lty = 2, lwd=2, col= "blue")
legend('topleft',legend=c("AUDIT Cut-off", "PCL Cuf-off"), col = c("red", "blue"),lty = 2, lwd=2)


#Current PCL total Score and gad8
#computes correlation between a dicotomous variable and a continous variable
library("ltm")
biserial.cor(dat000au$audit_total_score_use, as.factor(dat000au$pcl_5_dsm.x), use = c("complete.obs"), level = 1)


#plot PTSD case/controls VS AUDIT TOTAL SCORE
p0 <- hist(subset(dat000au,pcl_5_dsm.x == 1)$audit_total_score_use,plot=FALSE)
p1 <- hist(subset(dat000au,pcl_5_dsm.x == 0)$audit_total_score_use,plot=FALSE)
yli <- max(p0$density, p1$density  ) #p3$density,p4$density 
transparency_level=0.5
par(mar=c(5, 4, 4, 2) + 0.5)
plot(p0, col=rgb(1,0,0,transparency_level),freq=FALSE,xlim=range(dat000au$audit_total_score_use,na.rm=T),ylim=c(0,yli),ylab="Frequency", xlab="Audit Total Score",cex.axis=1.45,cex.lab=1.6,  main= "Audit Total Score Frequency") 
plot(p1, col=rgb(0,0,1,transparency_level),freq=FALSE,add=T)  # second
legend('topright',legend=c("Current PTSD Case", "Current PTSD Control"),col=c(rgb(1,0,0,transparency_level),rgb(0,0,1,transparency_level)),pch=c(19,19))

View(dat000au[,c("assessment_id" , "pcl_total.x", "audit_total_score_use", "pcl_total.x")])

library("ggplot2")

#violin plot
p <- ggplot(dat000au, aes(x=as.factor(pcl_5_dsm.x), y=audit_total_score_use, fill=as.factor(pcl_5_dsm.x))) + 
  geom_violin(trim=FALSE) +
  stat_summary(fun.y=median, geom="point", shape=23, size=2)

p + labs(x = "PTSD_DX", y= "AUDIT_Total_Score", fill = "PTSD DX", title="Current PTSD DX and Audit Total Score")

#Current PCL total Score and gad7
cor.test(dat000au$pcl_total.x, dat000au$audit_total_score_use, method="pearson") 

#Lifetime PCL total Score and gad7
cor.test(dat000au$pcl_total.y, dat000au$audit_total_score_use, method="pearson") 
