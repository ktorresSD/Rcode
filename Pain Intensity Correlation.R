#Looking at PCL and Pain

#Merge in datasets
dat11 <- merge(datpain1, promis_scored, by=c("assessment_id", "vista_lastname","visit_number"), all = FALSE)
dim(dat11)

dat22 <- merge(dat11, score_datphq15, by=c("assessment_id", "vista_lastname", "visit_number"), all = FALSE)
dim(dat22)

dat000 <- merge(dat22, pcl_5_scorescurr, by=c("assessment_id", "vista_lastname", "visit_number"), all = FALSE)
dim(dat000)


#basic pain
dat000$pain_number
#Promis
dat000$pain_level
#PHQ-15
#dat000$phq15_score_total
dat000$phq14_score_males



#COMPARING BOTH PAIN INTENSITY MODULES
ggplot(dat000, aes(x = pain_number, y = pain_level)) +
  ggtitle("Pain Intensity in PROMIS and Basic Pain modules") +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)
#comparing pain scores (ordinal and ordinal) #SPEARMAN RHO
cor.test(dat000$pain_level, dat000$pain_number, method = "spearman")





#PCL and PROMIS (ordinal and continous)
plot(  dat000$pcl_incomplete, dat000$pain_level, main= "Pain Intensity in PROMIS and Current PCL Score", 
      xlab= "PCL Current Score", ylab= "PROMIS- Pain Intensity", pch= 16, col= "blue", xlim=c(0, 80), ylim=c(0, 5))
# cor.test(dat000$pain_level, dat000$pcl_incomplete, method = "pearson", use="complete.obs")
# cor.test(dat000$pain_level, dat000$pcl_incomplete, method = "spearman")
cor.test(dat000$pain_level, dat000$pcl_incomplete, method = "kendall")



#PCL and Basic Pain (ordinal and continous) 
plot(  dat000$pcl_incomplete, dat000$pain_number, main= "Current PCL Score and Basic Pain Intensity", 
       xlab= "PCL Current Score", ylab= "Basic Pain - Pain Intensity", pch= 16, col= "blue", xlim=c(0, 80), ylim=c(0, 10))
cor.test(dat000$pain_number, dat000$pcl_incomplete, use="complete.obs")
#abline(lm(dat000$pain_number~dat000$pcl_incomplete))




#PCL and PHQ15 (continous and continous)  #PEARSON CORRELATION
plot(  dat000$pcl_incomplete, dat000$phq15_score_total, main= "Current PCL Score and PHQ-15", 
       xlab= "PCL Current Score", ylab= "PHQ15 - Pain Bother", pch= 16, col= "blue", xlim=c(0, 80), ylim=c(0, 30))
cor.test(dat000$phq15_score_total, dat000$pcl_incomplete, use="complete.obs")
abline(lm(dat000$phq15_score_total~dat000$pcl_incomplete))



#output merged dataframe
write.csv(dat000, "pains.csv", row.names=F,na="NA")