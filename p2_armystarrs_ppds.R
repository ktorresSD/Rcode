#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Child Trauma data and scoring functions
##########################################################################################

#To the user: Set path to where data is stored
setwd("~/Documents/GWAS_Freeze2_CURRENT/14_15_16_ArmySTARRS/pheno_2")
#________________________________________________________________________________________
#READ AND SUBSET LARGE DATA TO ONLY CONTAIN DESIRED QUESTIONAIRE VARIABLES
#----------------------------------------------------------------------------------------
#Read all data
#stringsAsFactors=FALSE,
datchild <- read.csv('C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/14_15_16_ArmySTARRS/pheno_2/pgcid_ppds_data_wpsw1.csv',header=T,  na.strings=c(NA,999))

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
#Scoring Childhood trauma function defined
abuse.score <- function(x)
{
  
  #attach(x)
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])

  #some of the variables are reverse scored, meaning if they answered a 1, this is really a 5
  
 #Calculating scale total scores for each category
  EA_scale <- CA_VerbalAbuse +  CA_FamilyHate +  CA_EmotionAbuse
  PA_scale <- CA_PhysicalAbuse + CA_HitBruise
  SA_scale <- CA_TouchSex + CA_SexAbuse

  
#Dichotomization of Source Variable(s)
  
#Sexual Abuse
  if(!is.na(CA_TouchSex)){
    if(CA_TouchSex >=2)  { CA_TouchSex_di <- 1} 
    else{ CA_TouchSex_di<- 0} 
  }
  else { CA_TouchSex_di<- NA }
  
  if(!is.na(CA_SexAbuse)){
    if(CA_SexAbuse >=2)  { CA_SexAbuse_di <- 1} 
    else{ CA_SexAbuse_di<- 0} 
  }
  else { CA_SexAbuse_di<- NA }
  
#Pysical Abuse
  if(!is.na(CA_PhysicalAbuse)){
    if(CA_PhysicalAbuse >=3)  { CA_PhysicalAbuse_di <- 1} 
    else{CA_PhysicalAbuse_di<- 0} 
  }
  else {CA_PhysicalAbuse_di<- NA }
  
  if(!is.na(CA_HitBruise)){
    if(CA_HitBruise >=3)  { CA_HitBruise_di <- 1} 
    else{CA_HitBruise_di<- 0} 
  }
  else {CA_HitBruise_di<- NA }
  
#Emotional Abuse
  
  if(!is.na(CA_VerbalAbuse)){
    if(CA_VerbalAbuse >=4)  {CA_VerbalAbuse_di <- 1} 
    else{CA_VerbalAbuse_di<- 0} 
  }
  else {CA_VerbalAbuse_di<- NA }
  
  if(!is.na(CA_FamilyHate)){
    if(CA_FamilyHate >=4)  {CA_FamilyHate_di <- 1} 
    else{CA_FamilyHate_di<- 0} 
  }
  else {CA_FamilyHate_di<- NA }
  
  if(!is.na(CA_EmotionAbuse)){
    if(CA_EmotionAbuse >=3)  {CA_EmotionAbuse_di <- 1} 
    else{CA_EmotionAbuse_di<- 0} 
  }
  else {CA_EmotionAbuse_di<- NA }  

  #
  #return back newly calculated scores
      

#Derived variable
if(CA_TouchSex_di==1 | CA_SexAbuse_di==1){
  SexualAbuse <-1 }else{SexualAbuse <-0}

if(CA_PhysicalAbuse_di==1 | CA_HitBruise_di==1){
  PhysicalAbuse <- 1}else{PhysicalAbuse <- 0}

if(CA_VerbalAbuse_di==1 | CA_FamilyHate_di==1 | CA_EmotionAbuse_di==1){
  EmotionalAbuse <- 1}else{EmotionalAbuse <- 0}

  
CT_Count <- SexualAbuse + PhysicalAbuse + EmotionalAbuse 

childcores <- data.frame(SexualAbuse, PhysicalAbuse, EmotionalAbuse, EA_scale, PA_scale, SA_scale,  
                          CA_TouchSex_di, CA_SexAbuse_di, CA_PhysicalAbuse_di, CA_HitBruise_di,
                          CA_VerbalAbuse_di, CA_FamilyHate_di, CA_EmotionAbuse_di, CT_Count   )

return(childcores)
}
#Calculate summary scores in data

abuse_scores <- adply(datchild, 1, abuse.score)



#Scoring Lifetime trauma function defined
#________________________________________________________________________________________ 

#Find and replace all factor and make them 1 if it ocurred and 0 if it did not
abuse_scores[abuse_scores=="0"]<-0
abuse_scores[abuse_scores=="1"]<-1
abuse_scores[abuse_scores=="2-4"]<-1
abuse_scores[abuse_scores=="5-9"]<-1
abuse_scores[abuse_scores=="10ormore"]<-1

abuse_scores$lt1<- as.numeric(abuse_scores$lifestress_physicalassault) -1 
abuse_scores$lt2<- as.numeric(abuse_scores$lifestress_sexualassault) -1 
# abuse_scores$lt3<- as.numeric(abuse_scores$lifestress_assault_friend) -1 
abuse_scores$lt4<- as.numeric(abuse_scores$lifestress_murder_friend) -1 
abuse_scores$lt5<- as.numeric(abuse_scores$lifestress_suicide_friend) -1 
# abuse_scores$lt6<- as.numeric(abuse_scores$lifestress_suicatt_friend) -1 
abuse_scores$lt7<- as.numeric(abuse_scores$lifestress_comdeath_friend) -1 
abuse_scores$lt8<- as.numeric(abuse_scores$lifestress_accdeath_friend) -1 
abuse_scores$lt9<- as.numeric(abuse_scores$lifestress_witnessdeath) -1 
abuse_scores$lt10<- as.numeric(abuse_scores$lifestress_handledeadbody) -1 
abuse_scores$lt11<- as.numeric(abuse_scores$lifestress_lifethreateningillne) -1 
abuse_scores$lt12<- as.numeric(abuse_scores$lifestress_indisaster) -1 
abuse_scores$lt13<- as.numeric(abuse_scores$lifestress_other) -1 
# abuse_scores$lt14<- as.numeric(abuse_scores$lifestress_bully) -1 
# abuse_scores$lt15<- as.numeric(abuse_scores$lifestress_loved) -1 


#Scoring lifetime trauma function defined
lt_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  if(all(is.na(c(lt1,lt2,lt4,lt5,lt7,lt8,lt9,lt10,lt11,lt12,lt13))))
  {LT_Count <- NA} else{  
  LT_Count<- sum(c(lt1,lt2,lt4,lt5,lt7,lt8,lt9,lt10,lt11,lt12,lt13), na.rm=TRUE)}
  
  Current_PTSD_Continuous<- Sev_Reexp	+ Sev_Avoid	+ Sev_Hyper
  
  ltcores <- data.frame(LT_Count, Current_PTSD_Continuous)
  
  return(ltcores)
}

#Calculate summary scores in data
lt_scores <- adply(abuse_scores, 1, lt_score)


lt_scores$Exposure <- lt_scores$trauma_exposed_critA

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv(lt_scores, "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/14_15_16_ArmySTARRS/pheno_2/p2_ppds20190107.csv",quote=T,row.names=F,na="NA")


