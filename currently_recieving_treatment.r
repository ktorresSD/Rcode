
#fuction that creates columns about treatment
diag_treated0 <- function(x){
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  #Currently being treated for a mental health issue?, call it "curr_treatment"
  #if they answered Yes to any of the disgnoses being treated or yes to being treated, it will be coded as a yes
  if(!((is.na(CurrTx2a_Dep))|(is.na(CurrTx2b_Anx))| (is.na(CurrTx2c_PTSD))|(is.na(CurrTx2d_Schiz))|(is.na(CurrTx2e_BP))|(is.na(CurrTx2f_Subst))|(is.na(CurrTx2g_Other)))){
    if(CurrTx2a_Dep==1 | CurrTx2b_Anx==1 |CurrTx2c_PTSD==1 | CurrTx2d_Schiz==1 |CurrTx2e_BP==1 | CurrTx2f_Subst==1 | CurrTx2g_Other==1)
    {diagnosis_treated <- 1}
    else{diagnosis_treated <- 0}
  }else{diagnosis_treated<-NA}
  
  
  if (is.na(diagnosis_treated) & is.na(CurrentTreatments1_MHTx) )
  {
    curr_treatment <- NA
  } else if ( diagnosis_treated == 1 & is.na(CurrentTreatments1_MHTx)  )
  {
    curr_treatment <- 1
  } else if ( is.na(diagnosis_treated) & CurrentTreatments1_MHTx == 1  )
  {
    curr_treatment <- 1
  } else if ( diagnosis_treated == 1 & CurrentTreatments1_MHTx == 1  )
  {
    curr_treatment <- 1
  } else if ( diagnosis_treated == 0 & is.na(CurrentTreatments1_MHTx)  )
  {
    curr_treatment <- 0
  } else if ( is.na(diagnosis_treated) & CurrentTreatments1_MHTx == 0  )
  {
    curr_treatment <- 0
  } else if ( diagnosis_treated == 0 & CurrentTreatments1_MHTx == 0  )
  {
    curr_treatment <- 0
  } else if ( diagnosis_treated == 0 & CurrentTreatments1_MHTx == 1  )
  {
    curr_treatment <- 1
  } else if ( diagnosis_treated == 1 & CurrentTreatments1_MHTx == 0 )
  {
    curr_treatment <- 1
  } else {curr_treatment <- NA}
  
  
  
  #Currently recieve medications as part of their treatment
  #treated_medication, if they answered yes to any medication
  if(!((is.na(CurrTx5a_AntiD))|(is.na(CurrTx5b_Mood))| (is.na(CurrTx5c_Stim))|(is.na(CurrTx5d_Sleep))|(is.na(CurrTx5e_Benzo))|(is.na(CurrTx5f_AntiPsy))|(is.na(CurrTx5g_Adren))|(is.na(CurrTx5h_Other)))){
    if(CurrTx5a_AntiD==1 | CurrTx5b_Mood==1 |CurrTx5c_Stim==1 | CurrTx5d_Sleep==1 |CurrTx5e_Benzo==1 | CurrTx5f_AntiPsy==1 | CurrTx5g_Adren==1| CurrTx5h_Other==1)
    {
      treated_medication <- "Yes_meds"}else{treated_medication <- "No_meds"}
  }else{treated_medication<-"No_meds"}
  
  
  
  #Currently recieve psychotherapy as part of their treatment
  #treated_talk, if they said yest to currently being treated or said yes to recieving a specific Phsychotherapy
  
  if (is.na(treated_psych) & is.na(curr_psychor) )
  {
    treated_talk <- NA
  } else if (treated_psych == 1 & is.na(curr_psychor)  )
  { 
    treated_talk <- 1
  } else if (is.na(treated_psych) & curr_psychor == 1  )
  {
    treated_talk <- 1
  } else if (treated_psych == 1 & curr_psychor == 1  )
  {
    treated_talk <- 1
  } else if (treated_psych == 0 & is.na(curr_psychor)  )
  {
    treated_talk <- 0
  } else if (is.na(treated_psych) & curr_psychor == 0  )
  {
    treated_talk <- 0
  } else if (treated_psych == 0 & curr_psychor == 0  )
  {
    treated_talk <- 0
  } else if (treated_psych == 0 & curr_psychor == 1  )
  {
    treated_talk <- 1
  } else if (treated_psych == 1 & curr_psychor == 0 )
  {
    treated_talk <- 1
  } else {treated_talk <- NA}
  
  
  #both_treatments, if they are currently recieving either medications or psychotherapy
  if(!((is.na(treated_medication))|(is.na(treated_talk)))){
    if((treated_medication== "Yes_meds") & (treated_talk == 1)){
      both_treatments <- "Yes_both" }else{both_treatments<- "Not_both"}
  }else{both_treatments<-NA}
  
  treat <- data.frame( curr_treatment, treated_talk, treated_medication, both_treatments)
  return(treat)
}
