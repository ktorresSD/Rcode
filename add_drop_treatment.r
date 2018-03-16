#CREATE THE ADD AND DROP COLUMNS
add_drop <- function(x){
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  #MEDS COLUMNS
  #Add and Drop Meds columns (visit 1 to 2)
  if(!((is.na(v1_meds_treat))|(is.na(v2_meds_treat)))){
    if((v1_meds_treat == "Yes_meds") & (v2_meds_treat == "Yes_meds")){
      addmeds_v1_2 <- "No_Change" }
    
    else if((v1_meds_treat == "No_meds") & (v2_meds_treat == "No_meds")){
      addmeds_v1_2 <- "No_Change" }
    
    else if((v1_meds_treat == "No_meds") & (v2_meds_treat == "Yes_meds")){
      addmeds_v1_2 <- "Add_Meds"
      
    }else{addmeds_v1_2 <- "0"
    }
  }else{addmeds_v1_2<-NA}
  
  if(!((is.na(v1_meds_treat))|(is.na(v2_meds_treat)))){
    if((v1_meds_treat == "Yes_meds") & (v2_meds_treat == "Yes_meds")){
      dropmeds_v1_2 <- "No_Change" }
    
    else if((v1_meds_treat == "No_meds") & (v2_meds_treat == "No_meds")){
      dropmeds_v1_2 <- "No_Change" }
    
    else if((v1_meds_treat == "Yes_meds") & (v2_meds_treat == "No_meds")){
      dropmeds_v1_2 <- "drop_Meds"
      
    }else{dropmeds_v1_2 <- "0"
    }
  }else{dropmeds_v1_2<-NA}
  
  #Add and Drop Meds columns (visit 2 to 3)
  if(!((is.na(v2_meds_treat))|(is.na(v3_meds_treat)))){
    if((v2_meds_treat == "Yes_meds") & (v3_meds_treat == "Yes_meds")){
      addmeds_v2_3 <- "No_Change" }
    
    else if((v2_meds_treat == "No_meds") & (v3_meds_treat == "No_meds")){
      addmeds_v2_3 <- "No_Change" }
    
    else if((v2_meds_treat == "No_meds") & (v3_meds_treat == "Yes_meds")){
      addmeds_v2_3 <- "Add_Meds"
      
    }else{addmeds_v2_3 <- "0"
    }
  }else{addmeds_v2_3<-NA}
  
  
  if(!((is.na(v2_meds_treat))|(is.na(v3_meds_treat)))){
    if((v2_meds_treat == "Yes_meds") & (v3_meds_treat == "Yes_meds")){
      dropmeds_v2_3 <- "No_Change" }
    
    else if((v2_meds_treat == "No_meds") & (v3_meds_treat == "No_meds")){
      dropmeds_v2_3 <- "No_Change" }
    
    else if((v2_meds_treat == "Yes_meds") & (v3_meds_treat == "No_meds")){
      dropmeds_v2_3 <- "drop_Meds"
      
    }else{dropmeds_v2_3 <- "0"
    }
  }else{dropmeds_v2_3<-NA}
  
  #TALK COLUMNS
  #Add and Drop TALK columns (VISIT 1 TO 2)
  if(!((is.na(v1_talk_treat))|(is.na(v2_talk_treat)))){
    if((v1_talk_treat == 1) & (v2_talk_treat == 1)){
      addtalk_v1_2 <- "No_Change" }
    
    else if((v1_talk_treat == 0) & (v2_talk_treat == 0)){
      addtalk_v1_2 <- "No_Change" }
    
    else if((v1_talk_treat == 0) & (v2_talk_treat == 1)){
      addtalk_v1_2 <- "Add_talk"
      
    }else{addtalk_v1_2 <- "0"
    }
  }else{addtalk_v1_2<-NA}
  
  if(!((is.na(v1_talk_treat))|(is.na(v2_talk_treat)))){
    if((v1_talk_treat == 1) & (v2_talk_treat == 1)){
      droptalk_v1_2 <- "No_Change" }
    
    else if((v1_talk_treat == 0) & (v2_talk_treat == 0)){
      droptalk_v1_2 <- "No_Change" }
    
    else if((v1_talk_treat == 1) & (v2_talk_treat == 0)){
      droptalk_v1_2 <- "drop_talk"
      
    }else{droptalk_v1_2 <- "0"
    }
  }else{droptalk_v1_2<-NA}
  
  #Add and Drop TALK columns (VISIT 2 TO 3)
  if(!((is.na(v2_talk_treat))|(is.na(v3_talk_treat)))){
    if((v2_talk_treat == 1) & (v3_talk_treat == 1)){
      addtalk_v2_3 <- "No_Change" }
    
    else if((v2_talk_treat == 0) & (v3_talk_treat == 0)){
      addtalk_v2_3 <- "No_Change" }
    
    else if((v2_talk_treat == 0) & (v3_talk_treat == 1)){
      addtalk_v2_3 <- "Add_talk"
      
    }else{addtalk_v2_3 <- "0"
    }
  }else{addtalk_v2_3<-NA}
  
  if(!((is.na(v2_talk_treat))|(is.na(v3_talk_treat)))){
    if((v2_talk_treat == 1) & (v3_talk_treat == 1)){
      droptalk_v2_3 <- "No_Change" }
    
    else if((v2_talk_treat == 0) & (v3_talk_treat == 0)){
      droptalk_v2_3 <- "No_Change" }
    
    else if((v2_talk_treat == 1) & (v3_talk_treat == 0)){
      droptalk_v2_3 <- "drop_talk"
      
    }else{droptalk_v2_3 <- "0"
    }
  }else{droptalk_v2_3<-NA}
  
  newcol<- data.frame(addmeds_v1_2, dropmeds_v1_2,addmeds_v2_3,dropmeds_v2_3, addtalk_v1_2, droptalk_v1_2, addtalk_v2_3, droptalk_v2_3)
  return(newcol)
}