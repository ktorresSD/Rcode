
#read in only the desired columns
library(data.table)
dat1 <- fread("p2_defe.csv", select = c("IID","FID","Self_report_ancestry",
                                        "Sex","Case","Age","Lifetime_PTSD_Dx","Current_PTSD_Dx","Lifetime_PTSD_Continuous",
                                        "Current_PTSD_Continuous","Exposure","CT_Count","LT_Count"))