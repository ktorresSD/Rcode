# -----------------------------------------
# Author: Katy Torres
# Project start: 9/28/2022
# Last edited on: 10/17/2022 8:43am pm PST by Katy Torres

# Dataset: list of failures and the z score for each month.

# Purpose: 
    ## Code runs through each failure in dataset and checks if it meets criteria based on the 4 western Electric rules
    ## Western Electric rule 1 - point falls outside 3 sigma limits
    ## Western Electric rule 2 - 2 consecutive points fall outside 2 sigma limits
    ## Western Electric rule 3 - 4 consecutive points fall beyond 1 sigma limits
    ## Western Electric rule 4 - 9 consecutive points fall on one side of the centerline
    ## Western Electric rule 5 - 6 consecutive points fall outside 2 sigma limits
    
    ##The function returns a list of failures that meet one of the rules for the latest month.

#Extra: Want to create plots of each failure showing which rule is violated and when. Want to append these visuals to some sort of document

# -----------------------------------------


#Things to add
# -----------------------------------------
#LOOP RETURN: #which rule was violated

#PLOT:
  #figure out months in axis

#STORE PLOTS:
#Want to append these visuals to some sort of document

  

# -----------------------------------------
# LOAD DATA
# -----------------------------------------
setwd("~/1_Western Electric Rules_R")
library(readr)
zscore <- read_csv("zscore2.csv")

# -----------------------------------------
#  LOOP EACH MODULE/COMPONENT THROUGH FUNCTION TO GET FREQUENCIES
# -----------------------------------------
#create dataframes to store output into
subset <- NULL
result_list <- data.frame()

#get unique list of failures
all_failures <- unique(zscore$`Failure Code Description`)

#document to store all plots
pdf("testing_several_plots.pdf")

#Loop through each row in dataset and group by failure
      for(j in 1:length(all_failures)){
      # -----------------------------------------
      #  Make subset of all failures with a specific failure
      # -----------------------------------------
      print(paste("failure:", all_failures[j]))
      subset <- zscore[which(zscore$`Failure Code Description` == all_failures[j]),]
      print(subset)
      
      
      #subset <- zscore[which(zscore$`Failure Code Description` == "BC02 - Barrel Clamp Assy- Corrosion"),]
      #examples of failures to try as use cases
      # -----------------------------------------
      #BZ99 - Bezel Assy- Sensor Clip Damage #no variation
      #CB07 - Comm Board- Disconnected #none meet criteria
      #CB05 - Comm Board- Rear Panel Damaged/Cracked #rule 2 met
      #BC02 - Barrel Clamp Assy- Corrosion #rule 2, 3 and 4 met
    
      # -----------------------------------------
      #  Code each western electric rule using indexes and plot with colors for each rule it meets
      # -----------------------------------------
      zz1<- as.Date(subset$`Start of Month`, format = "%m/%d/%Y")
      zz<- subset$`Z-Score`
      zz.s <- sd(subset$`Z-Score`)
      
      
      #initial plot
      plot(zz,ylim=c(-4*zz.s,4*zz.s),type="b", pch=16, main= paste(subset$`Failure Code Description`[1]), ylab= 'z-scores', xlab = 'Last 9 Months ( 9 = last month)')
      abline(h=0)
      abline(h=-3*zz.s,col="red3",lty=2)
      abline(h=3*zz.s,col="red3",lty=2)
      abline(h=2*zz.s,col="darkorange2",lty=2)
      abline(h=-2*zz.s,col="darkorange2",lty=2)
      abline(h=zz.s,col="darkgoldenrod1",lty=2)
      abline(h=-zz.s,col="darkgoldenrod1",lty=2)
      legend("topleft", legend=c("WE Rule 1", "WE Rule 2", "WE Rule 3", "WE Rule 4", "WE Rule 5"),
             col=c("red", "green", "blue", "orange", "purple"), lty=1,lwd=3, cex=0.8)
      text(zz,labels = zz,  cex = 0.6, pos = 3, col = "black")
      mysubtitle = "Horizontal lines are std dev (1 sigma, 2 sigma, 3 sigma)"
      mtext(side=3, line=0.5, at=3.75, adj=0, cex=0.7, mysubtitle)
      
      # # #MONTH names in X axis, but the indexes stop working (need for coloring WE rules) if we add them in
      # plot(zz1, zz,ylim=c(-4*zz.s,4*zz.s),type="l", xaxt = "n", main= 'Western Electric Rules', sub = "Please note that the horizontal lines are std dev (1,2,3)", ylab= 'z-scores', xlab = 'Month/Year')
      # axis(1, zz1, format(zz1, "%b %y"), cex.axis = .7)
      # abline(h=0)
      # abline(h=-3*zz.s,col="red3")
      # abline(h=3*zz.s,col="red3")
      # abline(h=2*zz.s,col="darkorange2",lty=2)
      # abline(h=-2*zz.s,col="darkorange2",lty=2)
      # abline(h=zz.s,col="darkgoldenrod1",lty=2)
      # abline(h=-zz.s,col="darkgoldenrod1",lty=2)
      # legend(1, 1, legend=c("WE Rule 1", "WE Rule 2", "WE Rule 3", "WE Rule 4"),
      # col=c("red", "green", "blue", "orange"), lty=1, cex=0.8)
      
      
      ## Western Electric rule 1
      # -----------------------------------------
      ## point falls outside 3 sigma limits
      zz.rule1 <- abs(zz) > 3*zz.s
      zz.rule1.rle <- rle(as.vector(zz.rule1))
      zz.rule1.warn <- which(zz.rule1.rle$lengths >= 1 
                             & zz.rule1.rle$values == TRUE)
      
      
      ## Western Electric rule 2
      # -----------------------------------------
      ## 2 consecutive points fall outside 2 sigma limits
      zz.rule2 <- abs(zz) > 2*zz.s
      zz.rule2.rle <- rle(as.vector(zz.rule2))
      zz.rule2.warn <- which(zz.rule2.rle$lengths >= 2 
                             & zz.rule2.rle$values == TRUE)
      zz.rule2.lastmonth <- if(zz.rule2[9] == TRUE && zz.rule2.rle$values == TRUE){
        zz.rule2.lastmonth<- TRUE}else{zz.rule2.lastmonth <-FALSE}
      
      ## Western Electric rule 3
      # -----------------------------------------
      ## 4 consecutive points fall beyond 1 sigma limits
      zz.rule3 <- abs(zz) > zz.s
      zz.rule3.rle <- rle(as.vector(zz.rule3))
      zz.rule3.warn <- which(zz.rule3.rle$lengths >= 4
                             & zz.rule3.rle$values == TRUE)
      
      ## Western Electric rule 4
      # -----------------------------------------
      ## 9 consecutive points fall on one side of the centerline
      zz.rule4 <- zz > 0
      zz.rule4.rle <- rle(as.vector(zz.rule4))
      zz.rule4.warn <- which(zz.rule4.rle$lengths >= 9)
      
      ## Western Electric rule 5
      # -----------------------------------------
      ## 6 consecutive points fall outside 2 sigma limits
      zz.rule5 <- abs(zz) > 2*zz.s
      zz.rule5.rle <- rle(as.vector(zz.rule5))
      zz.rule5.warn <- which(zz.rule5.rle$lengths >= 6 
                             & zz.rule5.rle$values == TRUE)
      
      # Color original plot with rules met
      # -----------------------------------------
      ## function takes the rle object, and the warning indexes, plots the right points and returns whh rule is violated
      
      plot.warn.x <- function(tsobj, rleobj, ind, col,...) {
        if(length(ind) <= 0) return()
        ## need to convert lengths back to indexes
        ind.x <- c(1,cumsum(rleobj$lengths)+1)
        
        for(i in seq(along=ind)) {
          x.coords <- ind.x[ind[i]]:(ind.x[ind[i]+1]-1)
          lines(x.coords + start(tsobj)[1]-1,tsobj[x.coords],col=col,...)
          #if('9' %in% x.coords){print(TRUE)}else{print(FALSE)} # cant figure out how to look at only 9th one being true here
          if('9' %in% x.coords){return(TRUE)}else{return(FALSE)} # cant figure out how to look at only 9th one being true here
          
          }
      }
      we.5 <- plot.warn.x(zz, zz.rule5.rle, zz.rule5.warn, "purple", lwd=3)  
      we.4 <-plot.warn.x(zz, zz.rule4.rle, zz.rule4.warn, "orange", lwd=3)
      we.3 <-plot.warn.x(zz, zz.rule3.rle, zz.rule3.warn, "blue", lwd=3)
      we.2 <-plot.warn.x(zz, zz.rule2.rle, zz.rule2.warn, "green", lwd=3)
      we.1 <-plot.warn.x(zz, zz.rule1.rle, zz.rule1.warn, "red", lwd=3)
      
      # #Return the failure name for any failure meeting any of the WE rule in the last month
      # #-----------------------------------------
      # #if any of the criteria is met, return the failure code
       if(!is.null(we.1) && we.1 == TRUE || !is.null(we.2) && we.2 == TRUE || !is.null(we.3) && we.3 == TRUE || !is.null(we.4) && we.4 == TRUE || !is.null(we.5) && we.5== TRUE){
         print(paste("Failure ", subset$`Failure Code Description`[1], " meets criteria for last month."))
         output <- subset$`Failure Code Description`[1]
         print(output)
         result_list <- rbind(result_list, output)
       } else{}
      }

dev.off()
