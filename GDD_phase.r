## An algorithm to partition maize growth life into phases using 100 growing degree day (GDD) increments
    ## The algorithm can be used to find sum of GDD and phase interval
    ## The temperature used for maximum (Tmax) and minimum (Tmin) is supplied in degree Celcius. 
        ## But GDD summation is performed on Fahrenheit
    ## data1 contains daily measurements of Tmax and Tmin values throughout the crop life starting from sowing to harvesting.
        ## Tmax and Tmin should be the column names
    ## VT and R1 are the GDD values for respective stages
    ## Need to remove/replace all 'NA' in the data1 
  
GDDphase <- function(data1,VT,R1) {{ 

    #to convert degree Celcius to degree Fahrenheit#
    TmaxF <- (data1$Tmax*(9/5))+32
    TminF <- (data1$Tmin*(9/5))+32
    R <- cbind(TmaxF,TminF)

      #to set bench marks for GDD summation#
      for (i in 1:nrow(R)){
      for (j in 1:ncol(R)){
      if (R[i,j]> 86){
      replace
      (R[i,j]=86)}
      
      if (R[i,j]<50) {
      replace
      (R[i,j]=50) }
      }
      }

        #to find GDD per day#
        GDD <- matrix(0,nrow(R),1)
          for (i in 1:nrow(R)){
        GDD[i,] = mean(R[i,]) -50
        }


          #for GDD summation#
          GDDsum <- matrix(0,nrow(GDD),1)
          for (i in 1:nrow(GDD)){
          GDDsum[1,1] = GDD[1,1]
          GDDsum[i,] <- sum(GDD[i,] +GDDsum [i-1,])
          }


            #to find phase interval in crop life using an increment of 100 GDD from VT and R1 stages#
            GDDinterval <- matrix(0,nrow(GDDsum),1)
                for (i in 1:nrow(GDDsum)){
                if (GDDsum[i,] <=VT & GDDsum[i,]>(VT-100)) {
                GDDinterval [i,] = "VT"}
                if (GDDsum[i,] <=R1 & GDDsum[i,]>VT){
                GDDinterval[i,] = "R1"}
                }
            
            interval= c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500)
            
                interval[1] =100
                for (i in 1:nrow(GDDsum)){
                if (GDDsum[i,] <= (VT-interval[1]) & GDDsum[i,] > (VT-(interval[1]+100))){  
                GDDinterval[i,] = (paste("VT_",1)) }
                if (GDDsum[i,] <=(R1+interval[1]) & GDDsum[i,] > R1){ 
                GDDinterval[i,] = (paste("R1_",1))}
                }
            
                  for (j in 2:15){
                  interval[j]=interval[j-1]+100
                  for (i in 1:nrow(GDDsum)){
                  if (GDDsum[i,] <= (VT-interval[j]) & GDDsum[i,] > (VT-(interval[j]+100))){
                  GDDinterval[i,] = (paste("VT_",j))}
                  if (GDDsum[i,] <= (R1+interval[j]) & GDDsum[i,] >(R1+(interval[j]-100))){
                  GDDinterval[i,]= (paste("R1_",j))}
            }
            }

GDDdata = cbind(R, GDD, GDDsum, GDDinterval)
}
return(GDDdata)
}