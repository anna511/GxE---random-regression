
  ## Algorithm for fitting the NRRMs for a MET data from an augmented trial
      ## data1 is the MET data matrix 
          ## data1 should contain columns Field_ids, Hybrid_ids, and Yield 
              ## Field_ids indicate the continuous numerical identity variables for Fields/locations in MET
              ## Hybrid_ids indicate the continuous numerical identity variables for Hybrids/genotypes in MET
          ## data1 should also contain environmental indices from growing phases
          ## posPC is a vector containing column positions of environmental indices in MET data  
              ## posPC for NRRMs1.1 should be a vector/matrix of one row and p columns, where p indicates the number of
                 #environmental indices. posPC for further NRRMs is a matrix of p rows and k columns, where k=number in set     

  looplmer <- function(data1, posPC) {
    
    #base model
      baseR <- lmer(Yield ~ 1+ (1|Field_ids)+ (1|Hybrid_ids), data1)
      basesummary <- summary(baseR)
      base_AIC <- as.matrix(unlist(basesummary@AICtab[1]))
      print(base_AIC)
      base_logL <- as.matrix(unlist(basesummary@AICtab[3]))
      print(base_logL)
                                                                       
         summary1 <- list()
            
         temp <- matrix(0,length(posPC), 1)
         model1_AIC <- temp
         model1_logL <- temp
                       
         temp1 <- matrix(0,length(posPC), 2)
         model1_AICa <- temp1
         AICratioa1 <- temp1
         model1_logLa <- temp1
         logLratioa1 <- temp1
                                        
   #NRRMs using one index
      for (i in c(1:length(posPC))){
        model1 <- lmer(Yield ~ 1+ (1|Field_ids) + (1+ data1[,posPC[i]]|Hybrid_ids), data1)
        summary1[[i]] <- summary(model1)
        model1_AIC[i,] <- as.matrix(unlist(summary1[[i]]@AICtab[1]))
        AICRatio1[i,] <- base_AIC/model1_AIC[i,]
        model1_logL[i,] <- as.matrix(unlist(summary1[[i]]@AICtab[3])) 
        logLratio1[i,] <- base_logL/model1_logL[i,]                
        }  
        
            model1_AICa[,1] <- posPC
            model1_AICa[,2] <- model1_AIC
            model1_AICsort <- model1_AICa[order(model1_AICa[,2]),]
                          
            model1_logLa[,1] <- posPC
            model1_logLa[,2] <- model1_logL
            model1_logLsort <- model1_logLa[order(model1_logLa[,2]),]
        

  Results = new.env()
  Results$a <- model1_AICsort
  Results$b <- model1_logLsort
       
  return(Results)  

  }
  



  
    



