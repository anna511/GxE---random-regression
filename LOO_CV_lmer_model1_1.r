  
  ## Algorithm for testing predicting performance of NRRMs using one environmental index 
      ## data1 indicates the MET dataset with columns Field_ids, Hybrid_ids, and Yield as columns 1,2,and 3
          ## Field_ids and Hybrid_ids are continuous identity variables
      ## posPC indicates a vector of index positions in the MET dataset

  cv.lmer <- function(data1,posPC){
  
  fieldlevels <- levels(factor(data1$Field_ids))
  
   train.data1 = list()
   test.data1 = list()
  
      baseR.train <- list()
      basesummary_train <- list()
      baseraneffect_train <- list()
    
      temp.baseraneffect <- list()
    
      yhatstar1_base <- list()
      cor1_yhatbase <- matrix(0,1,length(fieldlevels))
  
          model1_AIC_train = matrix(0,length(posPC[,1]), length(fieldlevels))
          Raneffect1_train = vector("list", length(fieldlevels))
      
          yhatstar1_test = vector("list", length(fieldlevels))
      
          temp.cor.1 <- vector("list", length(fieldlevels))
          temp.cor.2 <-vector("list", length(fieldlevels))
          temp.cor.3 <- vector("list", length(fieldlevels))
      
          cor1 = matrix(0,length(posPC[,1]), length(fieldlevels))
  
                
  
   for (k in c(1:length(fieldlevels))){
    test.fields <- fieldlevels[k]
    train.fields <- which(!(fieldlevels %in% test.fields))
  
      which(data1$Field_ids %in% train.fields)
      which(!(data1$Field_ids %in% train.fields))
      train.rows <- which(data1$Field_ids %in% train.fields)
      test.rows <- which(!(data1$Field_ids %in% train.fields))
  
      train.data1[[k]] <- data1[train.rows, ]
      test.data1[[k]] <- data1[test.rows, ]
  
  
  #base model
    baseR.train[[k]] <- lmer(Yield ~ 1+ (1|Field_ids) + (1|Hybrid_ids), train.data1[[k]])
    basesummary_train[[k]] <- summary(baseR.train[[k]])
    baseraneffect_train[[k]] <- ranef(baseR.train[[k]])$Hybrid_ids
  
          nHybrids <- as.numeric(levels(factor(train.data1[[k]]$Hybrid_ids)))
          temp.baseraneffect[[k]] <- cbind(matrix(nHybrids), as.matrix(baseraneffect_train[[k]]))
          colnames(temp.baseraneffect[[k]]) <- c("Hybrid_ids","Intercept")
  
          yhatstar1_base[[k]] <- merge(temp.baseraneffect[[k]], test.data1[[k]][,c(2,3)], by="Hybrid_ids")
  
          cor1_yhatbase[1,k] <- cor(as.matrix(yhatstar1_base[[k]])[,2], as.matrix(yhatstar1_base[[k]])[,3])
  
  
  #NRRMs using single index
      for (i in c(1:length(posPC[,1]))){
       model1_AIC_train[i,k] <- (unlist(summary(lmer(Yield ~ 1+ (1|Field_ids)  +
                                      (1+ train.data1[[k]][,posPC[i,1]] |Hybrid_ids),train.data1[[k]]))@AICtab[1]))                                                                                                  
       Raneffect1_train[[k]][[i]] <- matrix(unlist(ranef(lmer(Yield ~ 1+ (1|Field_ids)+
                                      (1+ train.data1[[k]][,posPC[i,1]] |Hybrid_ids),train.data1[[k]]))$Hybrid_ids),ncol=2)
  
              temp.raneff1 <- cbind(matrix(nHybrids), (Raneffect1_train[[k]][[i]]))
              colnames(temp.raneff1) <- c("Hybrid_ids","Intercept","slope1")
  
                temp.mergetest1 <- merge(temp.raneff1, test.data1[[k]][,c(2,3,posPC[i,1])], by="Hybrid_ids")
                yhatstar1_test[[k]][[i]] <-cbind(temp.mergetest1[,c(1,4)], matrix(unlist(temp.mergetest1[,2] 
                                                                + temp.mergetest1[,3]*temp.mergetest1[,5] ),ncol=1))
                colnames(yhatstar1_test[[k]][[i]]) <- c("Hybrid_ids", "Yield",  "yhatstar")
  
                   temp.cor.1[[k]][[i]] <- aggregate(yhatstar1_test[[k]][[i]][,2], list(yhatstar1_test[[k]][[i]][,1]),mean)
                   temp.cor.2[[k]][[i]] <- aggregate(yhatstar1_test[[k]][[i]][,3], list(yhatstar1_test[[k]][[i]][,1]),mean)
                   temp.cor.3[[k]][[i]] <- merge(temp.cor.1[[k]][[i]], temp.cor.2[[k]][[i]], by ="Group.1")
  
                   cor1[i,k] <- cor(temp.cor.3[[k]][[i]][,2], temp.cor.3[[k]][[i]][,3]) 
       
  
  } #end of i
  } #end of k
  
  Results = new.env()
  Results$a = cor1_yhatbase
  Results$b = cor1
  
  return(Results)
  }#end of function