  
  ## Algorithm for testing predictive performance of CBMi and CBMg 
    ## data1 has columns in the order Field_ids, Hybrid_ids, Yield, Geo_region, Ind_region, GEI_geo, and GEI_ind
        ## The variables except Yield are continuous index values
        ## Geo_region and Ind_region are index values for regions based on geographical and environmental clustering respectively
        ## GEI_geo and GEI_ind are index values for 'genotype x region' effect for geographical and environmental clusters respectively
        ## In the output, regions with only one location will be represented as -1, 0.99, or NA
    
  cv.cluster <- function(data1){
  fieldlevels <- levels(factor(data1$Field_ids))
  
   train.data1 = list()
   test.data1 = list()
  
      baseR.train <- list()
      basesummary_train <- list()
      baseraneffect_train <- list()
  
      temp.baseraneffect <- list()
  
      yhatstar_base <- list()
  
      cor_yhatbase <- matrix(0,1,length(fieldlevels))
  
          CBMg.train <- list()
          CBMg.summary_train <- list()
          CBMg.raneffect_train_H <- list()
          CBMg.raneffect_train_RH <- list()
  
          temp.modelraneffect_Hg <- list()
          yhatstar_model_Hg <- list()
          temp.modelraneffect_RHg <- list()
          yhatstar_model_RHg <- list()
  
          yhatstar_model_g <- list()
  
          cor_yhatmodel_g <- matrix(0,1,length(fieldlevels))
          
              CBMi.train <- list()
              CBMi.summary_train <- list()
              CBMi.raneffect_train_H <- list()
              CBMi.raneffect_train_RH <- list()
  
              temp.modelraneffect_Hi <- list()
              yhatstar_model_Hi <- list()
              temp.modelraneffect_RHi <- list()
              yhatstar_model_RHi <- list()
  
              yhatstar_model_i <- list()
  
              cor_yhatmodel_i <- matrix(0,1,length(fieldlevels))        
  
  
   for (k in c(1:length(fieldlevels))){
    test.fields <- fieldlevels[k]
    train.fields <- which(!(fieldlevels %in% test.fields))
  
      which(data1$Field_ids %in% train.fields)
      which(!(data1$Field_ids %in% train.fields))
      train.rows <- which(data1$Field_ids %in% train.fields)
      test.rows <- which(!(data1$Field_ids %in% train.fields))
  
      train.data1[[k]] <- data1[train.rows, ]
      test.data1[[k]] <- data1[test.rows, ]
  
  
  #for base model
  
    baseR.train[[k]] <- lmer(Yield ~ 1+ (1|Field_ids) + (1|Hybrid_ids), train.data1[[k]])
    basesummary_train[[k]] <- summary(baseR.train[[k]])
    baseraneffect_train[[k]] <- ranef(baseR.train[[k]])$Hybrid_ids
  
          nHybrids <- as.numeric(levels(factor(train.data1[[k]]$Hybrid_ids)))
          temp.baseraneffect[[k]] <- cbind(matrix(nHybrids), as.matrix(baseraneffect_train[[k]]))
          colnames(temp.baseraneffect[[k]]) <- c("Hybrid_ids","Intercept")
  
            yhatstar_base[[k]] <- merge(temp.baseraneffect[[k]], test.data1[[k]][,c(2,3)], by="Hybrid_ids")
  
              cor_yhatbase[1,k] <- cor(as.matrix(yhatstar_base[[k]])[,2], as.matrix(yhatstar_base[[k]])[,3])
  
  
  #model CBMg
      CBMg.train[[k]] <- lmer(Yield ~ 1+ (1|Field_ids) + (1|Hybrid_ids) + (1| Geo_region:Hybrid_ids), train.data1[[k]])
      CBMg.summary_train[[k]] <- summary(CBMg.train[[k]])
      CBMg.raneffect_train_H[[k]] <- ranef(CBMg.train[[k]])$Hybrid_ids
      CBMg.raneffect_train_RH[[k]] <- ranef(CBMg.train[[k]])$Geo_region
  
          nHybrids <- as.numeric(levels(factor(train.data1[[k]]$Hybrid_ids)))
          temp.modelraneffect_Hg[[k]] <- cbind(matrix(nHybrids), as.matrix(CBMg.raneffect_train_H[[k]]))
          colnames(temp.modelraneffect_Hg[[k]]) <- c("Hybrid_ids","Intercept")
          yhatstar_model_Hg[[k]] <- merge(temp.modelraneffect_Hg[[k]], test.data1[[k]][,c(2,3)], by="Hybrid_ids")
  
            temp.GEIg <- train.data1[[k]][,c(2,4,6)]
            temp.GEIg_1 <- temp.GEIg[order(temp.GEIg$Geo_region,temp.GEIg$Hybrid_ids,temp.GEIg$GEI_geo),]
            GEIg <- unique(temp.GEIg_1)
  
                temp.modelraneffect_RHg[[k]] <- cbind(as.matrix(GEIg[,3]), (CBMg.raneffect_train_RH[[k]]))
                colnames(temp.modelraneffect_RHg[[k]]) <- c("GEI_geo", "Intercept_GE")
                yhatstar_model_RHg[[k]] <- merge(temp.modelraneffect_RHg[[k]], test.data1[[k]][,c(2,6)], by="GEI_geo")
            
                    temp.yhatstar_model_g <- merge(yhatstar_model_Hg[[k]], yhatstar_model_RHg[[k]], by="Hybrid_ids")
                    yhatstar_model_g[[k]] <- cbind(matrix(temp.yhatstar_model_g[,3]), matrix(unlist(temp.yhatstar_model_g[,2] 
                                                                                              + temp.yhatstar_model_g[,5]), ncol=1))
                      
                         cor_yhatmodel_g[1,k] <- cor(yhatstar_model_g[[k]][,1], yhatstar_model_g[[k]][,2])
  
                                                    
  #model CBMi
      CBMi.train[[k]] <- lmer(Yield ~ 1+ (1|Field_ids) + (1|Hybrid_ids) + (1| Index_region:Hybrid_ids), train.data1[[k]])
      CBMi.summary_train[[k]] <- summary(CBMi.train[[k]])
      CBMi.raneffect_train_H[[k]] <- ranef(CBMi.train[[k]])$Hybrid_ids
      CBMi.raneffect_train_RH[[k]] <- ranef(CBMi.train[[k]])$Index_region
  
          nHybrids <- as.numeric(levels(factor(train.data1[[k]]$Hybrid_ids)))
          temp.modelraneffect_Hi[[k]] <- cbind(matrix(nHybrids), as.matrix(CBMi.raneffect_train_H[[k]]))
          colnames(temp.modelraneffect_Hi[[k]]) <- c("Hybrid_ids","Intercept")
          yhatstar_model_Hi[[k]] <- merge(temp.modelraneffect_Hi[[k]], test.data1[[k]][,c(2,3)], by="Hybrid_ids")
  
            temp.GEIi <- train.data1[[k]][,c(2,5,7)]
            temp.GEIi_1 <- temp.GEIi[order(temp.GEIi$Index_region,temp.GEIi$Hybrid_ids,temp.GEIi$GEI_ind),]
            GEIi <- unique(temp.GEIi_1)
  
                temp.modelraneffect_RHi[[k]] <- cbind(as.matrix(GEIi[,3]), (CBMi.raneffect_train_RH[[k]]))
                colnames(temp.modelraneffect_RHi[[k]]) <- c("GEI_ind", "Intercept_GE")
                yhatstar_model_RHi[[k]] <- merge(temp.modelraneffect_RHi[[k]], test.data1[[k]][,c(2,7)], by="GEI_ind")
  
                    temp.yhatstar_model_i <- merge(yhatstar_model_Hi[[k]], yhatstar_model_RHi[[k]], by="Hybrid_ids")
                    yhatstar_model_i[[k]] <- cbind(matrix(temp.yhatstar_model_i[,3]), matrix(unlist(temp.yhatstar_model_i[,2] 
                                                                                            + temp.yhatstar_model_i[,5]), ncol=1))
                    
                           cor_yhatmodel_i[1,k] <- cor(yhatstar_model_i[[k]][,1], yhatstar_model_i[[k]][,2])
  
                                
   } #end of k
  
  Results = new.env()
  Results$a = cor_yhatbase
  Results$b = cor_yhatmodel_g
  Results$ci = cor_yhatmodel_i
  
  return(Results)
  }#end of function
  
