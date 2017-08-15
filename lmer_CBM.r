
  ## Rscript for fitting CBM on MET dataset from unreplicated trial
      ## data1 contains Field_ids, Hybrid_ids, Yield, and Region as column 1,2,3, and 4
      ## These variables except Yield are continuous index values converted into factors
      ## baseR represents the base model and CBM represents the CBM model

  baseR <- lmer(Yield ~ 1+ (1|Field_ids) + (1|Hybrid_ids), data1)
  CBM   <- lmer(Yield ~ 1+ (1|Field_ids) + (1|Hybrid_ids) + (1| Region:Hybrid_ids),data1)









