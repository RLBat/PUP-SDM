rm(list= ls())
require(biomod2)
require(raster)
require(dplyr)
require(magrittr)
require(sp)

### the final models that I'm going to be using in the ensemble are 
## GBM  -- Generalized Boosting model or Boosted regression trees
## RF -- Random Forest
## FDA -- Flexible Discriminant Analysis
## MARS -- Multiple adaptive Regression Splines
## MAXENT -- Maxengt Phillips 

### For all models except Maxent the proportion of PAs best was 10x for MAXENT species with more than 8000 presences 1x was best below that 10x was best.
## So now lets go get those models. 

load("RData_European_Bee_Species_Thinned_Data_5km.RData")

Species <- c()
for(i in 1:length(Thinned_Data_5km)){
  Species[i] <- Thinned_Data_5km[[i]][["species"]][1]
}


setwd("D:/SDM_2")
for(i in 1:length(Species)){
  
  Spp_name <- Species[i]
  Spp_name_1 <- sub(" ",".", Spp_name)
     load(paste(paste(paste(paste("SDM/",Spp_name_1, sep = ""),Spp_name_1,sep = "/"),Spp_name,sep = "."),"FirstModeling_10x.models.out"))
   }


Model_Outs<-setNames(lapply(ls(pattern ="Modeling*"), function(x) get(x)),(ls(pattern="Modeling*")))
setwd("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs")
save(Model_Outs, file = "RData_European_Bee_Species_SDM_Evaluation_Model_Outs.RData")





