rm(list= ls())
require(biomod2)
require(dismo)
require(raster)
require(magrittr)
require(foreach)
require(doParallel)
require(rJava)
require(ff)

load("RData_European_Bee_Species_Thinned_Data_5km.RData")     ### Load Presence and Absence Data
load("RData_European_Bee_Species_Occ_Absence_Data.RData")
load("RData_European_Bee_Species_Random_Absence_Data_10x.RData")
load("RData_European_Bee_Species_Random_Absence_Data_1x.RData")

Bioclim_01 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_01.tif")  ## Load Bioclimatic variable Data

Bioclim_04 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_04.tif")

Bioclim_12 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_12.tif")

Bioclim_15 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_15.tif")

Bioclim_Stack <- stack(Bioclim_01, Bioclim_04, Bioclim_12, Bioclim_15)           ## Combine Bioclimatic variables into a stack 


setwd("C:/Users/patri/Desktop/SDM")     #### THIS was done for the benefit of MAXENT because it needed a path that didn't have any spaces in and Imperial onedrive had spaces in -.-
                                

## This script will take a long time to do as it will have to do all three models for all 367 species so I think I will look into running this in parallel
## in the future but I will have to see how I would do that.

registerDoParallel(cores = 4)


SDM_Evaluations <- foreach(i = 1:length(Thinned_Data_5km),
                           .packages = c("biomod2", "dismo", "raster", "magrittr","dplyr"),
                           .combine = "rbind") %dopar% {          
                             Spp_name <- Thinned_Data_5km[[i]][["species"]][1]                               ## For the thinned data extract : species Name
                             Spp_Presence <- Thinned_Data_5km[[i]][c("decimallongitude","decimallatitude")]    ### Longitude and latitude
                             Spp_Presence$presence <- 1                                                      ### Code Presences as 1
                             
                             Rnm_Spp_Absence_10x <- Random_Absence_Data[[Spp_name]][[1]]                                   ### Extract absence data where absences are 10x greater than presences 
                             colnames(Rnm_Spp_Absence_10x)[1:3] <- c("decimallongitude","decimallatitude", "presence")   ## Rename columns to match presence set
                             
                             Occ_Spp_Absence <- Occ_Absence_Data[[paste(Thinned_Data_5km[[i]][["species"]][1],"Occ_Absence_Data",sep = "_")]][c(1:2,4)]                                   ### Do the same for the occurrence absences
                             
                             Spp_Data_10x <- rbind(Spp_Presence,Rnm_Spp_Absence_10x,Occ_Spp_Absence)                    ### Bind all three datasets together to have the presence/absence dataset
                             RespVar_10x <- as.numeric(Spp_Data_10x$presence)                                       ## Seperate Response variable (Presence/absence)
                             RespVarCoord_10x <- Spp_Data_10x[1:2]                                                  ## And Coordinates
                             
                             xmin_10x <- floor(min(RespVarCoord_10x$decimallongitude))                             #### Get the extent of the SDMs 
                             xmax_10x <- ceiling(max(RespVarCoord_10x$decimallongitude))
                             ymin_10x <- floor(min(RespVarCoord_10x$decimallatitude))
                             ymax_10x <- ceiling(max(RespVarCoord_10x$decimallatitude)) 
                             
                             extent_10x <- extent(xmin_10x,xmax_10x,ymin_10x,ymax_10x)
                             ExplVar_10x<- stack(crop(Bioclim_Stack, extent_10x))                                   ### Crop Bioclimatic Stack to the right extent
                             
                             
                             
                             
                             ###### Make another Dataset for the equl number of absences as presences
                             
                             
                             Rnm_Spp_Absence_1x <- Random_Absence_Data[[Spp_name]][[2]]                                   ### Extract absence data where absences are 10x greater than presences 
                             colnames(Rnm_Spp_Absence_1x)[1:3] <- c("decimallongitude","decimallatitude", "presence")   ## Rename columns to match presence set
                             
                             
                             Spp_Data_1x <- rbind(Spp_Presence,Rnm_Spp_Absence_1x,Occ_Spp_Absence)                    ### Bind all three datasets together to have the presence/absence dataset
                             RespVar_1x <- as.numeric(Spp_Data_1x$presence)                                       ## Seperate Response variable (Presence/absence)
                             RespVarCoord_1x <- Spp_Data_1x[1:2]                                                  ## And Coordinates
                             
                             xmin_1x <- floor(min(RespVarCoord_1x$decimallongitude))                             #### Get the extent of the SDMs 
                             xmax_1x <- ceiling(max(RespVarCoord_1x$decimallongitude))
                             ymin_1x <- floor(min(RespVarCoord_1x$decimallatitude))
                             ymax_1x <- ceiling(max(RespVarCoord_1x$decimallatitude)) 
                             
                             extent_1x <- extent(xmin_1x,xmax_1x,ymin_1x,ymax_1x)
                             ExplVar_1x<- stack(crop(Bioclim_Stack, extent_1x))                                   ### Crop Bioclimatic Stack to the right extent
                             
                             
                             ### Now have two datasets Spp_Data_1x and the rest and Spp_Data_10x and the rest to make comparisons.
                             
                             
                             BiomodData_10x <- BIOMOD_FormatingData(resp.var = RespVar_10x,                       ### Biomod2 will format the data so that it is suitable to run SDMs with 
                                                                    expl.var = ExplVar_10x,                     ## This is done by basically creating two matricies one of the response variable and sampling unit
                                                                    resp.xy = RespVarCoord_10x,                  ## the other being explanatory variable and sampling unit 
                                                                    resp.name = Spp_name)
                             
                             BiomodData_1x <- BIOMOD_FormatingData(resp.var = RespVar_1x,                       
                                                                   expl.var = ExplVar_1x,                     
                                                                   resp.xy = RespVarCoord_1x,                 
                                                                   resp.name = Spp_name)
                             
                             
                             myBiomodOptions <- BIOMOD_ModelingOptions()                   ##### ENvironment "background_data_dir" route to environmental raster from which maxent choses backgrond points.  
                             
                             ## Biomod modelling options can be changed but I havent looked into doing this yet so options have been left on default
                             
                             myBiomodModelOut_10x <- BIOMOD_Modeling(BiomodData_10x,                            
                                                                     models = c("GLM","GAM","MARS","RF","SRE","GBM", "CTA", "ANN","FDA"),        ### The modelling approaches that are available with Biomod2 - will maybe look into doing HMSC as well.
                                                                     models.options = myBiomodOptions,
                                                                     NbRunEval = 3,                         ## Three fold cross validation ( 3 - k fold)
                                                                     DataSplit = 80,                       ## using an 80 - 20 split for calibration and evaluation of the model
                                                                     Prevalence = 0.5,                     ## Presence and absences are equalling weighted whereby the sum of the weights of the presences equals the sum of the weights of the absences
                                                                     VarImport = 3,                        ## Number of permutations to estimate variable importance
                                                                     models.eval.meth = c("TSS","ROC","ACCURACY","BIAS"),   ### Names of Evaluation metric used - True skill statistic (TSS - used to create binary response for projection), ACCURACY, Relative Operating Characteristic (ROC/AUC)(DISCRIMINATION), Accuracy (ACCURACY), KAPPA (Cohen's K) - Accuracy of the forecast relative to random chance.  
                                                                     SaveObj = TRUE,                            ### ACCURACY can then be used to also calculate precision and it is by these four measire of predictive performance we will choose our
                                                                     rescal.all.models = TRUE,                  ### Ensemble of SDMs.
                                                                     do.full.models = FALSE,
                                                                     modeling.id = paste(Spp_name,"FirstModeling_10x",sep = " "))     ## File to save the outputs of these models
                             
                             
                             #### Model for equal number of absences again
                             
                             myBiomodModelOut_1x <- BIOMOD_Modeling(BiomodData_1x,                            
                                                                    models = c("GLM","GAM","MARS","RF","SRE","GBM", "CTA", "ANN","FDA"),       
                                                                    models.options = myBiomodOptions,
                                                                    NbRunEval = 3,                        
                                                                    DataSplit = 80,                       
                                                                    Prevalence = 0.5,                     
                                                                    VarImport = 3,                        
                                                                    models.eval.meth = c("TSS","ROC","ACCURACY","BIAS"),     
                                                                    SaveObj = TRUE,                            
                                                                    rescal.all.models = TRUE,                  
                                                                    do.full.models = FALSE,
                                                                    modeling.id = paste(Spp_name,"FirstModeling_1x",sep = " "))
                             
                             
                             myBiomodModelEval_10x <- get_evaluations(myBiomodModelOut_10x)## Extract the evaluation statistics from the modelling output
                             myBiomodModelEval_1x <- get_evaluations(myBiomodModelOut_1x)
                             
                             
                             get_mean_eval <- function(data,x,y){                             ## This is a function that will get the mean of the three fold cross validation evaluation for each of the statistics
                               eval <- as.data.frame(data[y,"Testing.data",x,,])
                               mean_eval <- mean(eval[,1], na.rm = TRUE)
                             }
                             
                             myBiomod_Prediction_10x <- cbind(get_formal_data(myBiomodModelOut_10x,"resp.var"),
                                                              as.data.frame(get_predictions(myBiomodModelOut_10x)))
                             
                             myBiomod_Prediction_1x <- cbind(get_formal_data(myBiomodModelOut_1x,"resp.var"),
                                                             as.data.frame(get_predictions(myBiomodModelOut_1x)))
                             
                             get_precision_eval <- function(data, x){      ### This is a function that will calculate a measure of precision in the results being the standard deviation of the predictied species occurrences. 
                               Precision <- data %>%
                                 dplyr::select(paste(x,".RUN1.AllData",sep = ""),
                                               paste(x,".RUN2.AllData",sep = ""),
                                               paste(x,".RUN3.AllData",sep = ""))
                               mean <- c(mean(Precision[,1]/1000),mean(Precision[,2]/1000),mean(Precision[,3]/1000))  
                               mean <- mean(mean, na.rm = TRUE)
                               Precision <- sqrt(mean*(1-mean))  
                             }
                             
                             
                             
                             ### Compile all the means of all the statistics from each of the regressions methods 
                             
                             
                             ROC_GLM_10x <- get_mean_eval(myBiomodModelEval_10x,"GLM","ROC")
                             TSS_GLM_10x <- get_mean_eval(myBiomodModelEval_10x,"GLM","TSS")
                             ACCURACY_GLM_10x <- get_mean_eval(myBiomodModelEval_10x,"GLM","ACCURACY")
                             BIAS_GLM_10x <- get_mean_eval(myBiomodModelEval_10x,"GLM","BIAS")
                             PRECISION_GLM_10x <- get_precision_eval(myBiomod_Prediction_10x,"GLM")
                             
                             
                             ROC_GAM_10x <- get_mean_eval(myBiomodModelEval_10x,"GAM","ROC")
                             TSS_GAM_10x <- get_mean_eval(myBiomodModelEval_10x,"GAM","TSS")
                             ACCURACY_GAM_10x <- get_mean_eval(myBiomodModelEval_10x,"GAM","ACCURACY")
                             BIAS_GAM_10x <- get_mean_eval(myBiomodModelEval_10x,"GAM","BIAS")
                             PRECISION_GAM_10x <- get_precision_eval(myBiomod_Prediction_10x,"GAM")
                             
                             
                             ROC_MARS_10x <- get_mean_eval(myBiomodModelEval_10x,"MARS","ROC")
                             TSS_MARS_10x <- get_mean_eval(myBiomodModelEval_10x,"MARS","TSS")
                             ACCURACY_MARS_10x <- get_mean_eval(myBiomodModelEval_10x,"MARS","ACCURACY")
                             BIAS_MARS_10x <- get_mean_eval(myBiomodModelEval_10x,"MARS","BIAS")
                             PRECISION_MARS_10x <- get_precision_eval(myBiomod_Prediction_10x,"MARS")
                             
                             
                             ROC_RF_10x <- get_mean_eval(myBiomodModelEval_10x,"RF","ROC")
                             TSS_RF_10x <- get_mean_eval(myBiomodModelEval_10x,"RF","TSS")
                             ACCURACY_RF_10x <- get_mean_eval(myBiomodModelEval_10x,"RF","ACCURACY")
                             BIAS_RF_10x <- get_mean_eval(myBiomodModelEval_10x,"RF","BIAS")
                             PRECISION_RF_10x <- get_precision_eval(myBiomod_Prediction_10x,"RF")
                             
                             
                             ROC_SRE_10x <- get_mean_eval(myBiomodModelEval_10x,"SRE","ROC")
                             TSS_SRE_10x <- get_mean_eval(myBiomodModelEval_10x,"SRE","TSS")
                             ACCURACY_SRE_10x <- get_mean_eval(myBiomodModelEval_10x,"SRE","ACCURACY")
                             BIAS_SRE_10x <- get_mean_eval(myBiomodModelEval_10x,"SRE","BIAS")
                             PRECISION_SRE_10x <- get_precision_eval(myBiomod_Prediction_10x,"SRE")
                             
                             
                             ROC_GBM_10x <- get_mean_eval(myBiomodModelEval_10x,"GBM","ROC")
                             TSS_GBM_10x <- get_mean_eval(myBiomodModelEval_10x,"GBM","TSS")
                             ACCURACY_GBM_10x <- get_mean_eval(myBiomodModelEval_10x,"GBM","ACCURACY")
                             BIAS_GBM_10x <- get_mean_eval(myBiomodModelEval_10x,"GBM","BIAS")
                             PRECISION_GBM_10x <- get_precision_eval(myBiomod_Prediction_10x,"GBM")
                             
                             
                             ROC_CTA_10x <- get_mean_eval(myBiomodModelEval_10x,"CTA","ROC")
                             TSS_CTA_10x <- get_mean_eval(myBiomodModelEval_10x,"CTA","TSS")
                             ACCURACY_CTA_10x <- get_mean_eval(myBiomodModelEval_10x,"CTA","ACCURACY")
                             BIAS_CTA_10x <- get_mean_eval(myBiomodModelEval_10x,"CTA","BIAS")
                             PRECISION_CTA_10x <- get_precision_eval(myBiomod_Prediction_10x,"CTA")
                             
                             
                             ROC_ANN_10x <- get_mean_eval(myBiomodModelEval_10x,"ANN","ROC")
                             TSS_ANN_10x <- get_mean_eval(myBiomodModelEval_10x,"ANN","TSS")
                             ACCURACY_ANN_10x <- get_mean_eval(myBiomodModelEval_10x,"ANN","ACCURACY")
                             BIAS_ANN_10x <- get_mean_eval(myBiomodModelEval_10x,"ANN","BIAS")
                             PRECISION_ANN_10x <- get_precision_eval(myBiomod_Prediction_10x,"ANN")
                             
                             
                             ROC_FDA_10x <- get_mean_eval(myBiomodModelEval_10x,"FDA","ROC")
                             TSS_FDA_10x <- get_mean_eval(myBiomodModelEval_10x,"FDA","TSS")
                             ACCURACY_FDA_10x <- get_mean_eval(myBiomodModelEval_10x,"FDA","ACCURACY")
                             BIAS_FDA_10x <- get_mean_eval(myBiomodModelEval_10x,"FDA","BIAS")
                             PRECISION_FDA_10x <- get_precision_eval(myBiomod_Prediction_10x,"FDA")
                             
                             ### Again for the equal presences and absences
                             
                             ROC_GLM_1x <- get_mean_eval(myBiomodModelEval_1x,"GLM","ROC")
                             TSS_GLM_1x <- get_mean_eval(myBiomodModelEval_1x,"GLM","TSS")
                             ACCURACY_GLM_1x <- get_mean_eval(myBiomodModelEval_1x,"GLM","ACCURACY")
                             BIAS_GLM_1x <- get_mean_eval(myBiomodModelEval_1x,"GLM","BIAS")
                             PRECISION_GLM_1x <- get_precision_eval(myBiomod_Prediction_1x,"GLM")
                             
                             
                             ROC_GAM_1x <- get_mean_eval(myBiomodModelEval_1x,"GAM","ROC")
                             TSS_GAM_1x <- get_mean_eval(myBiomodModelEval_1x,"GAM","TSS")
                             ACCURACY_GAM_1x <- get_mean_eval(myBiomodModelEval_1x,"GAM","ACCURACY")
                             BIAS_GAM_1x <- get_mean_eval(myBiomodModelEval_1x,"GAM","BIAS")
                             PRECISION_GAM_1x <- get_precision_eval(myBiomod_Prediction_1x,"GAM")
                             
                             
                             ROC_MARS_1x <- get_mean_eval(myBiomodModelEval_1x,"MARS","ROC")
                             TSS_MARS_1x <- get_mean_eval(myBiomodModelEval_1x,"MARS","TSS")
                             ACCURACY_MARS_1x <- get_mean_eval(myBiomodModelEval_1x,"MARS","ACCURACY")
                             BIAS_MARS_1x <- get_mean_eval(myBiomodModelEval_1x,"MARS","BIAS")
                             PRECISION_MARS_1x <- get_precision_eval(myBiomod_Prediction_1x,"MARS")
                             
                             
                             ROC_RF_1x <- get_mean_eval(myBiomodModelEval_1x,"RF","ROC")
                             TSS_RF_1x <- get_mean_eval(myBiomodModelEval_1x,"RF","TSS")
                             ACCURACY_RF_1x <- get_mean_eval(myBiomodModelEval_1x,"RF","ACCURACY")
                             BIAS_RF_1x <- get_mean_eval(myBiomodModelEval_1x,"RF","BIAS")
                             PRECISION_RF_1x <- get_precision_eval(myBiomod_Prediction_1x,"RF")
                             
                             
                             ROC_SRE_1x <- get_mean_eval(myBiomodModelEval_1x,"SRE","ROC")
                             TSS_SRE_1x <- get_mean_eval(myBiomodModelEval_1x,"SRE","TSS")
                             ACCURACY_SRE_1x <- get_mean_eval(myBiomodModelEval_1x,"SRE","ACCURACY")
                             BIAS_SRE_1x <- get_mean_eval(myBiomodModelEval_1x,"SRE","BIAS")
                             PRECISION_SRE_1x <- get_precision_eval(myBiomod_Prediction_1x,"SRE")
                             
                             
                             ROC_GBM_1x <- get_mean_eval(myBiomodModelEval_1x,"GBM","ROC")
                             TSS_GBM_1x <- get_mean_eval(myBiomodModelEval_1x,"GBM","TSS")
                             ACCURACY_GBM_1x <- get_mean_eval(myBiomodModelEval_1x,"GBM","ACCURACY")
                             BIAS_GBM_1x <- get_mean_eval(myBiomodModelEval_1x,"GBM","BIAS")
                             PRECISION_GBM_1x <- get_precision_eval(myBiomod_Prediction_1x,"GBM")
                             
                             
                             ROC_CTA_1x <- get_mean_eval(myBiomodModelEval_1x,"CTA","ROC")
                             TSS_CTA_1x <- get_mean_eval(myBiomodModelEval_1x,"CTA","TSS")
                             ACCURACY_CTA_1x <- get_mean_eval(myBiomodModelEval_1x,"CTA","ACCURACY")
                             BIAS_CTA_1x <- get_mean_eval(myBiomodModelEval_1x,"CTA","BIAS")
                             PRECISION_CTA_1x <- get_precision_eval(myBiomod_Prediction_1x,"CTA")
                             
                             
                             ROC_ANN_1x <- get_mean_eval(myBiomodModelEval_1x,"ANN","ROC")
                             TSS_ANN_1x <- get_mean_eval(myBiomodModelEval_1x,"ANN","TSS")
                             ACCURACY_ANN_1x <- get_mean_eval(myBiomodModelEval_1x,"ANN","ACCURACY")
                             BIAS_ANN_1x <- get_mean_eval(myBiomodModelEval_1x,"ANN","BIAS")
                             PRECISION_ANN_1x <- get_precision_eval(myBiomod_Prediction_1x,"ANN")
                             
                             
                             ROC_FDA_1x <- get_mean_eval(myBiomodModelEval_1x,"FDA","ROC")
                             TSS_FDA_1x <- get_mean_eval(myBiomodModelEval_1x,"FDA","TSS")
                             ACCURACY_FDA_1x <- get_mean_eval(myBiomodModelEval_1x,"FDA","ACCURACY")
                             BIAS_FDA_1x <- get_mean_eval(myBiomodModelEval_1x,"FDA","BIAS")
                             PRECISION_FDA_1x <- get_precision_eval(myBiomod_Prediction_1x,"FDA")
                             
                             ##collates all the evaluation statistics together into a single table
                             
                             Evaluations <- data.frame(Spp_name,nrow(Spp_Presence), (nrow(Spp_Data_10x)-nrow(Spp_Presence)),(nrow(Spp_Data_1x)-nrow(Spp_Presence)),                                              ## Collate into a data frame
                                                       ROC_GLM_10x, TSS_GLM_10x,BIAS_GLM_10x, ACCURACY_GLM_10x, PRECISION_GLM_10x,
                                                       ROC_GLM_1x, TSS_GLM_1x,BIAS_GLM_1x, ACCURACY_GLM_1x, PRECISION_GLM_1x,           ### Measure of precision sqrt(ACCURACY(1-ACCURACY))
                                                       ROC_GAM_10x, TSS_GAM_10x,BIAS_GAM_10x, ACCURACY_GAM_10x, PRECISION_GAM_10x,
                                                       ROC_GAM_1x, TSS_GAM_1x,BIAS_GAM_1x, ACCURACY_GAM_1x, PRECISION_GAM_1x,
                                                       ROC_MARS_10x, TSS_MARS_10x,BIAS_MARS_10x, ACCURACY_MARS_10x, PRECISION_MARS_10x,
                                                       ROC_MARS_1x, TSS_MARS_1x,BIAS_MARS_1x, ACCURACY_MARS_1x, PRECISION_MARS_1x,
                                                       ROC_RF_10x, TSS_RF_10x,BIAS_RF_10x, ACCURACY_RF_10x, PRECISION_RF_10x,
                                                       ROC_RF_1x, TSS_RF_1x,BIAS_RF_1x, ACCURACY_RF_1x, PRECISION_RF_1x,
                                                       ROC_SRE_10x, TSS_SRE_10x,BIAS_SRE_10x, ACCURACY_SRE_10x, PRECISION_SRE_10x,
                                                       ROC_SRE_1x, TSS_SRE_1x,BIAS_SRE_1x, ACCURACY_SRE_1x, PRECISION_SRE_1x,
                                                       ROC_GBM_10x, TSS_GBM_10x,BIAS_GBM_10x, ACCURACY_GBM_10x, PRECISION_GBM_10x,
                                                       ROC_GBM_1x, TSS_GBM_1x,BIAS_GBM_1x, ACCURACY_GBM_1x, PRECISION_GBM_1x,
                                                       ROC_CTA_10x, TSS_CTA_10x,BIAS_CTA_10x, ACCURACY_CTA_10x,PRECISION_CTA_10x,
                                                       ROC_CTA_1x, TSS_CTA_1x,BIAS_CTA_1x, ACCURACY_CTA_1x, PRECISION_CTA_1x,
                                                       ROC_ANN_10x, TSS_ANN_10x,BIAS_ANN_10x, ACCURACY_ANN_10x, PRECISION_ANN_10x,
                                                       ROC_ANN_1x, TSS_ANN_1x,BIAS_ANN_1x, ACCURACY_ANN_1x, PRECISION_ANN_1x,
                                                       ROC_FDA_10x, TSS_FDA_10x,BIAS_FDA_10x, ACCURACY_FDA_10x, PRECISION_FDA_10x,
                                                       ROC_FDA_1x, TSS_FDA_1x,BIAS_FDA_1x, ACCURACY_FDA_1x, PRECISION_FDA_1x)
                             
                             colnames(Evaluations)[c(1:4)] <- c("Scientific_Name","Presences","Pseudo-Absences_10x","Pseudo-Absences_1x") 
                             
                             ## Name the columns
                             Evaluations <- data.frame(Evaluations)
                           }


setwd("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs") ## Set working dircetor back to the project
save(file = "RData_European_Bee_Species_SDM_Evaluations.RData", SDM_Evaluations)                               ## save. 
ff::file.move("C:/Users/patri/Desktop/SDM_Eval","C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/SDM_Eval") ## Move SDM_Eval folder from Desktop to SDMs folder

