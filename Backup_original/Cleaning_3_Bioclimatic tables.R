rm(list=ls())
require(dplyr)
require(tidyr)
require(raster)
require(rgdal)
require(sp)
require(ggplot2)
require(sjPlot)
require(doParallel)

load("RData_European_Bee_Species_Finish_Data.RData")   ### Load Finsish data
load("RData_European_Bee_Species_GBIF_Data.RData")     ### Load GBIF Data

## Load bioclimatic variables that are significant to Bees 
#Bioclim 1 : Annual mean temperature
#Bioclim 4 : temperature seasonality
#Bioclim 12 : annual precipitation
#Bioclim 15 : Precipitation seasonality

Bioclim_01 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_01.tif")

Bioclim_04 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_04.tif")

Bioclim_12 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_12.tif")

Bioclim_15 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_15.tif")

Bioclim_Stack <- stack(Bioclim_01,Bioclim_04,Bioclim_12,Bioclim_15) ## Can then stack the raster layers on top of each other


registerDoParallel(cores = 4)

Bioclimatic_Tables <- foreach(i = 1:length(Finish_Data),
                              .packages = c("dplyr","tidyr","raster","rgdal","sp","ggplot2")) %dopar% {

  
  
Coordinates_Clean <- Finish_Data[[i]][,c("decimallongitude","decimallatitude")]    ### Extract Coordinates 

SpatialCoord_Clean <- SpatialPointsDataFrame(coords = Coordinates_Clean, data = Finish_Data[[i]],
                                           proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))  ## Convert to spatial points dataframe 


Coordinates_GBIF<-GBIF_Data[[paste(Finish_Data[[i]][["species"]][1],"GBIF_Data",sep = "_")]][,c("decimallongitude","decimallatitude")]        #### ditto for GBIF data            

SpatialCoord_GBIF<-SpatialPointsDataFrame(coords = Coordinates_GBIF, data = GBIF_Data[[paste(Finish_Data[[i]][["species"]][1],"GBIF_Data",sep = "_")]], 
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) 






Clean_Bioclim <- raster::extract(Bioclim_Stack,SpatialCoord_Clean)      ### Extract the bioclimatic variable values for the coordinates in spdfs               
Clean_Bioclim <- data.frame(Clean_Bioclim)                       ### convert to data frame
colnames(Clean_Bioclim)[1:4] <- c("Annual_Mean_Temp",            ### Name columns accordingly 
                                "Temperature_Seasonality",
                                "Annual_Precipitation",
                                "Precipitation_Seasonality")                  
Clean_Bioclim$Dataset<-"Clean"                                      ### idnetify the dataset from which it has come from
Clean_Bioclim <- Clean_Bioclim %>% gather(Bioclimatic_Variable, Measure, c("Annual_Mean_Temp",             #### Gather the dataframe so that its a single bioclimatic variable value in each row.
                                                                          "Temperature_Seasonality",
                                                                          "Annual_Precipitation",
                                                                          "Precipitation_Seasonality"), na.rm = TRUE)



GBIF_Bioclim <- raster::extract(Bioclim_Stack,SpatialCoord_GBIF)                      ## Ditto for GBIF                                     
GBIF_Bioclim <- data.frame(GBIF_Bioclim)
colnames(GBIF_Bioclim)[1:4]<-c("Annual_Mean_Temp",
                               "Temperature_Seasonality",
                               "Annual_Precipitation",
                               "Precipitation_Seasonality")
GBIF_Bioclim$Dataset<-"GBIF"
GBIF_Bioclim <- GBIF_Bioclim %>% gather(Bioclimatic_Variable, Measure, c("Annual_Mean_Temp",
                                                                        "Temperature_Seasonality",
                                                                        "Annual_Precipitation",
                                                                        "Precipitation_Seasonality"), na.rm = TRUE)




Bioclim <- rbind(Clean_Bioclim,GBIF_Bioclim)     ## Join the datasets                              


boxplot <- ggplot(Bioclim, aes(x=Dataset, y = Measure)) +    #### Create a boxplot showing how the bioclimatic variables are influenced by the cleaning of the coordinates
  facet_wrap(.~Bioclimatic_Variable, scales = "free") +
  stat_summary(fun.y=mean, colour="red", geom="point") +
  geom_boxplot(aes(fill=Dataset), alpha= 0.5)+
  theme_bw()+
  labs( caption = "Figure 1. Boxplots for each of the four bioclimatic variables used in the analysis, annual mean temperature, annual precipitation,
        temperature seasonality and precipitation seasonality before cleaning (GBIF) and after cleaning (Clean).")


ggsave(paste((paste("Cleaning_6_Cleaning_Maps/",Finish_Data[[i]][["species"]][1],sep="")),"/6_boxplot.png",sep = ""),boxplot,device = "png", height = 10, width = 10,dpi=300)  ### Save boxplot in the species cleaning file


##### Here Im performing a t-test on the mean of each of the bioclimatic variable before and after cleaning and extracting the descriptive 
### statistics to put in a table -- Might be easier to create a function and then apply this to each variable.

## First annual temperature

Annual_Temp<-Bioclim %>%
  filter(Bioclimatic_Variable == "Annual_Mean_Temp")

lm_AT<-t.test(Measure~Dataset, data = Annual_Temp)

table_AT<-c(lm_AT$estimate,data.frame(
  lm_AT$statistic,
  lm_AT$parameter,
  lm_AT$p.value))

table_AT<-data.frame(NROW(which(Annual_Temp$Dataset == "GBIF")),
                     NROW(which(Annual_Temp$Dataset == "Clean")),
                     table_AT)

table_AT<-table_AT[,c(1:2,4,3,5:7)]

colnames(table_AT)[1:7]<-c("N before cleaning",
                           "N after cleaning",
                           "Mean before cleaning",
                           "Mean after cleaning",
                           "t-statistic",
                           "df","P-Value")


## Temperature seasonality

Temp_Season<- Bioclim %>%
  filter(Bioclimatic_Variable == "Temperature_Seasonality")
lm_TS<-t.test(Measure ~ Dataset, data = Temp_Season)
table_TS<-c(lm_TS$estimate,data.frame(
  lm_TS$statistic,
  lm_TS$parameter,
  lm_TS$p.value))
table_TS<-data.frame(NROW(which(Annual_Temp$Dataset == "GBIF")),
                     NROW(which(Annual_Temp$Dataset == "Clean")),
                     table_TS)
table_TS<-table_TS[,c(1:2,4,3,5:7)]
colnames(table_TS)[1:7]<-c("N before cleaning",
                           "N after cleaning",
                           "Mean before cleaning",
                           "Mean after cleaning",
                           "t-statistic",
                           "df","P-Value")

### Annual Precipitation

Annual_Precip<- Bioclim %>%
  filter(Bioclimatic_Variable == "Annual_Precipitation")
lm_AP<-t.test(Measure ~ Dataset, data = Annual_Precip)
table_AP<-c(lm_AP$estimate,data.frame(
  lm_AP$statistic,
  lm_AP$parameter,
  lm_AP$p.value))
table_AP<-data.frame(NROW(which(Annual_Temp$Dataset == "GBIF")),
                     NROW(which(Annual_Temp$Dataset == "Clean")),
                     table_AP)
table_AP<-table_AP[,c(1:2,4,3,5:7)]
colnames(table_AP)[1:7]<-c("N before cleaning",
                           "N after cleaning",
                           "Mean before cleaning",
                           "Mean after cleaning",
                           "t-statistic",
                           "df","P-Value")


## Precipitation Seasonality

Precip_Season<- Bioclim %>%
  filter(Bioclimatic_Variable == "Precipitation_Seasonality")
lm_PS<-t.test(Measure ~ Dataset, data = Precip_Season)
table_PS<-c(lm_PS$estimate,data.frame(
  lm_PS$statistic,
  lm_PS$parameter,
  lm_PS$p.value))
table_PS<-data.frame(NROW(which(Annual_Temp$Dataset == "GBIF")),
                     NROW(which(Annual_Temp$Dataset == "Clean")),
                     table_PS)
table_PS<-table_PS[,c(1:2,4,3,5:7)]
colnames(table_PS)[1:7]<-c("N before cleaning",
                           "N after cleaning",
                           "Mean before cleaning",
                           "Mean after cleaning",
                           "t-statistic",
                           "df","P-Value")



test_Bioclim<-rbind(table_AT,table_TS,table_AP,table_PS)    ### Bind all the resulting data frames together
 



Bioclimatic_Variable<-c("Annual Mean Temperature (Bioclim 1)",
                        "Temperature Seasonality (Bioclim 4)",
                        "Annual Precipitation (Bioclim 12)",
                        "Precipitation Seasonality (Bioclim 15)")    #### Create a vector of Bioclimatic variable names to add to df


test_Bioclim<-cbind(Bioclimatic_Variable,test_Bioclim)  ## Cbind
colnames(test_Bioclim)[1]<-c("Bioclimatic Variable") ### Name column


test_Bioclim <- test_Bioclim %>%
  mutate_if(is.numeric,round, digits = 5)  ### Round result to 5 decimal places 


for(j in 1:4){                                                   ## Add astrixs to signify significance
  if(as.numeric(test_Bioclim[j,8]) < 1e-03){
    test_Bioclim[j,8] = paste(test_Bioclim[j,8],"***",sep = "")
  } else {
    if(as.numeric(test_Bioclim[j,8]) < 1e-02){
      test_Bioclim[j,8] = paste(test_Bioclim[j,8],"**",sep = "")
    } else {
      if(as.numeric(test_Bioclim[j,8]) < 5e-02){
        test_Bioclim[j,8] = paste(test_Bioclim[j,8],"*",sep = "")
      }}}}


assign(paste(Finish_Data[[i]][["species"]][1],"Bioclimatic_Table",sep = "_"),test_Bioclim)   ### Create a dataframe in the environment for each species 
}

Bioclimatic_Tables<-setNames(lapply(ls(pattern ="Bioclimatic_Table_*"), function(x) get(x)),(ls(pattern="Bioclimatic_Table_*")))   ### Collate into a list
 
save(file = "RData_European_Bee_Species_Bioclimatic_Tables.RData", Bioclimatic_Tables)   ## Save list into an RData file

##for some reason the tables will not save using the for loop - unknown reasons - so I will collate the Bioclimatic tables together and 
## save them one by one (Y) LONG!!


Table<- function(x){        ### Createa function to produce table in a word document 
tab_df(Bioclimatic_Tables[[x]], title = "Table 1. Table showing the number of points and means of bioclimatic variables before and after
                      cleaning, the value of the t-statistic, degrees of freedom and P value comparing the means of bioclimatic variables
       before and after cleaning.", footnote = "'***' denotes significance - P < 0.05 *, P < 0.01 **, P < 0.001 ***", show.footnote = TRUE,
       use.viewer = TRUE, file = paste("Cleaning_6_Cleaning_Maps",paste(Finish_Data[[x]][["species"]][1],sep = ""),"7_Table.doc",sep="/"))
}

lapply(1:length(Bioclimatic_Tables),Table) ### instead of a for loop this worked very nicely! :)

