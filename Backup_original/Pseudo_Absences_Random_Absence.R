rm(list = ls())
require(dplyr)
require(tidyr)
require(biomod2)
require(magrittr)
require(rgdal)
require(sp)
require(dismo)
require(mopa)
require(raster)

load("RData_European_Bee_Species_Thinned_Data_5km.RData")
Bioclim_01 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_01.tif")
Bioclim_04 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_04.tif")
Bioclim_12 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_12.tif")
Bioclim_15 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_15.tif")   

Bioclim_Stack <- stack(Bioclim_01,Bioclim_04,Bioclim_12,Bioclim_15)


registerDoParallel(cores = 4)


Random_Absence_Data_10x <- foreach(i = 85:length(Thinned_Data_5km),
        .packages = c("dplyr","tidyr","magrittr","rgdal","sp","dismo","mopa","raster")) %dopar% {
spp_xy <- Thinned_Data_5km[[i]][c("decimallongitude","decimallatitude")]           ### extract the coordinates for each species
xmin <- floor(min(spp_xy$decimallongitude))                            ## min long
xmax <- ceiling(max(spp_xy$decimallongitude))                          ## max long
ymin <- floor(min(spp_xy$decimallatitude))                             ## min lat
ymax <- ceiling(max(spp_xy$decimallatitude))                           ## max at

ex <- extent(xmin, xmax, ymin, ymax)                                   ## create and extent raster
bg <- crop(x = Bioclim_Stack, y = ex)                                  ## crop bioclimatic variable raster stack to the extent size

background <- backgroundGrid(bg)                                       ### create a background grid to define the area for pseudoabsences to be chosen from 
unsuitable_bg <- OCSVMprofiling(xy = spp_xy, varstack = Bioclim_Stack,background = background$xy)  ## define a area that is climatically suitable for the species and exlude these areas from being chosen as absences 

pseudo_abs_10 <- pseudoAbsences(xy= spp_xy, background = unsuitable_bg$absence,              ### choose pseudo-absences points from the background grid (excluding the areas highlighted) 
                             prevalence = -4, kmeans = FALSE,exclusion.buffer = 0.0415)   ## prevalence - proportion of points that are presence point -- exclusion buffer set a 5km - so pseudoabsence points are at least 5km away from a presence point so grid cells will not contain bouth an absence and a presence point.
pseudo_abs_10 <- pseudo_abs_10[["species1"]][["PA01"]][[1]] %>% filter( v == 0)                ## filter out just the pseudo-absences generated 
assign(paste(Thinned_Data_5km[[i]][["species"]][1],"Random_Absence_10x", sep = "_"),pseudo_abs_10)
}




Random_Absence_Data_1x <- foreach(i = 85:length(Thinned_Data_5km),
                                   .packages = c("dplyr","tidyr","magrittr","rgdal","sp","dismo","mopa","raster")) %dopar% {
  spp_xy <- Thinned_Data_5km[[i]][c("decimallongitude","decimallatitude")]           ### extract the coordinates for each species
  xmin <- floor(min(spp_xy$decimallongitude))                            ## min long
  xmax <- ceiling(max(spp_xy$decimallongitude))                          ## max long
  ymin <- floor(min(spp_xy$decimallatitude))                             ## min lat
  ymax <- ceiling(max(spp_xy$decimallatitude))                           ## max at
  
  ex <- extent(xmin, xmax, ymin, ymax)                                   ## create and extent raster
  bg <- crop(x = Bioclim_Stack, y = ex)                                  ## crop bioclimatic variable raster stack to the extent size
  
  background <- backgroundGrid(bg)                                       ### create a background grid to define the area for pseudoabsences to be chosen from 
  unsuitable_bg <- OCSVMprofiling(xy = spp_xy, varstack = Bioclim_Stack,background = background$xy)  ## define a area that is climatically suitable for the species and exlude these areas from being chosen as absences 
  


pseudo_abs_1 <-pseudoAbsences(xy = spp_xy, background = unsuitable_bg$absence,
                              prevalence = 0.5, kmeans = FALSE, exclusion.buffer = 0.0415)
pseudo_abs_1 <- pseudo_abs_1[["species1"]][["PA01"]][[1]] %>% filter( v == 0) 
assign(paste(Thinned_Data_5km[[i]][["species"]][1],"Random_Absence_1x", sep = "_"),pseudo_abs_1)
}                                                                          

Random_Absence_Data_1x  <- Pseudo_Absence_Data_1x
Random_Absence_Data_10x <- Pseudo_Absence_Data_10x


for(i in 1:length(Thinned_Data_5km)){
  names(Pseudo_Absence_Data_1x)[i] <- paste(Thinned_Data_5km[[i]][["species"]][i],"Random_Absence_1x",sep = "_")
  names(Pseudo_Absence_Data_10x)[i] <- paste(Thinned_Data_5km[[i]][["species"]][i],"Random_Absence_10x",sep = "_")
  
  }


save(file = "RData_European_Bee_Species_Random_Absence_Data_10x.RData", Random_Absence_Data_10x)
save(file = "RData_European_Bee_Species_Random_Absence_Data_1x.RData", Random_Absence_Data_1x)

