rm(list= ls())
require(biomod2)    ### to make the projections
require(raster)      ## working with rasters 
require(dplyr)       ## data wrangling
require(magrittr)     ## piping
require(sp)          ## working with spatial data 

#dir.create("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections")
#dir.create("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current_2")


load("RData_European_Bee_Species_SDM_Evaluation_Model_Outs.RData") #### Load the chosen models for each species
load("RData_European_Bee_Species_OLE_Coord_Limits.RData")            ### Load the OLE projection bounds 


Bioclim_01 <- raster("Bioclim_asc/wc2.0_bio_2.5m_01.asc")  ## Load Bioclimatic variable Data

Bioclim_04 <- raster("Bioclim_asc/wc2.0_bio_2.5m_04.asc")

Bioclim_12 <- raster("Bioclim_asc/wc2.0_bio_2.5m_12.asc")

Bioclim_15 <- raster("Bioclim_asc/wc2.0_bio_2.5m_15.asc")

Bioclim_Stack <- stack(Bioclim_01,Bioclim_04,Bioclim_12,Bioclim_15)     ### stak the 

Europe_Extent <- extent(-25, 50, 30, 75) 
Europe_project <- stack(crop(Bioclim_Stack, Europe_Extent))

alphaMap <- reclassify(subset(Europe_project,1), c(-Inf,Inf,0))

setwd("D:/SDM_2/SDM")



for(i in c(1:86,127:length(Model_Outs))){
  MyBiomodEnsemble <- BIOMOD_EnsembleModeling(modeling.output =  Model_Outs[[i]],
                                              chosen.models = c(grep("RF", get_built_models(Model_Outs[[i]]), value = TRUE),
                                                                grep("GBM", get_built_models(Model_Outs[[i]]), value = TRUE),
                                                                grep("FDA", get_built_models(Model_Outs[[i]]), value = TRUE),
                                                                grep("GAM", get_built_models(Model_Outs[[i]]), value = TRUE),
                                                                grep("MARS", get_built_models(Model_Outs[[i]]), value = TRUE)),
                                              eval.metric = c("ROC"),
                                              eval.metric.quality.threshold = c(0.8),
                                              em.by = "all",
                                              prob.mean = T,
                                              prob.median = T,
                                              committee.averaging = T,
                                              prob.mean.weight = T,
                                              prob.mean.weight.decay = 'proportional')
  
  if(OLE_Coord_Limits[i,2] < -25 | OLE_Coord_Limits[i,2] == "NaN" ){
    xmin <- -25
  } else {
    xmin <- OLE_Coord_Limits[i, 2]
  }
  if(OLE_Coord_Limits[i,4] > 50 | OLE_Coord_Limits[i,4] == "NaN"){
    xmax <- 50
  } else {
    xmax <- OLE_Coord_Limits[i, 4]
  }
  if(OLE_Coord_Limits[i,6] < 30 | OLE_Coord_Limits[i,6] == "NaN"){
    ymin <- 30
  } else {
    ymin <- OLE_Coord_Limits[i, 6]
  }
  if(OLE_Coord_Limits[i,8] > 75 | OLE_Coord_Limits[i,8] == "NaN"){
    ymax <- 75
  } else {
    ymax <- OLE_Coord_Limits[i, 8]
  }
  
  projection_extent <- extent(xmin, xmax, ymin, ymax)
  Current_Projection <- stack(crop(Bioclim_Stack,projection_extent))
  
  for(j in 1:4){
  names(Current_Projection)[[j]] <- c(paste("layer.",j,sep = ""))
  }
  
  
  myBiomodProj <- BIOMOD_Projection(
    modeling.output = Model_Outs[[i]],
    new.env = Current_Projection,
    proj.name = 'current',
    selected.models = c(grep("RF", get_built_models(Model_Outs[[i]]), value = TRUE),
                        grep("GBM", get_built_models(Model_Outs[[i]]), value = TRUE),
                        grep("FDA", get_built_models(Model_Outs[[i]]), value = TRUE),
                        grep("GAM", get_built_models(Model_Outs[[i]]), value = TRUE),
                        grep("MARS", get_built_models(Model_Outs[[i]]), value = TRUE)),
    binary.meth = 'TSS',
    compress = 'xz',
    clamping.mask = T,
    output.format = '.grd')
  
  
  myBiomodEF <- BIOMOD_EnsembleForecasting(
    EM.output = MyBiomodEnsemble,
    projection.output = myBiomodProj)
  
  Spp_Name <- Model_Outs[[i]]@sp.name
  
  proj <- raster(paste(paste(paste(Spp_Name,"proj_current/proj_current_",sep = "/"),Spp_Name,sep = ""),"_TSSbin.grd",sep = ""))
  
  
  spp_alphaMap <- alphaMap + 
    subset(stack(proj),1)
  
  dir.create(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current_2", Spp_Name, sep = "/"))
  png(paste(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current_2", Spp_Name, sep = "/"),"/1_Raster_Image.png",sep = ""), width=4, height=4, units="in", res=300)
  plot(spp_alphaMap)
  dev.off()
  
  writeRaster(spp_alphaMap,filename = paste(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current_2", Spp_Name, sep = "/"),"/2_Raster_Tif",sep = ""),format = "GTiff", overwrite = TRUE)
  proj <- raster(paste(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current_2", Spp_Name, sep = "/"),"/2_Raster_Tif.tif",sep = ""))
  proj <- extend(x = proj, y = Europe_Extent)
  proj <- reclassify(proj, c(NA,NA,0))
  
  if(i == 1 ){
    All_alphaMap <-
      alphaMap +
      subset(stack(proj), 1)
  } else {
    All_alphaMap <- 
      All_alphaMap +
      subset(stack(proj), 1)
  }
}

Bee_Alpha <- reclassify(subset(Europe_project,1), c(-Inf,Inf,0))
for(i in 1:length(Model_Outs)){
bee_ras <- raster(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current_2/","/2_Raster_Tif.tif", sep = Model_Outs[[i]]@sp.name))
bee_ras <- extend(bee_ras, Europe_Extent)
bee_map <- reclassify(bee_ras, c(NA,NA,0))
Bee_Alpha <- Bee_Alpha + bee_map 
}


plot(Bee_Alpha)

png("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current_2/All_Bees_Raster_Image.png", width=4, height=4, units="in", res=300)
plot(Bee_Alpha)
dev.off()
writeRaster(Bee_Alpha,"C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current/All_Bees_Raster_Tif", format = "GTiff", overwrite = T)


setwd("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs")


