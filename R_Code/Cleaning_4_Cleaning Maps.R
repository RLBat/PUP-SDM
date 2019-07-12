rm(list=ls())
require(dplyr)
require(tidyr)
require(ggplot2)
require(doParallel)

load("RData_European_Bee_Species_GBIF_Data.RData")
load("RData_European_Bee_Species_Clean_Data.RData")
load("RData_European_Bee_Species_Finish_Data.RData")
load("RData_European_Bee_Species_Thinned_Data_5km.RData")

## Can see if we can create some before and after comparison maps.


wm<-map_data("world") %>% filter(region != "Antartica") %>% fortify() # This gets the polygon data for the world map


registerDoParallel( cores = 4 )


foreach(i = 1:length(Clean_Data),
          .packages = c("dplyr","ggplot2","maps","tidyr")) %dopar% {
  before<-ggplot()+ coord_fixed()+
    geom_map(data =wm, map = wm,
             aes(group = group, map_id= region),
             fill = "darkgrey")+                        ### if you want to add country borders  (colour= "#7f7f7f", size = 0.5)
    geom_point(data = GBIF_Data[[paste(Clean_Data[[i]][["species"]][1],"GBIF_Data",sep = "_")]], aes(x = decimallongitude, y = decimallatitude),
               colour = "darkred", size = 1)+
    lims(x = c(floor(min(GBIF_Data[[paste(Clean_Data[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallongitude"]])-10),     ### limit the extent of the map 
               ceiling(max(GBIF_Data[[paste(Clean_Data[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallongitude"]]))+10),  ### to capture the points
         y = c(floor(min(GBIF_Data[[paste(Clean_Data[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallatitude"]])-10),
               ceiling(max(GBIF_Data[[paste(Clean_Data[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallatitude"]]))+10))+
    theme_bw()
  after<-ggplot()+ coord_fixed()+
    geom_map(data =wm, map = wm,
             aes(group = group, map_id= region),
             fill = "darkgrey")+
    geom_point(data = Clean_Data[[i]], aes(x = decimallongitude, y = decimallatitude),
               colour = "blue", size = 1)+
    lims(x = c(floor(min(GBIF_Data[[paste(Clean_Data[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallongitude"]])-10),
               ceiling(max(GBIF_Data[[paste(Clean_Data[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallongitude"]]))+10), 
         y = c(floor(min(GBIF_Data[[paste(Clean_Data[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallatitude"]])-10),
               ceiling(max(GBIF_Data[[paste(Clean_Data[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallatitude"]]))+10))+
    theme_bw()
  ggsave(paste((paste("Cleaning_6_Cleaning_Maps/",Clean_Data[[i]][["species"]][1],sep="")),"/1_before.png",sep = ""),before,device = "png", height = 10, width = 10,dpi=300) ## save the before, after, and polish maps 
  ggsave(paste((paste("Cleaning_6_Cleaning_Maps/",Clean_Data[[i]][["species"]][1],sep = "")),"/2_after.png",sep = ""),after,device = "png", height = 10, width = 10,dpi=300)
}

registerDoSEQ()



## Now for the Finished cleaning map

registerDoParallel( cores = 4 )

foreach(i = 1:length(Finish_Data),
        .packages = c("dplyr","ggplot2","maps","tidyr")) %dopar% {
polish<-ggplot()+ coord_fixed()+
  geom_map(data =wm, map = wm,
           aes(group = group, map_id= region),
           fill = "darkgrey")+                        ### if you want to add country borders  (colour= "#7f7f7f", size = 0.5)
  geom_point(data = Finish_Data[[i]], aes(decimallongitude, decimallatitude),
             colour = "green", size = 1)+
  lims(x = c(floor(min(GBIF_Data[[paste(Finish_Data[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallongitude"]])-10),     ### limit the extent of the map 
             ceiling(max(GBIF_Data[[paste(Finish_Data[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallongitude"]]))+10),  ### to capture the points
       y = c(floor(min(GBIF_Data[[paste(Finish_Data[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallatitude"]])-10),
             ceiling(max(GBIF_Data[[paste(Finish_Data[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallatitude"]]))+10))+
  theme_bw()
ggsave(paste((paste("Cleaning_6_Cleaning_Maps/",Finish_Data[[i]][["species"]][1],sep="")),"/4_polish.png",sep = ""),polish,device = "png", height = 10, width = 10,dpi=300)
}

registerDoSEQ()
### final Map showing the Thinned data at 5km 

for(i in 1:length(Thinned_Data_5km)){
  thinned<-ggplot()+ coord_fixed()+
    geom_map(data =wm, map = wm,
             aes(group = group, map_id= region),
             fill = "darkgrey")+                        ### if you want to add country borders  (colour= "#7f7f7f", size = 0.5)
    geom_point(data = Thinned_Data_5km[[i]], aes(decimallongitude, decimallatitude),
               colour = "orange", size = 1)+
    lims(x = c(floor(min(GBIF_Data[[paste(Thinned_Data_5km[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallongitude"]])-10),     ### limit the extent of the map 
               ceiling(max(GBIF_Data[[paste(Thinned_Data_5km[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallongitude"]]))+10),  ### to capture the points
         y = c(floor(min(GBIF_Data[[paste(Thinned_Data_5km[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallatitude"]])-10),
               ceiling(max(GBIF_Data[[paste(Thinned_Data_5km[[i]][["species"]][1],"GBIF_Data",sep = "_")]][["decimallatitude"]]))+10))+
    theme_bw()
  ggsave(paste((paste("Cleaning_6_Cleaning_Maps/",Thinned_Data_5km[[i]][["species"]][1],sep="")),"/8_Thinned.png",sep = ""),thinned,device = "png", height = 10, width = 10,dpi=300)
}

