rm(list = ls())
require(dplyr)
require(tidyr)
require(biomod2)
require(magrittr)

### the method by which you generate psuedo-absences depends on the methodology you want to implenment to generate your SDM - as I want to 
## produce an ensemble of SDMs I will nee to generate pseudo-absences in a few different ways.
## For regression methodologies like GLM, GAM I want to randomly generate pseudo- absences numbering about 10 times the number of presences
## for MARS regression method - random with a number of replicates of PAs could be a higher prevalence
## MDA, CTA, BRT, RF - for low number of presences <100 2 degree geographic exclusion when greater "SRE" method better - 
## for CTA, BRT, RF - same number of PA as presence points


load("RData_European_Bee_Species_Clean_Data.RData")
load("RData_European_Bee_Species_Thinned_Data_5km.RData")

data <- c()      ### collate the inital cleaned data into a single dataframe
for( i in 1:length(Clean_Data)){
  data <- rbind(data, Clean_Data[[i]][c("species","decimallongitude","decimallatitude")])
}
data$presence <- 1     ### code presence as 1

coordinate <- data %>% spread(species, presence)  ### spread the data so that each row is the coordinates at while the presence is at
                                                   ## and each column is a species 

coord_occ <- c()                                   #### evaluate how many occurrences occur at the same coordinates 
for(i in 1:nrow(coordinate)){                        ### quite a lot of coordinates overlap with a number of species 
  coord_occ[i] <- sum(coordinate[i,c(3:407)])
}
       
hist(coord_occ,breaks = 25)                            ### looking at the histogram coord_occ is the number of species that share that coordinate
length(subset(coord_occ, coord_occ > 75))               ## at what point do we deem a coordinare to be comprehensively surveyed enough to then assume a true absence to it?
                                                         ## is there a way to weight the points in the generation of psuedo-absences so that those coordinates that overlap most are weighted more highly?
                                                         ## some of these points might be the centre of a region


for(i in 1:nrow(coordinate)){                             #### so this is basically if there are more than 75 species recorded at a single coordinate
if(sum(coordinate[i,c(3:407)], na.rm = TRUE) > 75){        ### it should be marked down as an absence as we can assume that this coordinate has been more 
  for(j in 3:407){                                         ### comprehensively surveyed therefore more likely to be a tre absence.
if(is.na(coordinate[i,j])){ 
  coordinate[i,j] = 0
}}}}

wabs_coordinates <- coordinate %>% gather(species, presence, -c("decimallongitude","decimallatitude"), na.rm = TRUE)


for(i in 1:length(unique(wabs_coordinates$species))){
absences <- subset(wabs_coordinates, species == unique(wabs_coordinates$species)[i] & presence == 0)
assign(paste(unique(wabs_coordinates$species)[i], "Occ_Absence_Data", sep = "_"),absences)
}

Occ_Absence_Data<-setNames(lapply(ls(pattern ="Occ_Absence_Data*"), function(x) get(x)),(ls(pattern="Occ_Absence_Data*")))

save(file = "RData_European_Bee_Species_Occ_Absence_Data.RData", Absence_Data)
