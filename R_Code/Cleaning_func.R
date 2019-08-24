rm(list=ls())
require(dplyr)
require(tidyr)
require(rgbif)
require(magrittr)
require(countrycode)
require(CoordinateCleaner)

## Initally flag those coordinates that are a bit dubious.

dat<-GBIF_Data%>%
    filter(!is.na(decimalLongitude)) %>%
    filter(!is.na(decimalLatitude))
dat$countryCode <-  countrycode(dat$countryCode, origin = 'iso2c', destination = 'iso3c')
dat <- clean_coordinates(x = dat, lon = "decimalLongitude", lat = "decimalLatitude",
    countries = "countryCode", species = "species", 
    tests = c("capitals", "centroids", "equal","gbif", "institutions",
    "zeros", "countries","seas", "duplicates"), seas_scale = 110)

Cleaned_Data<-dat %>% filter(.summary==TRUE)

### Filter out the records that have low precision

Cleaned_Data<-Cleaned_Data %>% filter(coordinateUncertaintyInMeters/1000 <= 5 | is.na(coordinateUncertaintyInMeters))

### Remove records that are from unsuitable data sources e.g fossils 

Cleaned_Data<-Cleaned_Data %>% filter( basisOfRecord == "HUMAN_OBSERVATION" 
    | basisOfRecord == "OBSERVATION" | basisOfRecord == "PRESERVED_SPECIMEN")

## filter out records with absence records and high individual count records also

Cleaned_Data<-Cleaned_Data %>% filter(individualCount > 0 | is.na(individualCount)) %>%
    filter(individualCount < 99 | is.na(individualCount))

#### filter out the old collection records

Cleaned_Data<-Cleaned_Data %>% filter(year > 1945)

### Now most of the records have been cleaned we can re-run an outlier analysis to remove some 
#final spurious records -- species with less than 7 unique records cannot be tested

##########################
###CHECKPOINT
###########################

for(i in 1:unique(Cleaned_Data$species)){
    clean_species<-unique(Cleaned_Data$species)[i]
    
    if((nrow(Cleaned_Data)) >= 7){
        clean<- Clean_Data[[paste(GBIF_Data[[i]][["species"]][1],"clean_Data",sep = "_")]] %>%
            cc_outl(mltpl = 12) # may alter number
        assign(paste(GBIF_Data[[i]][["species"]][1],"clean_Data",sep = "_"),clean)
    }
}
    
Clean_Data<-setNames(lapply(ls(pattern ="clean_Data*"), function(x) get(x)),(ls(pattern="clean_Data*")))

save(Clean_Data, file = "RData_European_Bee_Species_Clean_Data.RData") ## save clean data
rm(list=ls())                                                    ## Clear Environment
load("RData_European_Bee_Species_Clean_Data.RData")              ## Reload data after automated cleaning


# finally remove species that have less than 25 occurrences remaining after inital cleaning


for(i in 1:length(Clean_Data)){
  if ((NROW(Clean_Data[[paste(Clean_Data[[i]][["species"]][1],"clean_Data",sep = "_")]])) > 0){ 
    assign(paste(Clean_Data[[i]][["species"]][1],"clean_Data",sep = "_"),Clean_Data[[paste(Clean_Data[[i]][["species"]][1],"clean_Data",sep = "_")]])
  }}


Clean_Data<-setNames(lapply(ls(pattern ="clean_Data*"), function(x) get(x)),(ls(pattern="clean_Data*"))) ### 405 species remain with over 25 occurences after inital cleaning 


save(Clean_Data, file = "RData_European_Bee_Species_Clean_Data.RData") ## Save the clean data RData



