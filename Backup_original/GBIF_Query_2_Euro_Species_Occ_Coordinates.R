rm(list=ls())
require(dplyr)
require(tidyr)
require(rgbif)
require(magrittr)

load("RData_European_Bee_Species.RData")
Species<-European_Bee_Species


### Extracting the decimal coordinates of the occurrences of each european Species from GBIF.
## have also extended the code to extract extra information from the meta-data that may infrom the data cleaning process
for(i in 1:nrow(Species)){
  Data <- occ_search(taxonKey = Species[i,2], hasCoordinate = TRUE,hasGeospatialIssue = FALSE,limit = 150000, return = "data")
  GBIF <- Data %>%
    dplyr::select(one_of("species", "decimalLongitude", "decimalLatitude", "countryCode", "individualCount",
                         "gbifID", "family", "taxonRank", "coordinateUncertaintyInMeters", "year",
                         "basisOfRecord", "institutionCode", "datasetName", "geodeticDatum",
                         "dataGeneralizations","verbatimSRS","verbatimCoordinateSystem","georeferenceRemarks"))
  colnames(GBIF)[2:3]<-c("decimallongitude","decimallatitude")
  assign(paste(Species[i,1],"GBIF_Data",sep="_"),GBIF)
}
  

GBIF_Data<-setNames(lapply(ls(pattern ="GBIF_Data*"), function(x) get(x)),(ls(pattern="GBIF_Data*")))

save(file = "RData_European_Bee_Species_GBIF_Data.RData", GBIF_Data)
 

GBIF_Species <- c()                                             #### Identify mis-matches between the scientific name and name given by GBIF
for(i in 1:length(GBIF_Data)){                                   ### Could be indicative of species synonyms or outdated species names 
GBIF_Species[i] <- GBIF_Data[[i]][["species"]][1]
}

Species_1 <- cbind(Species,GBIF_Species)
Species_1$Scientific_Name <- as.character(Species$Scientific_Name)
Species_1$GBIF_Species <- as.character(Species$GBIF_Species)

rm_Species <- Species_1 %>%
  filter(Scientific_Name != GBIF_Species)

keep_Species <- Species_1 %>%
  filter(Scientific_Name == GBIF_Species)

#### reviewing the species that have been removed due to mismatches only one -- sephcodes gibbus can be kept - name is a synonym but is still a distinct bee species that can be included.

GBIF_Data[["Sphecodes gibbus_GBIF_Data"]][["species"]] = "Sphecodes gibbus"

GBIF_Species_2 <- c()                                             #### Identify mis-matches between the scientific name and name given by GBIF
for(i in 1:length(GBIF_Data)){                                   ### Could be indicative of species synonyms or outdated species names 
  GBIF_Species_2[i] <- GBIF_Data[[i]][["species"]][1]
}

Species_2 <- cbind(Species,GBIF_Species_2)
Species$Scientific_Name <- as.character(Species_2$Scientific_Name)
Species$GBIF_Species <- as.character(Species_2$GBIF_Species)

flags <- Species_2$Scientific_Name == Species_2$GBIF_Species_2

GBIF_Data <- GBIF_Data[flags]

save(file = "RData_European_Bee_Species_GBIF_Data.RData", GBIF_Data)
