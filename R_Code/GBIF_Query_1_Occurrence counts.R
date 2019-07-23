rm(list=ls())
require(dplyr)
require(tidyr)
require(rgbif)
require(taxize)

print("Collecting names from PREDICTS data")

PREDICTS <- readRDS("../../taxa-2019-05-09-02-34-47.rds")       #### load PREDICTS Taxa list
PREDICTS$Scientific_Name <- paste(PREDICTS$Genus,PREDICTS$Species)      #### create column with scientific name
PREDICTS<- PREDICTS %>%
  drop_na(Species)                                                      ### rm columns with NA
Bee_Families <- c("Andrenidae") #,"Apidae","Colletidae","Halictidae","Megachilidae","Melittidae","Stenotritidae")  ## vector with the names of the seven bee families
PREDICTS <- PREDICTS %>% 
  filter(Family %in% Bee_Families)                ## Subset taxa list so that it only contains species in the bee families 
Bee_Species <- data.frame(unique(PREDICTS$Scientific_Name))           ### create data frame with just the species list from PREDICTS
Bee_Species <- Bee_Species[Bee_Species!=" "] # Removes spaces. Also converts to a vector for later ease of appending.
names(Bee_Species) <- "Scientific_Name"

European_Countries<-c("AT","AD","AL","AX","BA","BY","BE","BG","CY","GG","GI","JE","HR","CH","CY","CZ","DK","EE","FI","FR","DE",
                      "GR","HU","HR","IE","IT","LI","LV","LT","LU","ME","MC","MT","MK","MT","MD","NL","NO","PL","PT","VA","RO",
                      "RU","SK","SI","SM","ES","SE","UA","GB")  ### Vector of European CountryCodes  

## get the family IDs for the three databases we are going to query to get list of species names

TaxonKey <- taxize::get_ids(names = Bee_Families, db = c("col","gbif"))
TaxonKey_Bee <- data.frame(Bee_Families, TaxonKey[[1]], TaxonKey[[2]])
TaxonKey_Bee <- TaxonKey_Bee[,c(1:2,8)]
colnames(TaxonKey_Bee)[2:3] <- c("col", "gbif")

## query each database in turn 

print("Querying GBIF and Catalogue of life to collect full and distinct list of species names")
for(i in 1:length(Bee_Families)){
col_bee_down <- col_downstream(id = TaxonKey_Bee[i,2], downto = "species", intermediate = FALSE, extant_only = TRUE)         ## Catelouge of Life
gbif_bee_down <- gbif_downstream(key = TaxonKey_Bee[i,3], downto = "species", intermediate = FALSE, start = 1, limit = 100000000) ## Gbif database
Species <- rbind(col_bee_down[[1]][2],gbif_bee_down$name)
append(Species[[1]], Bee_Species) # Adds the new species list to the overall species list
}

Bee_Species <- data.frame(Bee_Species) # Converts to data frame
names(Bee_Species) <- "Scientific_Name" # Renames the column 

Bee_Species <- Bee_Species %>%              ### Keep only the unique species -- PREDICTS Database adds a single species -- Halictus gemmeus
  distinct(Scientific_Name)

print("List generated.")

save(file = "RData_All_Bee_Speices.RData", Bee_Species)

##generated the taxon key for each species in the list so that I can query GBIF

UsageKey<-c()
for(i in 1:nrow(Bee_Species)){
  Taxon <- name_backbone(name=(Bee_Species[i,1]),rank="Species",kingdom="Animalia")
  TaxonKey <- Taxon$usageKey
  UsageKey[i]<-TaxonKey
}

Bee_TaxonKey <- data.frame(Bee_Species$Scientific_Name, UsageKey)


Occurrence<-c()
for(i in 1:nrow(Bee_TaxonKey)){
  Occurrence[i]<-occ_count(taxonKey = (Bee_TaxonKey[i,2]),georeferenced = TRUE)
}

Bee_Occurrence_Count<-data.frame(Bee_TaxonKey,Occurrence)
colnames(Bee_Occurrence_Count)[1:3]<-c("Scientific_Name","GBIF_TaxonKey","GBIF_Occurrence_Count")
Bee_Occurrence_Count <- Bee_Occurrence_Count %>% filter(GBIF_Occurrence_Count > 20) # occurance count! Make variable


###Queried GBIF to find the occurrence data for each species
##Then going to query again but now only those species that occur in europe --- Here I think it becomes a bit patchy with GBIF as not all occurrences in the GBIF database
#have been classified a continent so will be missing a lot of data. So I have got the country codes for all european countries and then summed the occurrence data from 
#each country.Hopefully identifying those species which have sufficient occurrence data in Europe.

print("Searching for all occurance records in specified geographic area.")

Europe_Occ<-c()
Country_Count<-c()
Full_Count<-c()
for(i in 1:nrow(Bee_Occurrence_Count)){
  Europe=occ_search(taxonKey = Bee_Occurrence_Count[i,2], hasCoordinate = TRUE, hasGeospatialIssue = FALSE, country = c(European_Countries), return = "meta")
  for(j in 1:50){
    Europe_Continent=Europe[[j]][["count"]]
    Country_Count[j]=sum(Europe_Continent)
  }
  Full_Count=sum(Country_Count)
  Europe_Occ[i]<-sum(Full_Count)
}

Europe_Occurrence_Count<-cbind(Bee_Occurrence_Count,Europe_Occ)
colnames(Europe_Occurrence_Count)[4]<-c("GBIF_Europe_Occurrence_Count")

European_Bee_Species <- Europe_Occurrence_Count %>%
  filter(GBIF_Europe_Occurrence_Count > 20) %>%
  arrange(Scientific_Name)

save(file ="RData_European_Bee_Species.RData", European_Bee_Species)

print ("Occurance data gathered")
