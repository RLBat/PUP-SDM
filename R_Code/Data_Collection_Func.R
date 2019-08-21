# Data_Collection_Func.R - Contains all functions for the package *insert name* that deal with data input and collection from outside sources.
# Author: Rachel Bates (adapted from code by Patrick Walkden)
# Date: 21/06/19

require(dplyr)
require(tidyr)
require(rgbif)
require(taxize)

#' Using a list of species (or if other taxa it first generates a list of species), checks for synonyms with GBIF and catalogue of life, gets all occurance data (for a specific geographic area), and associates the required data with it.
#' 
#' @param taxon_rank The taxon level for which you are searching. 1=Species, indicates using binomial species names. 0 indicates using any taxanomic rank Genus or higher. Defaults to 0.
#' @param Kingdom The kingdom to which your chosen species belong. Options are Animalia or Plantae. Defaults to Animalia.
#' @param taxa_names A vector or single column data frame of the names of taxa to search (e.g. bee families). If using species the binomial names should be given (separated with a space).
#' @param input_data A single column data frame species list to check against GBIF and catalogue of life to identify any synonyms. Defaults to FALSE. Only use where you have a specific species list under other umbrella terms. If only looking for certain species, use taxa_names.
#' @param min_occur The minimum number of occurance records for a species for it to be included. Defaults to 20.
#' @param geo_area The geographical area you wish to model. This should be a vector of country codes.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' 
GBIFSpecies <- function(taxon_rank=0, Kingdom="Animalia", taxa_names, min_occur=20, input_data=FALSE, geo_area){

    # Step one, sort out what the species are for different taxanomic ranks:

    if (Kingdom=="animalia"){
        Kingdom <- "Animalia"
    }
    if (Kingdom=="plantae"){
        Kingdom <- "Plantae"
    }
    if (Kingdom=="Plantae"|Kingdom=="Animalia"){
        return
    } else {
        stop("Please enter a valid Kingdom")
    }
    
    # Still need to add something to allow species lists to be imported and maybe alter future steps
    # if (taxon_rank==0){
    #     print("hello")
    # }else{
    #     print("goodbye")
    # }

    print("Checking species list")

    if (input_data!=FALSE){
        Species_list <- input_data
    } else {
        Species_list <- c()
    }
    ## get the family IDs for the three databases we are going to query to get list of species names

    TaxonKey <- taxize::get_ids(names = taxa_names, db = c("col","gbif"))
    TaxonKey <- data.frame(taxa_names, TaxonKey[[1]], TaxonKey[[2]])
    TaxonKey <- TaxonKey[,c(1:2,8)]
    colnames(TaxonKey)[2:3] <- c("col", "gbif")

    ## query each database in turn 

    print("Querying GBIF and Catalogue of life to collect full and distinct list of species names. This may take several minutes.")
    for(i in 1:length(taxa_names)){
    col_names <- col_downstream(id = TaxonKey[i,2], downto = "species", intermediate = FALSE, extant_only = TRUE)         ## Catelouge of Life
    gbif_names <- gbif_downstream(key = TaxonKey[i,3], downto = "species", intermediate = FALSE, start = 1, limit = 100000000) ## Gbif database
    Query_Species <- rbind(col_names[[1]][2],gbif_names$name)
    Query_Species <- unname(unlist(Query_Species))
    Species_list<- append(Query_Species, Species_list) # Adds the new species list to the overall species list
    }

    Species_list <- data.frame(Species_list) # Converts to data frame
    names(Species_list) <- "Scientific_Name" # Renames the column 

    Species_list <- Species_list %>%  # Keep only the unique species
    distinct(Scientific_Name)

    print("List generated.")

    #save(file = "RData_All_Bee_Speices.RData", Species_list)

    ##generated the taxon key for each species in the list so that I can query GBIF

    print("Fetching species IDs from GBIF")
    UsageKey<-c()
    for(i in 1:nrow(Species_list)){
    Taxon <- name_backbone(name=(Species_list[i,1]),rank="Species",kingdom=Kingdom) #User input for which Kingdom to search
    UsageKey[i] <- Taxon$usageKey
    }

    Species_list <- data.frame(Species_list$Scientific_Name, UsageKey)

    print("Counting available occurrence data")
    Occurrence<-c()
    for(i in 1:nrow(Species_list)){
    Occurrence[i]<-occ_count(taxonKey = (Species_list[i,2]),georeferenced = TRUE)
    }

    Species_list<-data.frame(Species_list,Occurrence)
    colnames(Species_list)[1:3]<-c("Scientific_Name","GBIF_TaxonKey","GBIF_Occurrence_Count")
    Species_list <- Species_list %>% filter(GBIF_Occurrence_Count > min_occur) # Removes all species that have fewer than min_occur records
    print("Species with too few records have been removed")

    ###Queried GBIF to find the occurrence data for each species
    ##Then going to query again but now only those species that occur in europe --- Here I think it becomes a bit patchy with GBIF as not all occurrences in the GBIF database
    #have been classified a continent so will be missing a lot of data. So I have got the country codes for all european countries and then summed the occurrence data from 
    #each country.Hopefully identifying those species which have sufficient occurrence data in Europe.

    print("Searching for all occurance records in specified geographic area.")

    Geo_Occurrence<-c()
    Country_Count<-c()
    for(i in 1:nrow(Species_list)){
    Area_Occurrence=occ_search(taxonKey = Species_list[i,2], hasCoordinate = TRUE, hasGeospatialIssue = FALSE, country = c(geo_area), return = "meta")
    for(j in 1:length(geo_area)){
        Country_Count[j]=sum(Area_Occurrence[[j]][["count"]])
    }
    Geo_Occurrence[i]<-sum(sum(Country_Count))
    }

    Species_list<-cbind(Species_list,Geo_Occurrence)
    colnames(Species_list)[4]<-c("GBIF_Area_Occurrence_Count")
  
    Species_list <- Species_list %>%
    filter(GBIF_Area_Occurrence_Count > min_occur) %>% # Removes all species with less than min_occur records in the geogrphic area
    arrange(Scientific_Name)

    #save(file ="RData_European_Bee_Species.RData", European_Bee_Species)

    print ("Occurance data gathered. Data contained in Species_list")
    Species_list<<-Species_list
    return(Species_list)
}


geo_area<-c("AT","AD","AL","AX","BA","BY","BE","BG","CY","GG","GI","JE","HR","CH","CY","CZ","DK","EE","FI","FR","DE",
                      "GR","HU","HR","IE","IT","LI","LV","LT","LU","ME","MC","MT","MK","MT","MD","NL","NO","PL","PT","VA","RO",
                      "RU","SK","SI","SM","ES","SE","UA","GB")  ### Vector of European CountryCodes  