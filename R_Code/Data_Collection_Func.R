# Data_Collection_Func.R - Contains all functions for the package *insert name* that deal with data input and collection from outside sources.
# Author: Rachel Bates (adapted from code by Patrick Walkden)
# Date: 21/06/19

require(dplyr)
require(tidyr)
require(rgbif)
require(taxize)
require(magrittr)

#' Using a list of species (or if other taxa it first generates a list of species), checks for synonyms with GBIF and catalogue of life, gets all occurance data (for a specific geographic area), and associates the required data with it.
#' 
#' @param taxon_rank The taxon level for which you are searching. 1=Species, indicates using binomial species names. 0 indicates using any taxanomic rank Genus or higher. Defaults to 0.
#' @param Kingdom The kingdom to which your chosen species belong. Options are Animalia or Plantae. Defaults to Animalia.
#' @param taxa_names A vector or single column data frame of the names of taxa to search (e.g. bee families). If using species the binomial names should be given (separated with a space).
#' @param input_data A single column data frame species list to check against GBIF and catalogue of life to identify any synonyms. Defaults to NULL. Only use where you have a specific species list under other umbrella terms. If only looking for certain species, use taxa_names.
#' @param min_occur The minimum number of occurance records for a species for it to be included. Defaults to 20.
#' @param geo_area The geographical area you wish to model. This should be a vector of country codes.
#' @param 
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' 
GBIFSpecies <- function(taxon_rank=0, Kingdom="Animalia", taxa_names, min_occur=20, input_data=NULL, geo_area, keep_synonym==TRUE){

    # Ensure all parameters have been provided
    if (missing(geo_area)){
        stop(geo_area not found. Please provide an area to sample as a vector of country codes.)
    }
    if (missing(taxa_names)){
        stop(taxa_names not found. Please provide a vector of taxa.)
    }

    # Ensure that a valid kingdom name has been entered
    if (Kingdom=="animalia"|Kingdom=="animals"|Kingdom=="Animals"){
        Kingdom <- "Animalia"
    }
    if (Kingdom=="plantae"|Kingdom=="plants"|Kingdom=="Plants"){
        Kingdom <- "Plantae"
    }
    if (Kingdom=="Plantae"|Kingdom=="Animalia"){
        return
    } else {
        stop("Please enter a valid Kingdom")
    }
    
    # If species names inputted, copy to input data
    if (taxon_rank==1){ 
        input_data <- taxa_names
    }

    # Creates a vector to place species if none provided
    if (missing(input_data)){
        Species_list <- c()
    } else {
        Species_list <- input_data
    }
    
    ## Gets list of species if rank genus or higher is used
    if (taxon_rank==0){
        print("Gathering species list")
        ## get the family IDs for the three databases we are going to query to get list of species names
        TaxonKey <- taxize::get_ids(names = taxa_names, db = c("col","gbif"))
        TaxonKey <- data.frame(taxa_names, TaxonKey[[1]], TaxonKey[[2]])
        TaxonKey <- TaxonKey[,c(1:2,8)]
        colnames(TaxonKey)[2:3] <- c("col", "gbif")
        
        print("Querying GBIF and Catalogue of life to collect full and distinct list of species names. This may take several minutes.")
        for(i in 1:length(taxa_names)){
            col_names <- col_downstream(id = TaxonKey[i,2], downto = "species", intermediate = FALSE, extant_only = TRUE)         ## Catelouge of Life
            gbif_names <- gbif_downstream(key = TaxonKey[i,3], downto = "species", intermediate = FALSE, start = 1, limit = 100000000) ## Gbif database
            Query_Species <- rbind(col_names[[1]][2],gbif_names$name)
            Query_Species <- unname(unlist(Query_Species))
            Species_list<- append(Query_Species, Species_list) # Adds the new species list to the overall species list
            print("List generated.")
        }
    }

    Species_list <- data.frame(Species_list) # Converts to data frame
    names(Species_list) <- "Scientific_Name" # Renames the column 

    Species_list <- Species_list %>%  # Keep only the unique species
    distinct(Scientific_Name)
    
    ##generating the taxon key for each species in the list so that I can query GBIF

    print("Fetching species IDs from GBIF")
    Species_list <- data.frame(Species_list$Scientific_Name, UsageKey=NA)
    for(i in 1:nrow(Species_list)){
        Species_list$UsageKey[i] <- name_backbone(name=(Species_list[i,1]),rank="Species",kingdom=Kingdom)$usageKey
    }

    print("Counting available occurrence data")
    Occurrence<-c()
    for(i in 1:nrow(Species_list)){
    Occurrence[i]<-occ_count(taxonKey = (Species_list[i,2]),georeferenced = TRUE)
    }

    Species_list<-data.frame(Species_list,Occurrence)
    colnames(Species_list)[1:3]<-c("Scientific_Name","GBIF_TaxonKey","GBIF_Occurrence_Count")
    Species_list <- Species_list %>% filter(GBIF_Occurrence_Count > min_occur) # Removes all species that have fewer than min_occur records
    print("Species with too few records have been removed")

    ###Querying GBIF to find the occurrence data for each species
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

    print ("Gathering occurance data")

    ### From GBIF 2

    GBIF_Data<-data.frame(species=NA, decimalLongitude=NA, decimalLatitude=NA, countryCode=NA, individualCount=NA,
        gbifID=NA, family=NA, taxonRank=NA, coordinateUncertaintyInMeters=NA, year=NA, basisOfRecord=NA, 
        institutionCode=NA, datasetName=NA, geodeticDatum=NA, verbatimSRS=NA,verbatimCoordinateSystem=NA)
    
    for(i in 1:nrow(Species)){
        Data <- occ_search(taxonKey = Species[i,2], hasCoordinate = TRUE,hasGeospatialIssue = FALSE,limit = 150000, return = "data")
        Data <- Data %>% dplyr::select(one_of("species", "decimalLongitude", 
            "decimalLatitude", "countryCode", "individualCount",
            "gbifID", "family", "taxonRank", "coordinateUncertaintyInMeters", "year",
            "basisOfRecord", "institutionCode", "datasetName", "geodeticDatum",
            "verbatimSRS","verbatimCoordinateSystem"))
        GBIF_Data<-rbind(GBIF_Data, Data)
    }
    
    ### Identify mis-matches between the scientific name and name given by GBIF
    ### Could be indicative of species synonyms or outdated species names
    if (keep_synonym == FALSE){
            print("Removing species with name mis-matches")
            GBIF_Species <- unique(GBIF_Data$species)                                           

            Species <- cbind(Species,GBIF_Species)
            Species$Scientific_Name <- as.character(Species$Scientific_Name)
            Species$GBIF_Species <- as.character(Species$GBIF_Species)

            rm_Species <- Species %>% filter(Scientific_Name != GBIF_Species)
            print(paste("Removing ", nrow(rm_Species), " species records due to mismatched names", sep=""))
            #### CURRENTLY DOESN'T WORK TO REMOVE SPECIES ###
            GBIF_Data <- GBIF_Data[Species$Scientific_Name == Species$GBIF_Species]
    }
    
    return(GBIF_Data)
}