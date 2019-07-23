# Data_Collection_Func.R - Contains all functions for the package *insert name* that deal with data input and collection from outside sources.
# Author: Rachel Bates (adapted from code by Patrick Walkden)
# Date: 21/06/19


#' Using a list of species (or if other taxa it first generates a list of species), checks for synonyms with GBIF and catalogue of life, gets all occurance data (for a specific geographic area), and associates the required data with it.
#' 
#' @param taxon_rank The taxon level for which you are searching. 1=Species, indicates using binomial species names. 0 indicates using any taxanomic rank Genus or higher.
#' @param taxa_names A vector or single column data frame of the names of taxa to search (e.g. bee families). If using species the binomial names should be given (separated with a space).
#' @param input_data A single column data frame species list to check against GBIF and catalogue of life to identify any synonyms. Defaults to FALSE. Only use where you have a specific species list under other umbrella terms. If only looking for certain species, use taxa_names.
#' @param min_occur The minimum number of occurance records for a species for it to be included. Defaults to 20.
#' @param geo_area The geographical area you wish to model. This should be a vector of country codes.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' 
GBIFSpecies <- function(taxon_rank, taxa_names, min_occur=20, input_data=FALSE, geo_area){

    # Step one, sort out what the species are for different taxanomic ranks:

    if (taxon_rank==0){
        print("hello")
    }else{
        print("goodbye")
    }

    print("Checking species list")


    ## get the family IDs for the three databases we are going to query to get list of species names

    TaxonKey <- taxize::get_ids(names = taxa_names, db = c("col","gbif"))
    TaxonKey <- data.frame(taxa_names, TaxonKey[[1]], TaxonKey[[2]])
    TaxonKey <- TaxonKey[,c(1:2,8)]
    colnames(TaxonKey)[2:3] <- c("col", "gbif")

    ## query each database in turn 

    print("Querying GBIF and Catalogue of life to collect full and distinct list of species names")
    for(i in 1:length(taxa_names)){
    col_bee_down <- col_downstream(id = TaxonKey[i,2], downto = "species", intermediate = FALSE, extant_only = TRUE)         ## Catelouge of Life
    gbif_bee_down <- gbif_downstream(key = TaxonKey[i,3], downto = "species", intermediate = FALSE, start = 1, limit = 100000000) ## Gbif database
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
    Europe=occ_search(taxonKey = Bee_Occurrence_Count[i,2], hasCoordinate = TRUE, hasGeospatialIssue = FALSE, country = c(geo_area), return = "meta")
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

}