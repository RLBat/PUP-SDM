
require(ggplot2)
require(dplyr)

#' Using a list of species (or if other taxa it first generates a list of species), checks for synonyms with GBIF and catalogue of life, gets all occurance data (for a specific geographic area), and associates the required data with it.
#' 
#' @param data The data you wish to map. Will be an output from either Data_Collection_Func or Cleaning_Func
#' @param output_path The path to the directory in which to save the map/s created
#' @param input_species The species that you wish to generate maps for. Will use all species in file if not given. 
#' @param filename End of filename after species name. Defaults to "Map"
#' @return Map/s showing the occurrence data currently held for the specified species
#' @examples
#' 

# Neo_Species_list<-readRDS("../../PurvisProj/Coding/SDM/Data/neo_GBIF_checkpoint.rds")
# Neo_Species_list<-readRDS("../Data/neo_GBIF_checkpoint.rds")

Distribution_Map <- function(data, output_path, input_species=NULL, filename="Map", country=FALSE){
    ## Add if statement for if specific species are given
    data<-subset(data, data$species!="NA")
    if (missing(input_species)){
        no_species<-unique(data$species)
    } else {
        data <- data %>% filter(data$species==input_species)
        no_species<-input_species
    }
    
    wm<-map_data("world") %>% filter(region != "Antartica") %>% fortify() # This gets the polygon data for the world map
    for(i in 1:length(no_species)){
        iter_species<-data %>% filter(data$species==no_species[i]) #subsets to just one species
        p <- ggplot()+ coord_fixed() + geom_map(data=wm, map=wm, aes(group = group, map_id= region),
            fill = "darkgrey")     
        if (country==TRUE){
            p <- p + aes(colour= "black", size = 0.02)
        } 
        p <- p + geom_point(data = iter_species, aes(decimalLongitude, decimalLatitude),
            colour = "orange", size = 1)
        # Limit map extent to that of the points
        p <- p + lims(x = c(floor(min(iter_species$decimalLongitude)-10), ceiling(max(iter_species$decimalLongitude)+10)),
            y = c(floor(min(iter_species$decimalLatitude)-10), ceiling(max(iter_species$decimalLatitude)+10)))
        p <- p + theme_bw()
        print(paste("Saving map of", no_species[i], spe=" "))
        invisible(ggsave(filename=paste(no_species[i], filename, sep="_"), path=output_path, device="pdf"))
    }
}