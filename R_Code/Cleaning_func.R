rm(list=ls())
require(dplyr)
require(tidyr)
require(rgbif)
require(magrittr)
require(countrycode)
require(CoordinateCleaner)
require(rgdal)
require(sp)
require(ggplot2)
require(magrittr)
require(adehabitatHR)
require(scales)
require(doParallel)

## Initally flag those coordinates that are a bit dubious.
Clean_GBIF_Data<-function(GBIF_Data, outlier=12, min_occur=20, precision_m=5){
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

    Cleaned_Data<-Cleaned_Data %>% filter(coordinateUncertaintyInMeters/1000 <= precision_m | is.na(coordinateUncertaintyInMeters))

    ### Remove records that are from unsuitable data sources e.g fossils 

    Cleaned_Data<-Cleaned_Data %>% filter( basisOfRecord == "HUMAN_OBSERVATION" 
        | basisOfRecord == "OBSERVATION" | basisOfRecord == "PRESERVED_SPECIMEN")

    ## filter out records with absence records and high individual count records also

    Cleaned_Data<-Cleaned_Data %>% filter(individualCount > 0 | is.na(individualCount)) %>%
        filter(individualCount < 99 | is.na(individualCount))

    ### Now most of the records have been cleaned we can re-run an outlier analysis to remove some 
    #final spurious records -- species with less than 7 unique records cannot be tested
    no_outlier<-Cleaned_Data[FALSE,]
    for(i in 1:length(unique(Cleaned_Data$species))){
        clean_species<-unique(Cleaned_Data$species)[i]
        iter_species<-subset(Cleaned_Data, Cleaned_Data$species==clean_species)
        if(nrow(iter_species) >= min_occur){ #Removes all species with fewer than min_occur records
            iter_outlier <- iter_species %>% cc_outl(lon="decimalLongitude", lat="decimalLatitude", species="species", mltpl = 12)
            # Some say removed 0, some say removed NA. Likely something going wrong here.
            if (nrow(iter_outlier)>= min_occur){
                no_outlier <- rbind(no_outlier, iter_outlier) #Only retains species with enough occurence records
            }
        }
    }

    print(paste("Data cleaning finished.", nrow(no_outlier), "records remaining of", length(unique(no_outlier$species)), "species.", sep=" "))
    # finally remove species that have less than 25 occurrences remaining after inital cleaning
    return(no_outlier)
}


