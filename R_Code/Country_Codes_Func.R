
#Country_Code_CSV<-read.csv("../Data/Country_Codes.csv") # Will be included as data for the package

#' Using a list of species (or if other taxa it first generates a list of species), checks for synonyms with GBIF and catalogue of life, gets all occurance data (for a specific geographic area), and associates the required data with it.
#' 
#' @param level The level at which to search. Options are Country_Name, UN_Subregion, UN_Region or Realm.
#' @param region The area which you wish to gather country codes for. Can be a string of characters or a vector of names.
#' @return The sum of \code{x} and \code{y}.
#' @examples Country_Codes(level="UN_subregion", region=c("Western_Europe", "Eastern_Europe"))
#' 
Country_Codes <- function(level="Realm", region="Palearctic"){
    geo_area = c() # Initialise empty vector
    if (level=="Realm"|level=="realm"){
        for (i in 1:length(region)){ 
            geo_area = append(geo_area, as.character(Country_Code_CSV$Country_Code[Country_Code_CSV$Realm==region[i]]))
        }
    } else if (level=="UN_Subregion"|level=="UN_subregion"){
        for (i in 1:length(region)){ 
        geo_area = append(geo_area, as.character(Country_Code_CSV$Country_Code[Country_Code_CSV$UN_Subregion==region[i]]))
        }
    } else if (level=="UN_Region"|level=="UN_region"){
        for (i in 1:length(region)){ 
            geo_area = append(geo_area, as.character(Country_Code_CSV$Country_Code[Country_Code_CSV$UN_Region==region[i]]))
        }
    } else if (level=="Country_Name"|level=="Country_name"|level=="country_Name"|level=="country_name"){
        for (i in 1:length(region)){ 
            geo_area = append(geo_area, as.character(Country_Code_CSV$Country_Code[Country_Code_CSV$Country_Name==region[i]]))
        }
    }
    return (geo_area)
}