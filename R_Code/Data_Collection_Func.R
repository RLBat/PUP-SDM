# Data_Collection_Func.R - Contains all functions for the package *insert name* that deal with data input and collection from outside sources.
# Author: Rachel Bates (adapted from code by Patrick Walkden)
# Date: 21/06/19


# Documentation to follow

# Using a list of species (or if other taxa it first generates a list of species), checks for 
# synonyms with GBIF and catalogue of life, gets all occurance data (for a specific geographic
# area), and associates the required data with it.

#' Add together two numbers.
#' 
#' @param taxon_rank The taxon level for which you are searching (e.g. Family). Options are Kingdom, Phylum, Class, Genus or Species.
#' @param taxa_names The names of taxa to search (e.g. bee families)
#' @param input_data A species list to check against GBIF and catalogue of life to identify any synonyms. 
#' @param geo_area The geographical area you wish to model. This should be a list of country codes.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' 
GBIFSpecies <- function(taxon_rank, taxa_names, input_data=0){

}