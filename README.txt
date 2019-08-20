READ ME:: Script Order







1) GBIF_Query_1_Occurrence_Counts -> 

Data required:: Family names of species that you would like to model

Querys catelogue of life and GBIF for species names downstream of familiy names given.
With species names queries GBIF to get taxonkey and occurrence data.

Choices that can be made here:: Countries that you want the species to come from

Data output:: (SPECIES_DATA - Data frame with species name, taxonkey, total number of occurrences, total number of occurrences in target region) 

Comments:: Here I have removed species that have less that 20 occurrences in GBIF, I think that including those species can be useful for later in the modeling process
as you may be able to identify surrogates, it would be better to just remove species with no GBIF occurrences. Also, might be worth adding in an extra step here to check for 
species synonyms in the output - I think "taxize" has a function to this effect. 









2) GBIF_Query_2_Euro_Species_Occ_Coordinates -> 

Data required:: (SPECIES_DATA - Data frame with species name and taxonkey) 

This again queries gbif, getting coordinates and other metadata for each of the species occurrences.

Data Output:: (GBIF_DATA - a list of data frames, each of which contains the coordinates and metadata for each species occurrences.) 


Comments:: There is a bit of code at the end here where I evaluate the output of the query. Some of the species couldn't be located in GBIF
so have given just the output of the whole genus - I think this might have arisen due to some synonyms being used, when I was doing this I
came across a way that could do this in the "taxize" R package used in "GBIF_Query_1_Occurrence_Counts". Although it could just be a good check regardless. 










3) Cleaning_1_Cleaning <-

Data required:: (GBIF_DATA)

This uses the "CoordinateCleaner" package to remove and spurious occurrences based on their coordinates, can be because they're in the sea, close to museums or
natural history institutions, or outliers etc. Also, removes some spurious records depending on the metadata.

Choices that can be made here:: by what indices do you want to cleaning the data by. eg. coordinate uncertainty I have used 5km but could be greater than that. etc

Data Output:: (CLEAN_DATA - a list of data frames, each of which contains cleaned coordinates and metadata for each species.)

Comments:: again I have removed species at the end with less than 25 occurences these I think could be left in. Also, this is where Norway gave a bit of a problem so there is a wierd
work around right at the begining 









4) Cleaning_4_Cleaning_Maps

Data required:: (GBIF_DATA, CLEAN_DATA, FINISH_DATA*, THINNED_DATA*) * This data will be produced later down the pipeline


Data output:: creates a map with the coorindates of each of the occurences  

Comments:: This script is something that I revisited a few times throughout the process when it came to show the coordinates after each phase of cleaning, I think that it could possibly work better that 
a map is produced at the end of each script but this is just where I worked with it so its a bit out of nowhere. --- So it might be worth sort of incorporating this script into the others
but it is important that a file is made at some point for the maps and other outputs for each species to be saved in. In the script this happens in the beginning of the "polish" script but would probably be
better at the end of inital cleaning.  









5) Cleaning_2_Polish <-

Data required:: (GBIF_DATA, CLEAN_DATA)

Even though cleaning coordinates does identify outliers some can still persist so this creates a minimum convex polygon around the cleaned points, and also calculates the area within 
the polygon in increments of 0.1 from 50 to 100 percent. This is where some manual evaluation comes in where you have to look at both the maps and the mcp area output to decide the optimal
percent of points to include in the polygon. Once you have decided the right percent for each species you can run an function in coordinate cleaner to remove points outside of its bounds.

Choices that can be made here:: the percentage of mcp you want to use

Data output :: (FINISH_DATA - a list of data frames, with species data after removing further outliers,
                also, saves a file in cleaning_maps of the mcp area graph will be saved for evaluation)

Comments:: I save some RData here "Spatial_Data" and "Polygon Data" I dont know whether this is necessary as I dont think I revisit them at all later in the pipeline. for Bee_mcp_Percent
I just created and imported an csv file - I don't know whether there is an easier way to do this. 









6) Cleaning_3_Bioclimatic_tables

Data required:: (FINISH_DATA, GBIF
                 Bioclimatic varaible rasters that you want to use and at the resolution to want to model at.)

This script creates and saves a boxplot in Cleaning_Maps species file of how the bioclimatic variables have been affected by the cleaning of coordinates. Also produces a table of the results 
of a t-test to see whether this was a significant difference. 

Data output:: (Bioclimatic_Tables - a list of data frames with some descriptive statistics of how bioclimatic variables have been influenced by coordinate cleaning and save boxplot and 
table in cleaning_maps species file)

Comments:: SORRY this one is really poorly annotated, I have been meaning to go back and tidy it up but I wanted to produce these stats for my project, if we were to be creating a package
I don't think this script will be needed. Although in the table it does tell you how many points have been removed - could be a useful stat to extract at some point.   








7) Spatial_Autocorrelation_Spatial_Thinning

Data required::  (FINISH_DATA)

Performs a thinning algorithm on the datasets so that coordinates are not within a set distance from each other - the algorithm removes points at random and then iterates until an optimal 
dataset it obtained.

Choices that can be made here:: the algorithm by which ou thin and the distance at which you thin.

Data Output:: (THINNED DATA)

Comments:: to use the gurobi optimizer algorithm you have to download it online, really easy. also getting the package 'spThin' can be a tad annoying sometimes, but you
hav to make sure that you download the version from github in 'jeffreyhansons' repository.


8)Pseudo-absence_random_absence 

Data required: thinned data 
               Biolcimatic variables that you want to model the occurrences as a function of. 

This script runs a two-step pseudo-absence generation. First it performs environmental profiling on the occurrence data using a one class support vector machine, to identify
areas of the background data (Boclimatic variables) that are more environmentally dissimilar to choose pseudo-absences from, this is with the aim of reducing the prevelance 
of false absences. ----- NOTE THIS SCRIPT ALSO TAKES A LONG TIME TO RUN.

choices to be made here: the minimum distance pseudo-absences are generated away from each other and presence points. 
                               The proportion of pseudo-absence to presenc points that you want to generate.

Data output: data frame of coordinates that have been generated as pseudo absences.



9) Pseudo_absence_occ_absences --- NOT THE MOST NECESSARY SCRIPT IN THE WORLD. 

Data_required: CLEAN_DATA 

Chooses additional pseudo-absences points by identifying coordinates within the GBIF data that multiple bees have been recorded at - a lot of bees at a single coordinate
could be indicative of the area being comprehensively surveyed and therefore be more likely to be a true-absence. 

data output: another data frame with pseudo-absences generated from occurrence points.  



AT THIS POINT THE POTENTIAL INACCURACIES AND SAMPLING BIASES IN THE DERIVED GBIF DATA SHOULD BE ACCOUNTED FOR (to the best of our ability),
THEREFORE YOU CAN START TO THINK ABOUT RUNNING SOME SPECIES DISTRIBUTION MODELS - HERE I HAVE DONE THIS WITH THE R PACKAGE BIOMOD2.

10) SDM_Modelling_Modeling

Data required: (Thinned data), 
              pseudo-absences 
              the bioclimatic variables

I have tried to annotate this script as much as possible

decisions that can be made: the models that you want to run the SDMs with,
 the evaluation statistics that you want to calculate to be able to compared the predictive performance of each of the models. 


data output: a dataframe of the evaluation statistics of each model. Biomod2 saves the models and all the relevant data in your working directory the next script will extract the models you want.



11)
