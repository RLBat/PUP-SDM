rm(list=ls())
require(dplyr)
require(tidyr)
require(rgdal)
require(sp)
require(ggplot2)
require(magrittr)
require(sjPlot)

setwd("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project")
Bee_Species<-read.csv("Data/Taxa/European_Bee_Species.csv")
Bee_Species<- Bee_Species %>%
  arrange(Scientific_Name)

Bioclimatic_variables<- c("Annual Mean Temperature (Bioclim 1)",
                          "Temperature Seasonality (Bioclim 4)",
                          "Annual Precipitation (Bioclim 12)",
                          "Precipitation Seasonality (Bioclim 15)")

Definitions<- c("The mean of all the weekly mean temperatures. Each weekly mean temperature is the mean of the weekly maximum and minimum temperatures over the whole year (Measured in degrees Celsius (°C)).",
                "Standard deviation - The amount of temperature variation over a given year based on the standard deviation of monthly temperature averages (Measured in Standard deviation * 100).",
                "The sum of all twelve monthly precipitation values (Measured in Millimetres (mm).",
                "Coefficient of Variation - The standard deviation of the weekly precipitation values expressed as a percentage of those estimates.")

Bioclim_Definitions<-data.frame(Bioclimatic_variables,Definitions)

tab_df(Bioclim_Definitions, title = "Table 2. Definitions of the four bioclimatic variables used in the species distribution models. Each bioclimatic variable is the mean for a given grid cell over the years of 1970 - 2000. The definitions follow that of ANUCLIM (Xu & Hutchinson, 2011), except for temperature seasonality that used standard deviation - definition derived from US Geological Survey (O'Donnel & Ignizio, 2012) - because coefficient of variation is erroneous for temperatures between -1 and 1.",
       footnote = "O'Donnel, M. S, Ignizio, D. A. (2012). Bioclimatic predictors for supporting ecological applications in the conterminous United States (No. 691). US Geological Survey.
       Xu, T, Hutchinson, M.F. (2011). ANUCLIM Version 6.1 User Guide. The Australian National University, Fenner School of Environment and Society, Canberra.",
       show.footnote = TRUE, file = "Work/Tables/Bioclimatic_Variable_Defintions.doc")
