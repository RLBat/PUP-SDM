
# Collecting data for the Eastern Lowland Olingo in the Neotropics (South America)

geo_area_Neo<-Country_Codes(level="Realm", region="Neotropics")
Fig<-GBIFSpecies(taxon_rank=1, Kingdom="Plants", taxa_names="Ficus maxima", min_occur=20, input_data=NULL, geo_area=geo_area_Neo)



# Collecting data for all bees in Northern Europe

geo_area_NEur<-Country_Codes(level="UN_Subregion", region="Northern_Europe")
Bee_Families <- c("Andrenidae", "Apidae","Colletidae","Halictidae","Megachilidae","Melittidae","Stenotritidae")
Bees<-GBIFSpecies(taxon_rank=0, Kingdom="Animalia", taxa_names=Bee_Families, min_occur=20, input_data=NULL, geo_area=geo_area_NEur)