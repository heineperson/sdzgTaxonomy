library(data.table)

tpl <- readRDS("Data/PlantList_California_201470424.rds")
allSpecies # Generated from file "00_CleaningMispellings.R"


tpl[,TaxonName:=trimws(TaxonName)]
mergeExact <- merge(species, tpl, by.x="TaxonName",by.y="TaxonName",all.x=T)
mergeExact$`Taxonomic status in TPL`


mergeExact[`Taxonomic status in TPL`=="Unresolved"]