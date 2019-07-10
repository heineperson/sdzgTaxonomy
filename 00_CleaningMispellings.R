# Loading Packages

library(data.table)
library(stringr)
library(taxize)

# Loading Data
## This is the ExpTax02 output from IrisBG - I believe it includes all species even if they are deaccessioned or dead
allSpecies <- fread("Data/SDZGSpeciesList_LivingandDead.csv")


# Give each species a unique ID
allSpecies[,ID:=.I]

# Create a field for the edited species name by copying the TaxonName and trimming white space
allSpecies[,EditedName:=trimws(TaxonName)]

## Fixing a littany of issues 
# Standardizing nomencalture by change ssp. to subsp. for subspecies
allSpecies[grepl("ssp ",TaxonName)]
allSpecies[grepl("ssp. ",TaxonName)]
allSpecies[,EditedName:=gsub(" ssp "," subsp. ",EditedName,fixed=T)]

# Searching for var. with no period
allSpecies[grepl("var ",TaxonName)]
allSpecies[,EditedName:=gsub(" var "," var. ",EditedName,fixed=T)]

# Searching for v with no period
allSpecies[grepl(" v. ",TaxonName)]
allSpecies[,EditedName:=gsub(" v. "," var. ",EditedName,fixed=T)]
allSpecies[,EditedName:=gsub(" va "," var. ",EditedName,fixed=T)]

# Searching for two spaces in a species name
allSpecies[grepl("  ",TaxonName)]
allSpecies[,EditedName:=gsub("  "," ",EditedName,fixed=T)]


# Finding ITIS Names
speciesNamesResolve_all <- gnr_resolve(names = allSpecies$EditedName[45:length(allSpecies$EditedName)],best_match_only = TRUE,preferred_data_sources = c(3), canonical = TRUE)

speciesNamesResolve_45100 <- as.data.table(speciesNamesResolve_45100)


# Names that were merged
allSpecies[,.(Count=.N,NamesToMerge=toString(unique(TaxonName[TaxonName!=EditedName]))),by="EditedName"][Count>1]
