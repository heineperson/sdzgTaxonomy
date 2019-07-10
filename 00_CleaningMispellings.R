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

# Fixing discrepancy in quotation marks
allSpecies[,EditedName:=gsub("\"", "\'",EditedName)]
allSpecies[,EditedName:=gsub("\''", "\'",EditedName)]


# Indicating the fix
allSpecies[,nameChangeReason:=ifelse(EditedName!=TaxonName, "Spelling Standardization","No Change")]


# Names that were merged
allSpecies[,.(Count=.N,NamesToMerge=toString(unique(TaxonName[TaxonName!=EditedName]))),by="EditedName"][Count>1]

# Finding ITIS Names
speciesNamesResolve_all <- gnr_resolve(names = allSpecies$EditedName[45:length(allSpecies$EditedName)],best_match_only = TRUE,preferred_data_sources = c(3,4), canonical = TRUE)
speciesNamesResolve_all <- as.data.table(speciesNamesResolve_all)

# Select names for update that have a lower matching score but have multiple words
PotentialResolutions <- speciesNamesResolve_all[score < 0.98 & str_count(matched_name2," ")>=1 & !grepl("\'", user_supplied_name)]
setkey(PotentialResolutions,user_supplied_name)
setkey(allSpecies, EditedName)
allSpecies[PotentialResolutions,`:=` (EditedName=i.matched_name2,nameChangeReason="Fuzzy Match ITIS or NCBI" )]

# Defining what changes to show peopel first
SpeciesChangeSummary <- allSpecies[,.(Count=.N,NamesToMerge=toString(unique(TaxonName[TaxonName!=EditedName])), nameChangeReason=toString(unique(nameChangeReason))),by="EditedName"]
SpeciesChangeSummary <- SpeciesChangeSummary[Count>2 | nameChangeReason!="No Change" ]
SpeciesChangeSummary[,ActionNeeded:=ifelse(Count>1, "Merge Taxa with Existing Correct Name","Change Spelling of Original Name")]

SpeciesChangeSummary <- SpeciesChangeSummary[,.(OriginalName=NamesToMerge, PotentialCorrectedName=EditedName,ActionNeeded)][order(-ActionNeeded)]

write.csv(SpeciesChangeSummary,"Data/SpeciesChangeSummary_Jul2019.csv")
