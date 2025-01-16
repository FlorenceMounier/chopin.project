####################################
##         PROJET CHOPIN
## 
##         MAIN SCRIPT
## MISE EN FORME & TRANSFORMATIONS
##      DES DONNEES BRUTES
##      
####################################

# --------------------------------------------------------------
# Select database

path_CHOPIN_BDD = if (interactive() && .Platform$OS.type == "windows")
  choose.files(default = "Data/CHOPIN_BASE_DE_DONNEES_GENERALE.xlsx",
               caption = "Select file named CHOPIN_BASE_DE_DONNEES_GENERALE.xlsx",filters = Filters[c("zip", "All"),])


# --------------------------------------------------------------
# Chargement des info contaminants

source("Script/CHOPIN_DATA_FORMATTING_contaminants.R")


# --------------------------------------------------------------
# Chargement, mise en forme et transformations des donnees SEDIMENTS

source("Script/CHOPIN_DATA_FORMATTING_sediments.R")


# --------------------------------------------------------------
# Chargement, mise en forme et transformations des donnees BENTHOS

source("Script/CHOPIN_DATA_FORMATTING_benthos.R")


# --------------------------------------------------------------
# Chargement, mise en forme et transformations des donnees SOLES

source("Script/CHOPIN_DATA_FORMATTING_soles.R")
