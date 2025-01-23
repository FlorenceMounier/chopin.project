####################################
##         PROJET CHOPIN
##
##    MISE EN FORME DES DONNEES
##           SEDIMENTS
####################################


# Loadings

## Load packages

library(tidyverse)
library(readxl)

## Load contaminant lists and labels
# File "1_data_formatting_contaminants.R" should be ran previously
# If not, run the following script
source(file = "data-raw/data_formatting_contaminants.R")


## Load data

sed_contam = read_excel(here("data-raw/CHOPIN_BASE_DE_DONNEES_GENERALE.xlsx"),
                       sheet= "sediments")

## PFASs selection for sediment analysis

PFAS = PFAS_ALL
PFAS_lab = PFAS_ALL_lab
sub_family = sub_family_ALL
n_C = n_C_ALL
FOSAs = FOSAs_ALL
FOSAs_lab = FOSAs_ALL_lab

# Wrangling

## Translate "saison" and rename factor values of the variable "season"

sed_contam <- sed_contam %>%
  mutate(saison = as_factor(saison)) %>%
  mutate(season = saison) %>%
  mutate(season = fct_collapse(saison,
                               "Spring" = c("juin"),
                               "Autumn" = c("octobre")))


## Rename columns to add the unit, ng.gdw-1, as a suffix

sed_contam <- sed_contam %>%
rename_at(c(PCB, PFAS, HBCDD), ~ paste(., "ng.gdw", sep = "_"))


## Compute concentrations in ng.gCorg-1

gdw_TO_gCorg <- function(contams, data){
  Corg_ng_g <- pull(data, `Corg_mg_g-1`)/1000
  contam_ng_gCorg <- function(contam_ng_gdw){
    contam_ng_gdw / Corg_ng_g
  }
  data %>%
    mutate_at(paste(contams, "ng.gdw", sep = "_"), list("tmp" = contam_ng_gCorg)) %>%
    rename_with(.cols = ends_with("tmp"), ~ paste(contams, "ng.gCorg", sep = "_"))
}

sed_contam <- gdw_TO_gCorg(contams = c(PCB, PFAS, HBCDD), data = sed_contam)


# Compute the sum of the concentrations per family

sum_by_family <- function(family, contams, data, unit){

    sum_by_family <- function(contams, data, unit){
    data %>%
      select(contains(contams) & ends_with(paste(unit))) %>%
      rowSums(na.rm = TRUE)
    }

  data %>%
    mutate(tmp_sum = sum_by_family(contams, data, unit)) %>%
    rename_with(~ gsub("tmp_sum", paste("sum", family, unit, sep="_"), x = .x))
  }

# * in ng.gCorg-1

sed_contam <- sum_by_family(contams = PCB, family = "PCB", data = sed_contam, unit = "ng.gCorg")
sed_contam <- sum_by_family(contams = PFAS, family = "PFAS", data = sed_contam, unit = "ng.gCorg")
sed_contam <- sum_by_family(contams = HBCDD, family = "HBCDD", data = sed_contam, unit = "ng.gCorg")

# * in ng.gdw-1

sed_contam <- sum_by_family(contams = PCB, family = "PCB", data = sed_contam, unit = "ng.gdw")
sed_contam <- sum_by_family(contams = PFAS, family = "PFAS", data = sed_contam, unit = "ng.gdw")
sed_contam <- sum_by_family(contams = HBCDD, family = "HBCDD", data = sed_contam, unit = "ng.gdw")


## Compute normalised concentrations by the sum by family ( ng_gdw )

normalised_conc <- function(family, contams, data, unit){
  sum_unit <- pull(data, paste("sum", family, unit, sep="_"))
  norm_conc <- function(contam_unit){
    contam_unit / sum_unit
  }
  data %>%
    mutate_at(paste(contams, unit, sep = "_"),
              list("tmp" = norm_conc)) %>%
    rename_with(.cols = ends_with("tmp"), ~ paste(contams, "normalised_sum", unit, sep = "_"))
}

# * in ng.gCorg-1
sed_contam <- normalised_conc(contams = PCB, family = "PCB", data = sed_contam, unit = "ng.gCorg")
sed_contam <- normalised_conc(contams = PFAS, family = "PFAS", data = sed_contam, unit = "ng.gCorg")
sed_contam <- normalised_conc(contams = HBCDD, family = "HBCDD", data = sed_contam, unit = "ng.gCorg")

# * in ng.gdw-1
sed_contam <- normalised_conc(contams = PCB, family = "PCB", data = sed_contam, unit = "ng.gdw")
sed_contam <- normalised_conc(contams = PFAS, family = "PFAS", data = sed_contam, unit = "ng.gdw")
sed_contam <- normalised_conc(contams = HBCDD, family = "HBCDD", data = sed_contam, unit = "ng.gdw")


# Save dataset
write_csv(x = sed_contam, file = "data-raw/sed_contam.csv")


# Subset par saison (tableau complet)
sed_contam_juin = sed_contam[which(sed_contam$season=="Printemps"),]
sed_contam_oct = sed_contam[which(sed_contam$season=="Automne"),]


# Output data

usethis::use_data(sed_contam, overwrite = TRUE)
usethis::use_data(sed_contam_juin, overwrite = TRUE)
usethis::use_data(sed_contam_oct, overwrite = TRUE)
