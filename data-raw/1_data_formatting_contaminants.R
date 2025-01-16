## Data formatting - Contaminants


# Loadings

## Load packages

library(readxl)
library(tidyverse)
library(here)

## Load data

contam = read_excel("data-raw/CHOPIN_BASE_DE_DONNEES_GENERALE.xlsx",
                    sheet= "INFO_contam")

# Custom function for data selection


wrangle_contam <- function(grp_contam, grp_type, out_var){
  contam %>%
  filter(!!sym(grp_type) == grp_contam) %>%
  pull(!!sym(out_var))
  }

# Labels in data

PCB <- wrangle_contam(grp_contam = "PCB", grp_type = "family", out_var = "chemical")
HBCDD <- wrangle_contam(grp_contam = "HBCDD", grp_type = "family", out_var = "chemical")
PFAS_ALL <- wrangle_contam(grp_contam = "PFAS", grp_type = "family", out_var = "chemical")
sub_family_ALL <- wrangle_contam(grp_contam = "PFAS", grp_type = "family", out_var = "sub_family_TAG")
PFCAs <- wrangle_contam(grp_contam = "PFCAs", grp_type = "sub_family_TAG", out_var = "chemical")
PFSAs <- wrangle_contam(grp_contam = "PFSAs", grp_type = "sub_family_TAG", out_var = "chemical")
FOSAs <- wrangle_contam(grp_contam = "FOSAs", grp_type = "sub_family_TAG", out_var = "chemical")
FOSAAs <- wrangle_contam(grp_contam = "FOSAAs", grp_type = "sub_family_TAG", out_var = "chemical")
FOSAs_ALL <- append(FOSAs, FOSAAs)
FTSAs <- wrangle_contam(grp_contam = "FTSAs", grp_type = "sub_family_TAG", out_var = "chemical")
diPAP <- wrangle_contam(grp_contam = "di-PAPs", grp_type = "sub_family_TAG", out_var = "chemical")
other_PFAS <- wrangle_contam(grp_contam = "other", grp_type = "sub_family_TAG", out_var = "chemical")

# Labels for graphics

PCB_lab <- wrangle_contam(grp_contam = "PCB", grp_type = "family", out_var = "chem_label")
HBCDD_lab <- wrangle_contam(grp_contam = "HBCDD", grp_type = "family", out_var = "chem_label")
PFAS_ALL_lab <- wrangle_contam(grp_contam = "PFAS", grp_type = "family", out_var = "chem_label")
PFCAs_lab <- wrangle_contam(grp_contam = "PFCAs", grp_type = "sub_family_TAG", out_var = "chem_label")
PFSAs_lab <- wrangle_contam(grp_contam = "PFSAs", grp_type = "sub_family_TAG", out_var = "chem_label")
FOSAs_lab <- wrangle_contam(grp_contam = "FOSAs", grp_type = "sub_family_TAG", out_var = "chem_label")
FOSAAs_lab <- wrangle_contam(grp_contam = "FOSAAs", grp_type = "sub_family_TAG", out_var = "chem_label")
FOSAs_ALL_lab <- append(FOSAs_lab, FOSAAs_lab)
FTSAs_lab <- wrangle_contam(grp_contam = "FTSAs", grp_type = "sub_family_TAG", out_var = "chem_label")
diPAP_lab <- wrangle_contam(grp_contam = "di-PAPs", grp_type = "sub_family_TAG", out_var = "chem_label")
other_PFAS_lab <- wrangle_contam(grp_contam = "other", grp_type = "sub_family_TAG", out_var = "chem_label")


# Chemical characteristics

## Number of carbon atoms within PFASs
n_C_ALL <- wrangle_contam(grp_contam = "PFAS", grp_type = "family", out_var = "n_C")

## LogKow within PCBs
log_Kow <- wrangle_contam(grp_contam = "PCB", grp_type = "family", out_var = "logKow")


# Output data

usethis::use_data(contam, overwrite = TRUE)

usethis::use_data(PCB, overwrite = TRUE)
usethis::use_data(HBCDD, overwrite = TRUE)
usethis::use_data(PFAS_ALL, overwrite = TRUE)
usethis::use_data(sub_family_ALL, overwrite = TRUE)
usethis::use_data(PFCAs, overwrite = TRUE)
usethis::use_data(PFSAs, overwrite = TRUE)
usethis::use_data(FOSAs, overwrite = TRUE)
usethis::use_data(FOSAAs, overwrite = TRUE)
usethis::use_data(FOSAs_ALL, overwrite = TRUE)
usethis::use_data(FTSAs, overwrite = TRUE)
usethis::use_data(diPAP, overwrite = TRUE)
usethis::use_data(other_PFAS, overwrite = TRUE)

usethis::use_data(PCB_lab, overwrite = TRUE)
usethis::use_data(HBCDD_lab, overwrite = TRUE)
usethis::use_data(PFAS_ALL_lab, overwrite = TRUE)
usethis::use_data(PFCAs_lab, overwrite = TRUE)
usethis::use_data(PFSAs_lab, overwrite = TRUE)
usethis::use_data(FOSAs_lab, overwrite = TRUE)
usethis::use_data(FOSAAs_lab, overwrite = TRUE)
usethis::use_data(FOSAs_ALL_lab, overwrite = TRUE)
usethis::use_data(FTSAs_lab, overwrite = TRUE)
usethis::use_data(diPAP_lab, overwrite = TRUE)
usethis::use_data(other_PFAS_lab, overwrite = TRUE)

usethis::use_data(n_C_ALL, overwrite = TRUE)
usethis::use_data(log_Kow, overwrite = TRUE)
