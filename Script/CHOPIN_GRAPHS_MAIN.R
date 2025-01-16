####################################
##         PROJET CHOPIN
## 
##         MAIN SCRIPT
##         GRAPHIQUES
##      
####################################


########################################################
## GRAPH des PROFILS NORMALISES

source('Script/CHOPIN_GRAPHS_normalised_profiles_sediments.R')

source('Script/CHOPIN_GRAPHS_normalised_profiles_benthos.R')

source('Script/CHOPIN_GRAPHS_normalised_profiles_soles.R')



########################################################
## GRAPH des NIVEAUX EN COMPOSE MAJORITAIRE PAR FAMILLE
#  et par classe d'âge (soles) / taxon (benthos), zones, saison

source('Script/CHOPIN_GRAPHS_benthos_niveaux_composes_majoritaires.R')



########################################################
## GRAPH des SOMMES PAR FAMILLE (sous-famille pour les PFAS)
#  et par classe d'âge (soles) / taxon (benthos), zones, saison

source('Script/CHOPIN_GRAPHS_sommes_famille.R', encoding = 'UTF-8')


########################################################
## GRAPH des NIVEAUX NORMALISES PAR LA SOMME PAR FAMILLE (sous-famille pour les PFAS)
#  et par classe d'âge (soles) / taxon (benthos), zones, saison

source('Script/CHOPIN_GRAPHS_niveaux_normalises_par_sommes_famille.R')

