####################################
##         PROJET CHOPIN
## 
##          ANALYSES
##     SOMMES PAR FAMILLE
##      
####################################


#-------------------------------------------------
# Chargement de la fonction de calcul des stats
source('Script/CHOPIN_FUN_analyses_stats_somme_facteur.R')


#-------------------------------------------------
# BENTHOS - Tableau de synthese des sommes par taxon, zone et saison

benthos_somme_taxon = stat_somme_facteur(dataset=benthos_contam, facteur = "grp", title="")
benthos_somme_zone = stat_somme_facteur(dataset=benthos_contam, facteur = "zone", title="")
benthos_somme_saison = stat_somme_facteur(dataset=benthos_contam, facteur = "season", title="")

benthos_somme_par_famille = rbind(benthos_somme_taxon, benthos_somme_zone, benthos_somme_saison)

write.xlsx(benthos_somme_par_famille,row.names = F,
           "Output/BENTHOS/BENTHOS_tableau_sommaire_somme_par_famille.xlsx", overwrite = T)


#-------------------------------------------------
# SOLES - Tableau de synthese des sommes par classe d'age, zone et saison

soles_somme_group = stat_somme_facteur(dataset=soles_contam, facteur = "grp", title="")
soles_G0_somme_zone = stat_somme_facteur(dataset=solesG0_contam, facteur = "zone", title="G0")
soles_G0_somme_saison = stat_somme_facteur(dataset=solesG0_contam, facteur = "season", title="G0")

soles_somme_par_famille = rbind(soles_somme_group, soles_G0_somme_zone, soles_G0_somme_saison)

write.xlsx(soles_somme_par_famille,row.names = F,
           "Output/SOLES/soles_tableau_sommaire_somme_par_famille.xlsx", overwrite = T)

