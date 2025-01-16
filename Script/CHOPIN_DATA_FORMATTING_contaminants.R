####################################
##         PROJET CHOPIN
## 
##    MISE EN FORME DES DONNEES
##      LISTE DES CONTAMINANTS
####################################



#~~~~~~~~~~~~~~~~~~
#       PCB
#~~~~~~~~~~~~~~~~~~

# Labels des tableaux de donnees

PCB = c("CB28","CB31","CB44","CB49","CB52","CB101","CB105",
        "CB110","CB118","CB128","CB132","CB138","CB149",
        "CB153","CB156","CB170",
        "CB180","CB187","CB194")

# Labels des graphiques

PCB_lab = c("CB028","CB031","CB044","CB049","CB052","CB101","CB105",
            "CB110","CB118","CB128","CB132","CB138","CB149",
            "CB153","CB156","CB170",
            "CB180","CB187","CB194")

#~~~~~~~~~~~~~~~~~~
#       PFAS
#~~~~~~~~~~~~~~~~~~

# Labels des tableaux de donnees

PFAS_ALL = c("PFPeA","PFHxA" ,"PFHpA","PFOA","PFNA","PFDA","PFUnDA","PFDoA","PFTrDA","PFTeDA",
             "FOSA","MeFOSA","EtFOSA","FOSAA","MeFOSAA","EtFOSAA",
             "PFBS","PFHxS","PFHpS", "L-PFOS","Br-PFOS","PFDS",
             "PFECHS",
             "4:2.FTSA","6:2.FTSA","8:2.FTSA","10:2.FTSA",
             "6:2.diPAP","8:2.diPAP",
             "HFPO-DA","NaDONA")

# Labels des graphiques

PFAS_ALL_lab = c("PFPeA","PFHxA","PFHpA","PFOA", "PFNA","PFDA","PFUnDA","PFDoA","PFTrDA","PFTeDA",
                 "FOSA","MeFOSA","EtFOSA","FOSAA", "MeFOSAA", "EtFOSAA",
                 "PFBS","PFHxS", "PFHpS" ,"PFOS(L)", "PFOS(Br)", "PFDS", 
                 "PFECHS",
                 "4:2-FTSA","6:2-FTSA", "8:2-FTSA", "10:2-FTSA", 
                 "6:2-diPAP", "8:2-diPAP",
                 "HFPO-DA","NaDONA")

# Labels des sous-familles pour les graphiques

ss_famille_ALL = c("PFCA","PFCA" ,"PFCA","PFCA","PFCA","PFCA","PFCA","PFCA","PFCA","PFCA",
                   "FOSA","FOSA","FOSA","FOSAAs","FOSAAs","FOSAAs",
                   "PFSA","PFSA","PFSA","PFSA","PFSA","PFSA",
                   "Autres PFAS",
                   "FTS","FTS","FTS","FTS",
                   "diPAP","diPAP",
                   "Autres PFAS","Autres PFAS")


PFCA_lab = c("PFPeA","PFHxA","PFHpA","PFOA", "PFNA","PFDA","PFUnDA","PFDoA","PFTrDA","PFTeDA")
PFCA = c("PFPeA","PFHxA","PFHpA","PFOA", "PFNA","PFDA","PFUnDA","PFDoA","PFTrDA","PFTeDA")

FOSAs_ALL_lab = c("FOSA","MeFOSA","EtFOSA","FOSAA", "MeFOSAA", "EtFOSAA")
FOSAs_ALL = c("FOSA","MeFOSA","EtFOSA","FOSAA", "MeFOSAA", "EtFOSAA")

PFSA_lab = c("PFBS","PFHxS", "PFHpS" ,"PFOS(L)", "PFOS(Br)", "PFDS")
PFSA = c("PFBS","PFHxS","PFHpS", "L-PFOS","Br-PFOS","PFDS")

otherPFASs_lab = c("PFECHS", "HFPO-DA","NaDONA")
otherPFASs = c("PFECHS", "HFPO-DA","NaDONA")

FTS_lab = c("4:2-FTSA","6:2-FTSA", "8:2-FTSA", "10:2-FTSA")
FTS = c("4:2.FTSA","6:2.FTSA","8:2.FTSA","10:2.FTSA")

diPAP_lab = c("6:2-diPAP", "8:2-diPAP")
diPAP = c("6:2.diPAP","8:2.diPAP")


# Nombre de carbones des carboxylates et sulfonates

n_C_ALL = c(5,6 ,7,8,9,10,11,12,13,14,
            "FOSA","MeFOSA","EtFOSA","FOSAA", "MeFOSAA", "EtFOSAA",
            4,6,7, 8,8,10,
            "PFECHS",
            "4:2.FTSA","6:2.FTSA","8:2.FTSA","10:2.FTSA",
            "6:2.diPAP","8:2.diPAP",
            "HFPO-DA","NaDONA")


#~~~~~~~~~~~~~~~~~~
#       HBCDD
#~~~~~~~~~~~~~~~~~~

HBCDD = c("a-HBCDD","b-HBCDD","g-HBCDD")
HBCDD_lab = c("a-HBCDD","b-HBCDD","g-HBCDD")

