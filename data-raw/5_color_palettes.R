####################################
##         PROJET CHOPIN
##
##    REPRESENATATIONS GRAPHIQUES
##      PALETTES DE COULEURS
####################################


# # Palette couleur pour les graphiques
# cbPalette<-c("pink", "forestgreen", "lightblue", "gold1", "red", "blue", "purple", "green", "yellow", "gold4", "blue2", "royalblue", "coral", "orange", "grey", "pink3", "orange2", "black", "green2")
# cbPalette2<-c("#b24745", "#ca932f", "#55b144", "#06346a", "#cd87f7", "#ec78c1") # 2e palette pour faire ressortir les soles du graphique isotopes, couleurs trouvées sur imagecolorpicker.com
# PALETTE = c("#1B9E77" ,"#D95F02" ,"#7570B3" ,"#E7298A" ,"#66A61E",
#             "#E6AB02", "#666666","#A6761D", "#B2DF8A","#E31A1C")
#
# PALETTE_PFAS_sed = c(PALETTE,"pink", "forestgreen")

# # Carbo
# "PFNA" = "#440154FF"
# "PFDA" = "#482878FF"
# "PFUnDA" = "#3E4A89FF"
# "PFDoA" = "#31688EFF"
# "PFTrDA" # benthos
# "PFTeDA" # benthos

# # FOSAs
# "FOSA" # benthos
# "MeFOSAA" # sed
# "EtFOSAA"

# # Sulfo
# "L.PFOS"
# "Br.PFOS"
# "PFDS" # sed

# # autres
# "X6.2.FTSA"
# "X10.2.FTSA" # sed


# test = c("PFNA"="#660066",
#          "PFDA"="#31688EFF",
#          "PFUnDA" = "#00cccc",
#          "PFDoA" = "#0099cc",
#
#          "MeFOSAA"="#248f24",
#          "EtFOSAA" = "#ccff99",
#
#          "L.PFOS" = "#ffcc66",
#          "Br.PFOS"="#e68a00",
#          "PFDS" = "#e68a00",
#
#          "#e62e00","red", "grey")

PALETTE_PFAS_sed = c("PFNA" = "#4d194d",
                     "PFDA" = "#9900cc",
                     "PFUnDA" =  "#ff66ff",
                     "PFDoA" = "#0000b3",

                     "MeFOSAA" = "#0099cc",
                     "EtFOSAA" = "#ccffe6" ,

                     "PFOS(L)" = "#51C56AFF" ,
                     "PFOS(Br)" = "#C2DF23FF",
                     "PFDS" = "#FDE725FF",

                     "6:2-FTSA" = "#e67300",
                     "10:2-FTSA" = "#e63900",
                     "Autres" = "#990000")

PALETTE_PFAS_benthos = c("PFNA" = "#4d194d",
                         "PFDA" = "#9900cc",
                         "PFUnDA" = "#ff66ff",
                         "PFDoA" = "#0000b3",
                         "PFTrDA" = "#0066ff",
                         "PFTeDA" = "#80bfff",

                         "FOSA" = "#33cccc",
                         "EtFOSAA" = "#ccff99",

                         "L.PFOS" = "#51C56AFF",

                         "X6.2.FTSA" = "#e67300",
                         "otherPFASs" = "#990000")

PALETTE_PFAS_soles = c("PFNA" = "#4d194d",
                       "PFDA" = "#9900cc",
                       "PFUnDA" = "#ff66ff",
                       "PFDoA" = "#0000b3",
                       "PFTrDA" = "#0066ff",
                       "PFTeDA" = "#80bfff",

                       "FOSA" = "#33cccc",

                       "L.PFOS" = "#51C56AFF",

                       "otherPFASs" = "#990000")

usethis::use_data(PALETTE_PFAS_sed, overwrite = TRUE)
usethis::use_data(PALETTE_PFAS_benthos, overwrite = TRUE)
usethis::use_data(PALETTE_PFAS_soles, overwrite = TRUE)
