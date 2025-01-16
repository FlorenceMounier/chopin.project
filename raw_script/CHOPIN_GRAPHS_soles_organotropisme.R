####################################
##         PROJET CHOPIN
## 
##          GRAPHIQUES
##     ORGANOTROPISME SOLES
##      
####################################



# --------------------------------------------------------------
# Suppression des composâs non analysâs

source('Script/CHOPIN_DATA_FORMATTING_contaminants.R')
PFAS_lab = PFAS_ALL_lab[-which(PFAS_ALL=="MeFOSA"|PFAS_ALL=="EtFOSA")]
ss_famille = ss_famille_ALL[-which(PFAS_ALL=="MeFOSA"|PFAS_ALL=="EtFOSA")]
n_C = n_C_ALL[-which(PFAS_ALL=="MeFOSA"|PFAS_ALL=="EtFOSA")]
PFAS = PFAS_ALL[-which(PFAS_ALL=="MeFOSA"|PFAS_ALL=="EtFOSA")]




#############################################################################
# Fig19_RGS_lipides_G2.jpeg

fig18 = data.frame("Lipides"=c(2.7,3.3,7.5,2.4,5.7,11.5,13.2,12.9,11.9),
                   "RGS"=solesG2_contam$RGS[-c(4,7)])

png("Output/SOLES/Fig19_G2_RGS_lip.png", width = 9,height = 5,units = "cm",res=360)
par(mar=c(4,4,1,1))
ggplot(data = fig18, aes(x = RGS,y=Lipides))+
  geom_point(color="cornflowerblue", cex=3)+
  theme_bw()+
  labs(x="RGS (% pf)", y="Lipides (% ps)")
dev.off()



#############################################################################
# Fig21_Correlation_lipides_G2.jpeg

lm_HBCDD = lm(soles_contam_G2orga[1:39,]$a.HBCDD_pg_gdw ~ soles_contam_G2orga[1:39,]$lip_percent_dw)
lm_CB153 = lm(soles_contam_G2orga[1:39,]$X153_ng_gdw ~ soles_contam_G2orga[1:39,]$lip_percent_dw)
lm_PFOS = lm(soles_contam_G2orga[1:39,]$L.PFOS. ~ soles_contam_G2orga[1:39,]$lip_percent_dw)


p1=ggplot(data = soles_contam_G2orga[1:39,], 
          aes(x=lip_percent_dw,
              y=X153_ng_gdw,
              color=Tissus,fill=Tissus))+
  geom_point(cex=3)+
  geom_smooth(inherit.aes=F,data = soles_contam_G2orga[1:39,],
              aes(x=lip_percent_dw,
                  y=X153_ng_gdw),
              stat="smooth", method="lm", se=F,colour="black")+
  theme_bw()+
  theme(legend.position="none") +
  annotate(size=3,"text",x=10, y=670, 
           label = paste("R2 = ",signif(summary(lm_CB153)$r.squared, 2),
                         "\n p-value =",signif(summary(lm_CB153)$coef[2,4], 2)))+
  labs(y="CB153 (ng.g-1 ps)", x="Lipides (% ps)")



p2=ggplot(data = soles_contam_G2orga[1:39,], 
          aes(x=lip_percent_dw,
              y=a.HBCDD_pg_gdw,
              color=Tissus,fill=Tissus))+
  geom_point(cex=3)+
  geom_smooth(inherit.aes=F,data = soles_contam_G2orga[1:39,],
              aes(x=lip_percent_dw,
                  y=a.HBCDD_pg_gdw),
              stat="smooth", method="lm", se=F,colour="black")+
  theme_bw()+
  theme(legend.position="none") +
  
  annotate(size=3,"text",x=10, y=600, 
           label = paste("R2 = ",signif(summary(lm_HBCDD)$r.squared, 2),
                         "\n p-value =",signif(summary(lm_HBCDD)$coef[2,4], 2)))+
  labs(y="a-HBCDD (pg.g-1 ps)", x="Lipides (% ps)")


p3=ggplot(data = soles_contam_G2orga[1:39,], 
          aes(x=lip_percent_dw,
              y=L.PFOS.,
              color=Tissus,fill=Tissus))+
  geom_point(cex=3)+
  # geom_smooth(inherit.aes=F,data = soles_contam_G2orga[1:39,],
  #             aes(x=lip_percent_dw,
  #                 y=L.PFOS.),
  #             stat="smooth", method="lm", se=F,colour="black")+
  theme_bw()+
  annotate(size=3,"text",x=10, y=63,
           label = paste("R2 = ",signif(summary(lm_PFOS)$r.squared, 2),
                         "\n p-value =",signif(summary(lm_PFOS)$coef[2,4], 2)))+
  labs(y="L-PFOS (ng.g-1 ps)", x="Lipides (% ps)")

plot = ggdraw()+
  draw_plot(p1, x=0, y=0, width=0.3, height=1) +
  draw_plot(p2, x=0.3,y=0, 0.3, 1) +
  draw_plot(p3, x=0.6,y=0, 0.4, 1)

ggsave(filename ="Output/SOLES/Fig21_Correlation_lipides_G2_stats.jpeg",
       plot = plot,device = "jpeg",width = 25,height = 6, units = "cm",dpi=300)



########################################################



# --------------------------------------------------------------
# Suppression des poissons non analys?s

soles_contam_G2orga = soles_contam_G2orga[-which(soles_contam_G2orga$fish_TAG=="112018-G2-08"|
                                                   soles_contam_G2orga$fish_TAG=="112018-G2-16"),]


# --------------------------------------------------------------
# Remplacement des valeurs <LOD/LOQ par 0 pour les HBCDD

soles_contam_G2orga$a.HBCDD_pg_gdw[which(soles_contam_G2orga$fish_TAG=="112018-G2-10")]=0


# --------------------------------------------------------------
# Calcul des masses de lipides par organe

soles_contam_G2orga$g_lip_organe = (soles_contam_G2orga$lip_percent_dw/100)*soles_contam_G2orga$mass_tissue_gdw


# --------------------------------------------------------------
# Calcul des concentrations en ng pour l'HBCDD


for(c in 1:length(HBCDD)){
  soles_contam_G2orga[,paste(HBCDD[c],"_ng_gdw",sep="")] = soles_contam_G2orga[,paste(HBCDD[c],"_pg_gdw",sep="")]/1000
}

# --------------------------------------------------------------
# Calcul des concentrations en poids de lipides

PCB = c("X28","X31","X44","X49","X52","X101","X105",
        "X110","X118","X128","X132","X138","X149",
        "X153","X156","X170",
        "X180","X187","X194")

for(c in 1:length(PCB)){
  soles_contam_G2orga[,paste(PCB[c],"_ng_g.1pl",sep="")] = soles_contam_G2orga[,paste(PCB[c],"_ng_gdw",sep="")]/(soles_contam_G2orga$lip_percent_dw/100)
}

for(c in 1:length(HBCDD)){
  soles_contam_G2orga[,paste(HBCDD[c],"_ng_g.1pl",sep="")] = soles_contam_G2orga[,paste(HBCDD[c],"_ng_gdw",sep="")]/(soles_contam_G2orga$lip_percent_dw/100)
}



# --------------------------------------------------------------
# Calcul des concentrations en poids frais

for(c in 1:length(PCB)){
  soles_contam_G2orga[,paste(PCB[c],"_ng_g.1ww",sep="")] = soles_contam_G2orga[,paste(PCB[c],"_ng_gdw",sep="")]*((soles_contam_G2orga$dry_content_percent_tissue)/100)
}

for(c in 1:length(PFAS)){
  soles_contam_G2orga[,paste(PFAS[c],"_ng_g.1ww",sep="")] = soles_contam_G2orga[,paste(PFAS[c],".",sep="")]*((soles_contam_G2orga$dry_content_percent_tissue)/100)
}

for(c in 1:length(HBCDD)){
  soles_contam_G2orga[,paste(HBCDD[c],"_ng_g.1ww",sep="")] = soles_contam_G2orga[,paste(HBCDD[c],"_ng_gdw",sep="")]*((soles_contam_G2orga$dry_content_percent_tissue)/100)
}

# --------------------------------------------------------------
# Calcul des quantit?s de contaminants

for(c in 1:length(PCB)){
  soles_contam_G2orga[,paste(PCB[c],"_ng",sep="")] = soles_contam_G2orga[,paste(PCB[c],"_ng_gdw",sep="")]*soles_contam_G2orga$mass_tissue_gdw
}
for(c in 1:length(PFAS)){
  soles_contam_G2orga[,paste(PFAS[c],"_ng",sep="")] = soles_contam_G2orga[,paste(PFAS[c],".",sep="")]*soles_contam_G2orga$mass_tissue_gdw
}

for(c in 1:length(HBCDD)){
  soles_contam_G2orga[,paste(HBCDD[c],"_ng",sep="")] = soles_contam_G2orga[,paste(HBCDD[c],"_ng_gdw",sep="")]*soles_contam_G2orga$mass_tissue_gdw
}


# --------------------------------------------------------------
# Calcul des sommes par famille

soles_contam_G2orga$sommePCB_ng = apply(soles_contam_G2orga[,140:158], MARGIN = 1, FUN = sum)

soles_contam_G2orga$sommePFAS_ng = apply(soles_contam_G2orga[,159:187], MARGIN = 1, FUN = sum)

# soles_contam_G2orga$sommePFCA = apply(soles_contam_G2orga[,45:54], MARGIN = 1, FUN = sum)
# soles_contam_G2orga$sommeFOSA = apply(soles_contam_G2orga[,55:58], MARGIN = 1, FUN = sum)
# soles_contam_G2orga$sommePFSA = apply(soles_contam_G2orga[,59:64], MARGIN = 1, FUN = sum)
# soles_contam_G2orga$sommeautres = apply(soles_contam_G2orga[,65:73], MARGIN = 1, FUN = sum)

soles_contam_G2orga$sommeHBCDD_ng = apply(cbind(soles_contam_G2orga$a.HBCDD_ng,
                                                soles_contam_G2orga$b.HBCDD_ng,
                                                soles_contam_G2orga$g.HBCDD_ng), MARGIN = 1, FUN = sum)


# --------------------------------------------------------------
# Calcul indices

total = soles_contam_G2orga[which(soles_contam_G2orga$Tissus=="foie"),c(1,3,4,5,7)]
foie = soles_contam_G2orga[which(soles_contam_G2orga$Tissus=="foie"),]
reste = soles_contam_G2orga[which(soles_contam_G2orga$Tissus=="reste"),]
gonades = soles_contam_G2orga[which(soles_contam_G2orga$Tissus=="gonades"),]

total$RHS_pf = foie$mass_tissue_gww / foie$mass_fish_gww*100
total$RGS_pf = gonades$mass_tissue_gww / gonades$mass_fish_gww*100
total$glip_tot = foie$g_lip_organe + reste$g_lip_organe + gonades$g_lip_organe

for(c in 1:length(PCB)){
  total[,paste(PCB[c],"_somme_ng",sep="")] = foie[,paste(PCB[c],"_ng",sep="")] + gonades[,paste(PCB[c],"_ng",sep="")] + reste[,paste(PCB[c],"_ng",sep="")]
}
for(c in 1:length(PFAS)){
  total[,paste(PFAS[c],"_somme_ng",sep="")] = foie[,paste(PFAS[c],".",sep="")] + gonades[,paste(PFAS[c],".",sep="")] + reste[,paste(PFAS[c],".",sep="")]
}

for(c in 1:length(HBCDD)){
  total[,paste(HBCDD[c],"_somme_ng",sep="")] = foie[,paste(HBCDD[c],"_ng",sep="")] + gonades[,paste(HBCDD[c],"_ng",sep="")] + reste[,paste(HBCDD[c],"_ng",sep="")]
}

#-------------------

prop_PCB_foie =  cbind("RHS_pf"=total$RHS_pf, data.frame(100*foie[,140:158] / total[,8:26]))
prop_PCB_reste =  data.frame(100*reste[,140:158] / total[,8:26])
prop_PCB_gonades = cbind("RGS_pf"=total$RGS_pf, data.frame(100*gonades[,140:158] / total[,8:26]))

ggplot(data=prop_PCB_gonades, aes(x=RGS_pf,y=X153_ng))+
  geom_point()
cor.test(prop_PCB_gonades$RGS_pf,prop_PCB_gonades$X153_ng)
summary(lm(prop_PCB_gonades$X153_ng ~ prop_PCB_gonades$RGS_pf))

ggplot(data=prop_PCB_foie, aes(x=RHS_pf,y=X153_ng))+
  geom_point()
cor.test(prop_PCB_foie$RHS_pf,prop_PCB_foie$X153_ng)
summary(lm(prop_PCB_foie$X153_ng ~ prop_PCB_foie$RHS_pf))

#-------------------

prop_HBCDD_foie = cbind("RHS_pf"=total$RHS_pf, data.frame(100*foie[,188:190] / total[,56:58]))
prop_HBCDD_reste = data.frame(100*reste[,188:190] / total[,56:58])
prop_HBCDD_gonades = cbind("RGS_pf"=total$RGS_pf, data.frame(100*gonades[,188:190] / total[,56:58]))

ggplot(data=prop_HBCDD_gonades, aes(x=RGS_pf,y=a.HBCDD_ng))+
  geom_point()
cor.test(prop_HBCDD_gonades$RGS_pf,prop_HBCDD_gonades$a.HBCDD_ng)
summary(lm(prop_HBCDD_gonades$a.HBCDD_ng ~ prop_HBCDD_gonades$RGS_pf))

ggplot(data=prop_HBCDD_foie, aes(x=RHS_pf,y=a.HBCDD_ng))+
  geom_point()
cor.test(prop_HBCDD_foie$RHS_pf,prop_HBCDD_foie$a.HBCDD_ng)
summary(lm(prop_HBCDD_foie$a.HBCDD_ng ~ prop_HBCDD_foie$RHS_pf))

#-------------------

prop_PFAS_foie = cbind("RHS_pf"=total$RHS_pf, data.frame(100*foie[,159:187] / total[,27:55]))
prop_PFAS_reste = data.frame(100*reste[,159:187] / total[,27:55])
prop_PFAS_gonades = cbind("RGS_pf"=total$RGS_pf, data.frame(100*gonades[,159:187] / total[,27:55]))


ggplot(data=prop_PFAS_gonades, aes(x=RGS_pf,y=L.PFOS_ng))+
  geom_point()
cor.test(prop_PFAS_gonades$RGS_pf,prop_PFAS_gonades$L.PFOS_ng)
summary(lm(prop_PFAS_gonades$L.PFOS_ng ~ prop_PFAS_gonades$RGS_pf))

ggplot(data=prop_PFAS_foie, aes(x=RHS_pf,y=L.PFOS_ng))+
  geom_point()
cor.test(prop_PFAS_foie$RHS_pf,prop_PFAS_foie$L.PFOS_ng)
summary(lm(prop_PFAS_foie$L.PFOS_ng ~ prop_PFAS_foie$RHS_pf))


##################################################################################################



G2 = data.frame("TAG"=rep.int(x = foie$fish_TAG, 3),
                "tissue"= c(rep.int(x = "foie",times = 11), rep.int(x = "reste",times = 11),rep.int(x = "gonades",times = 11)),
                "CB153"= c( (foie$X153_ng_g.1pl * foie$g_lip_organe)/total$glip_tot,
                            (reste$X153_ng_g.1pl * reste$g_lip_organe)/total$glip_tot,
                            (gonades$X153_ng_g.1pl * gonades$g_lip_organe)/total$glip_tot),
                "a.HBCDD"= c( (foie$a.HBCDD_ng_g.1pl * foie$g_lip_organe)/total$glip_tot,
                              (reste$a.HBCDD_ng_g.1pl* reste$g_lip_organe)/total$glip_tot,
                              (gonades$a.HBCDD_ng_g.1pl* gonades$g_lip_organe)/total$glip_tot),
                "L.PFOS"= c( (foie$L.PFOS_ng_g.1ww * foie$mass_tissue_gww)/total$mass_fish_gww,
                             (reste$L.PFOS_ng_g.1ww * reste$mass_tissue_gww)/total$mass_fish_gww,
                             (gonades$L.PFOS_ng_g.1ww * gonades$mass_tissue_gww)/total$mass_fish_gww)
)

G2$TAG = rep.int(c("G2\n-05","G2\n-06","G2\n-07","G2\n-09","G2\n-10","G2\n-11","G2\n-12","G2\n-13","G2\n-14","G2\n-15","G2\n-17"),
                 times = 3)


p1=ggplot(data = G2[-which(G2$TAG=="G2\n-09"|G2$TAG=="G2\n-14"),],aes(x=TAG,y=CB153, fill=tissue))+
  geom_bar(stat = "identity")+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size = 4))+
  labs(title=NULL, x=NULL, y=expression(paste("CB153 (ng.",g[pl]^-1,")")))

p2=ggplot(data = G2[-which(G2$TAG=="G2\n-05"|G2$TAG=="G2\n-06"|G2$TAG=="G2\n-07"|G2$TAG=="G2\n-09"|G2$TAG=="G2\n-10"|G2$TAG=="G2\n-14"),],
          aes(x=TAG,y=a.HBCDD, fill=tissue))+
  theme_bw() +
  geom_bar(stat = "identity")+
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size = 4))+
  labs(title=NULL, x=NULL, y=expression(paste(alpha,"-HBCDD (pg.",g[pl]^-1,")")))

p3=ggplot(data = G2[-which(G2$TAG=="G2\n-06"|G2$TAG=="G2\n-10"|G2$TAG=="G2\n-11"|G2$TAG=="G2\n-17"),],
          aes(x=TAG,y=L.PFOS, fill=tissue))+
  scale_fill_discrete(name="Tissus")+
  theme_bw() +
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(size = 4))+
  labs(title=NULL, x=NULL, y=expression(paste("L-PFOS (ng.",g[pf]^-1,")")))

plot = ggdraw()+
  draw_plot(p1, x=0, y=0, width=0.35, height=1) +
  draw_plot(p2, x=0.35,y=0, 0.25, 1) +
  draw_plot(p3, x=0.60,y=0, 0.40, 1)

ggsave(filename = "Output/SOLES/soles_G2.jpeg",
       plot = plot,device = "jpeg",width = 20,height = 6, units = "cm",dpi=300)


#######################################################################################################

G2_ng.glip = data.frame("TAG"=rep.int(x = foie$fish_TAG, 3),
                        "tissue"= c(rep.int(x = "foie",times = 11), rep.int(x = "reste",times = 11),rep.int(x = "gonades",times = 11)),
                        "CB153"= c(foie$X153_ng_g.1pl, reste$X153_ng_g.1pl, gonades$X153_ng_g.1pl),
                        "a.HBCDD"= c(foie$a.HBCDD_ng_g.1pl, reste$a.HBCDD_ng_g.1pl, gonades$a.HBCDD_ng_g.1pl),
                        "L.PFOS"= c(foie$L.PFOS_ng_g.1ww,reste$L.PFOS_ng_g.1ww,gonades$L.PFOS_ng_g.1ww)
)

G2_ng.glip$TAG = rep.int(c("G2\n-05","G2\n-06","G2\n-07","G2\n-09","G2\n-10","G2\n-11","G2\n-12","G2\n-13","G2\n-14","G2\n-15","G2\n-17"),
                         times = 3)

wilcox.test(G2_ng.glip$CB153[G2$tissue=="foie"], G2_ng.glip$CB153[G2$tissue=="gonades"])
wilcox.test(G2_ng.glip$a.HBCDD[G2$tissue=="foie"], G2_ng.glip$a.HBCDD[G2$tissue=="gonades"])
wilcox.test(G2_ng.glip$L.PFOS[G2$tissue=="foie"], G2_ng.glip$L.PFOS[G2$tissue=="gonades"])

res_G2_lip = data.frame("name"= c(rep.int("CB153 \n foie",times=11),rep.int("CB153 \n gonades",times=11),
                                  rep.int("a.HBCDD \n foie",times=11),rep.int("a.HBDD \n gonades",times=11)),
                        "organe"=c(rep.int("foie",times=11),rep.int("gonades",times=11),
                                   rep.int("foie",times=11),rep.int("gonades",times=11)),
                        "contam"= c(G2_ng.glip$CB153[G2_ng.glip$tissue=="foie"],G2_ng.glip$CB153[G2_ng.glip$tissue=="gonades"],
                                    G2_ng.glip$a.HBCDD[G2_ng.glip$tissue=="foie"]*1000,
                                    G2_ng.glip$a.HBCDD[G2_ng.glip$tissue=="gonades"]*1000))


plot_G2 = ggplot(data=res_G2_lip, aes(x=name, y=contam, fill=organe))+
  theme_bw() +
  geom_boxplot()+
  annotate(size=3,"text",x=3.5,y=2500,label="Test de rang de Wilcoxon \n p-value=0.06253")+
  annotate(size=3,"text",x=1.5,y=2500,label="Test de rang de Wilcoxon \n p-value=0.8726")+
  labs(title=NULL, x=NULL, y=expression(paste("Concentration en  a-HBCDD (pg.",g[pl]^-1,")  &  CB153 (ng.",g[pl]^-1,")")))+
  theme(axis.title.y = element_text(size=8))
ggsave(filename = "C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/2020.06 - Rapport FINAL/SOLES/soles_G2_organes_normalise_lip.jpeg",
       plot = plot_G2,device = "jpeg",width = 15,height = 10, units = "cm",dpi=300)

