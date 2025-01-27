#############################################
###  CHOPIN - Facteurs de transfert

# clear workspace
rm(list=ls())

# save working directory
wd <- getwd()

#-----------------------
## Installation 

library(xlsx)


##---------------------
# DATA

soles_contam = read.xlsx(file = "C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/DATA/CHOPIN_BASE_DE_DONNEES_GENERALE.xlsx",
                         sheetName= "soles", h=T, dec=",")

soles_contam$month = soles_contam$season
soles_contam$month = format(as.Date(soles_contam$month), format="%m")
soles_contam$month[which(soles_contam$month=="06")] = "juin"
soles_contam$month[which(soles_contam$month=="09"|
                           soles_contam$month=="10"|
                           soles_contam$month=="11")] = "octobre"

# moins non dosés 
soles_contam = soles_contam[-c(45,53),]

soles_contam$year = as.factor(soles_contam$year)


soles_contam$species= as.character(soles_contam$species)
soles_contam$species[which(soles_contam$type=="G0")] = "SoleG0"
soles_contam$species[which(soles_contam$type=="G1")] = "SoleG1"
soles_contam$species[which(soles_contam$type=="G2")] = "SoleG2"
soles_contam$species = factor(soles_contam$species)

benthos_contam = read.xlsx(file = "C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/DATA/CHOPIN_BASE_DE_DONNEES_GENERALE.xlsx",
                           sheetName= "benthos", h=T, dec=",")


sed_contam = read.xlsx(file = "C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/DATA/CHOPIN_BASE_DE_DONNEES_GENERALE.xlsx",
                       sheetName= "sediments", h=T, dec=",")
sed_contam = sed_contam[-c(25:70),]

soles_contam$period = as.factor(paste(soles_contam$year, soles_contam$month))
benthos_contam$period = paste(benthos_contam$year, benthos_contam$season)
sed_contam$period = factor(paste(sed_contam$year, sed_contam$season))

soles_contam$zone[which(is.na(soles_contam$zone))] = "Embouchure"

save.image("C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/DATA/image_data_chopin.RData")




BMF_G0 = read.csv2(paste("C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/ANALYSES TROPHIQUES/BMF_solesG0.csv", sep=""), h=T)
BMF_G1 = read.csv2(paste("C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/ANALYSES TROPHIQUES/BMF_solesG1.csv", sep=""), h=T)
BMF_G2 = read.csv2(paste("C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/ANALYSES TROPHIQUES/BMF_solesG2.csv", sep=""), h=T)

BMF_G0$group = rep.int("G0", dim(BMF_G0)[1])
BMF_G1$group = rep.int("G1", dim(BMF_G1)[1])
BMF_G2$group = rep.int("G2", dim(BMF_G2)[1])

BMFsoles = rbind(BMF_G0, BMF_G1, BMF_G2)

BMFsoles_HBCDD = BMFsoles[which(BMFsoles$Contaminant=="a-HBCDD"),]
BMFsoles_PFOS = BMFsoles[which(BMFsoles$Contaminant=="L-PFOS"),]
BMFsoles_CB153 = BMFsoles[which(BMFsoles$Contaminant=="CB153"),]

summary(BMFsoles_HBCDD)

p_HBCDD = ggplot(BMFsoles_HBCDD, aes(x=species, y=mean, color=group, fill=group)) +
  theme_bw() +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=min, ymax=max), color="black", position=position_dodge()) +
  theme(axis.text.x=element_text(size=10, angle=55, hjust=1)) +
  labs(y="Moyenne (min-max)", x="Classe d'âge", title="BMF a-HBCDD des soles de l'estuaire de la Seine classées en fonction de la classe taxonomique de leurs proies") 

gg.gap(plot=p_HBCDD,
       segments=c(3750,3800),
       tick_width = c(250,10000),
       rel_heights=c(0.7,0,0.3),
       ylim=c(0,81000))


summary(BMFsoles_PFOS)

ggplot(BMFsoles_PFOS, aes(x=species, y=mean, color=group, fill=group)) +
  theme_bw() +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=min, ymax=max), color="black", position=position_dodge()) +
  theme(axis.text.x=element_text(size=10, angle=55, hjust=1)) +
  labs(y="Moyenne (min-max)", x="Classe d'âge", title="BMF L-PFOS des soles de l'estuaire de la Seine classées en fonction de la classe taxonomique de leurs proies") 

summary(BMFsoles_CB153)

p_CB153 = ggplot(BMFsoles_CB153, aes(x=species, y=mean, color=group, fill=group)) +
  theme_bw() +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=min, ymax=max), color="black", position=position_dodge()) +
  theme(axis.text.x=element_text(size=10, angle=55, hjust=1)) +
  labs(y="Moyenne (min-max)", x="Classe d'âge", title="BMF CB153 des soles de l'estuaire de la Seine classées en fonction de la classe taxonomique de leurs proies") 
gg.gap(plot=p_CB153,
       segments=c(30,40),
       tick_width = c(2,10),
       rel_heights=c(0.8,0,0.2),
       ylim=c(0,80))

library(ggplot2)
library(gg.gap)
data(mtcars)
p<-ggplot(data = mtcars, aes(x = gear, fill = gear)) +
  geom_bar() +
  ggtitle("Number of Cars by Gear") +
  xlab("Gears")



#single segments and missing tick_width
gg.gap(plot=p,
       segments=c(5,10),
       ylim=c(0,50))
#tick_width can be one or more numbers
gg.gap(plot=p,
       segments=c(5,10),
       tick_width = c(1,10),
       ylim=c(0,50))
#segments list cantains more than one number vectors
gg.gap(plot=p,
       segments=list(c(2.5,4),c(5,10)),
       tick_width = c(1,0.5,10),
       ylim=c(0,50))
#rel_heights can set the relative height for segments and segmented y-axis
gg.gap(plot=p,
       segments=list(c(2.5,4),c(5,10)),
       tick_width = c(1,0.5,10),
       rel_heights=c(0.2,0,0.2,0,1),
       ylim=c(0,50))
#reversed y-axis
p<-ggplot(data = mtcars, aes(x = gear, fill = gear)) +
  geom_bar() +
  ggtitle("Number of Cars by Gear") +
  xlab("Gears")+
  scale_y_continuous(trans = 'reverse')
#single segments and missing tick_width
gg.gap(plot=p,
       segments=c(10,5),
       ylim=c(15,0))

#########################
##  BSAF proies


FUN_BSAF_prey = function(chemical_benth, chemical_sed){
  
  BSAF_prey_mean=BSAF_prey_n=BSAF_prey_sd = BSAF_prey_min =BSAF_prey_max =list()
  
  for(p in 1:length(levels(benthos_contam$species))){
    BSAF_prey_mean[[p]] = BSAF_prey_n[[p]] =BSAF_prey_sd[[p]] =BSAF_prey_min[[p]] =BSAF_prey_max[[p]] = 
      matrix(nrow=length(levels(benthos_contam$zone)),
             ncol = length(levels(sed_contam$period)), 
             dimnames=list(levels(benthos_contam$zone),
                           levels(sed_contam$period)))
    for(z in 1:length(levels(soles_contam$zone))){
      for(d in 1:length(levels(sed_contam$period))){
        benth_tmps = benthos_contam[which(benthos_contam$zone==levels(soles_contam$zone)[z]&
                                            benthos_contam$period==levels(sed_contam$period)[d]&
                                            benthos_contam$species==levels(benthos_contam$species)[p]),chemical_benth]
        
        sed_tmp = sed_contam[which(sed_contam$zone==levels(soles_contam$zone)[z]&
                                     sed_contam$period==levels(sed_contam$period)[d]),chemical_sed]
        BSF_tmp =c()
        for(i in 1:length(benth_tmps)){
          for(j in 1: length(sed_tmp)){
            BSF_tmp =c(BSF_tmp, benth_tmps[i]/sed_tmp[j])
          }
        }
        
        BSAF_prey_mean[[p]][z,d] = mean(BSF_tmp, na.rm=T)
        BSAF_prey_n[[p]][z,d] = length(BSF_tmp)
        BSAF_prey_sd[[p]][z,d] = sd(BSF_tmp, na.rm=T)
        BSAF_prey_min[[p]][z,d] = min(BSF_tmp, na.rm=T)
        BSAF_prey_max[[p]][z,d] = max(BSF_tmp, na.rm=T)
      }
    }}
  
  
  names(BSAF_prey_mean) = names(BSAF_prey_n) =names(BSAF_prey_sd) =names(BSAF_prey_min) =names(BSAF_prey_max) =levels(benthos_contam$species)
  print(BSAF_prey_mean)
  
  res_BSAF_benthos = cbind("mean"=lapply(X = BSAF_prey_mean, FUN = mean, na.rm=T),
                           "n_cmb"=lapply(X = BSAF_prey_n, FUN = sum, na.rm=T),
                           "min"=lapply(X = BSAF_prey_min, FUN = min, na.rm=T),
                           "max"=lapply(X = BSAF_prey_max, FUN = max, na.rm=T))
  print(res_BSAF_benthos)
  write.csv2(res_BSAF_benthos, 
  file = paste("C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/ANALYSES TROPHIQUES/BSAF_benth_",
               chemical_benth,".csv", sep=""))
  }


FUN_BSAF_prey(chemical_benth="CB153_ng_g.1ps", chemical_sed="CB153")
FUN_BSAF_prey(chemical_benth="L.PFOS.", chemical_sed="L.PFOS.")
FUN_BSAF_prey(chemical_benth="a.HBCDD_ng_g.1ps", chemical_sed="a.HBCDD")



#########################
##  BSAF soles

FUN_BSAF_sole = function(chemical_sole, chemical_sed){
  
  BSAF_sole_mean=BSAF_sole_n=BSAF_sole_sd = BSAF_sole_min =BSAF_sole_max =list()
  
  for(p in 1:length(levels(soles_contam$species))){
    BSAF_sole_mean[[p]] = BSAF_sole_n[[p]] =BSAF_sole_sd[[p]] =BSAF_sole_min[[p]] =BSAF_sole_max[[p]] = 
      matrix(nrow=length(levels(soles_contam$zone)),
             ncol = length(levels(sed_contam$period)), 
             dimnames=list(levels(soles_contam$zone),
                           levels(sed_contam$period)))
    for(z in 1:length(levels(soles_contam$zone))){
      for(d in 1:length(levels(sed_contam$period))){
        sole_tmps = soles_contam[which(soles_contam$zone==levels(soles_contam$zone)[z]&
                                            soles_contam$period==levels(sed_contam$period)[d]&
                                            soles_contam$species==levels(soles_contam$species)[p]),chemical_sole]
        
        sed_tmp = sed_contam[which(sed_contam$zone==levels(soles_contam$zone)[z]&
                                     sed_contam$period==levels(sed_contam$period)[d]),chemical_sed]
        BSF_tmp =c()
        for(i in 1:length(sole_tmps)){
          for(j in 1: length(sed_tmp)){
            BSF_tmp =c(BSF_tmp, sole_tmps[i]/sed_tmp[j])
          }
        }
        
        BSAF_sole_mean[[p]][z,d] = mean(BSF_tmp, na.rm=T)
        BSAF_sole_n[[p]][z,d] = length(BSF_tmp)
        BSAF_sole_sd[[p]][z,d] = sd(BSF_tmp, na.rm=T)
        BSAF_sole_min[[p]][z,d] = min(BSF_tmp, na.rm=T)
        BSAF_sole_max[[p]][z,d] = max(BSF_tmp, na.rm=T)
      }
    }}
  
  
  names(BSAF_sole_mean) = names(BSAF_sole_n) =names(BSAF_sole_sd) =names(BSAF_sole_min) =names(BSAF_sole_max) =levels(soles_contam$species)
  print(BSAF_sole_mean)
  
  res_BSAF_soles = cbind("mean"=lapply(X = BSAF_sole_mean, FUN = mean, na.rm=T),
                           "n_cmb"=lapply(X = BSAF_sole_n, FUN = sum, na.rm=T),
                           "min"=lapply(X = BSAF_sole_min, FUN = min, na.rm=T),
                           "max"=lapply(X = BSAF_sole_max, FUN = max, na.rm=T))
  print(res_BSAF_soles)
  write.csv2(res_BSAF_soles, 
             file = paste("C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/ANALYSES TROPHIQUES/BSAF_sole_",
                          chemical_sole,".csv", sep=""))
}

FUN_BSAF_sole(chemical_sole="CB153_ng_g.1ps", chemical_sed="CB153")
FUN_BSAF_sole(chemical_sole="L.PFOS", chemical_sed="L.PFOS.")
FUN_BSAF_sole(chemical_sole="a.HBCDD_ng_g.1ps", chemical_sed="a.HBCDD")


#################################Z
### BSAF zone

FUN_BSAF_zone_prey = function(chemical_benth, chemical_sed){
  
  BSAF_zone_prey_mean=BSAF_zone_prey_n=BSAF_zone_prey_sd = BSAF_zone_prey_min =BSAF_zone_prey_max =list()
  
  for(z in 1:length(levels(soles_contam$zone))){
    BSAF_zone_prey_mean[[z]] = BSAF_zone_prey_n[[z]] =BSAF_zone_prey_sd[[z]] =BSAF_zone_prey_min[[z]] =BSAF_zone_prey_max[[z]] = 
      matrix(nrow=length(levels(benthos_contam$species)),
             ncol = length(levels(sed_contam$period)), 
             dimnames=list(levels(benthos_contam$species),
                           levels(sed_contam$period)))
    for(p in 1:length(levels(benthos_contam$species))){
      for(d in 1:length(levels(sed_contam$period))){
        benth_tmps = benthos_contam[which(benthos_contam$zone==levels(benthos_contam$zone)[z]&
                                            sed_contam$period==levels(sed_contam$period)[d]&
                                            benthos_contam$species==levels(benthos_contam$species)[p]),chemical_benth]
        
        sed_tmp = sed_contam[which(sed_contam$zone==levels(benthos_contam$zone)[z]&
                                     sed_contam$period==levels(sed_contam$period)[d]),chemical_sed]
        BSF_tmp =c()
        for(i in 1:length(benth_tmps)){
          for(j in 1: length(sed_tmp)){
            BSF_tmp =c(BSF_tmp, benth_tmps[i]/sed_tmp[j])
          }
        }
        
        BSAF_zone_prey_mean[[z]][p,d] = mean(BSF_tmp, na.rm=T)
        BSAF_zone_prey_n[[z]][p,d] = length(BSF_tmp)
        BSAF_zone_prey_sd[[z]][p,d] = sd(BSF_tmp, na.rm=T)
        BSAF_zone_prey_min[[z]][p,d] = min(BSF_tmp, na.rm=T)
        BSAF_zone_prey_max[[z]][p,d] = max(BSF_tmp, na.rm=T)
      }
    }}
  
  
  names(BSAF_zone_prey_mean) = names(BSAF_zone_prey_n) =names(BSAF_zone_prey_sd) =names(BSAF_zone_prey_min) =names(BSAF_zone_prey_max) =levels(benthos_contam$zone)
  print(BSAF_zone_prey_mean)
  
  res_BSAF_benthos = cbind("mean"=lapply(X = BSAF_zone_prey_mean, FUN = mean, na.rm=T),
                           "n_cmb"=lapply(X = BSAF_zone_prey_n, FUN = sum, na.rm=T),
                           "min"=lapply(X = BSAF_zone_prey_min, FUN = min, na.rm=T),
                           "max"=lapply(X = BSAF_zone_prey_max, FUN = max, na.rm=T))
  print(res_BSAF_benthos)
  
  write.csv2(res_BSAF_benthos, 
             file = paste("C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/ANALYSES TROPHIQUES/BSAF_zone_benthos_",
                          chemical_benth,".csv", sep=""))
  
}


FUN_BSAF_zone_prey(chemical_benth="CB153_ng_g.1ps", chemical_sed="CB153")
FUN_BSAF_zone_prey(chemical_benth="L.PFOS.", chemical_sed="L.PFOS.")
FUN_BSAF_zone_prey(chemical_benth="a.HBCDD_ng_g.1ps", chemical_sed="a.HBCDD")



#################################Z
### BMF

FUN_BMF_sole = function(chemical_sole, chemical_benth){
  
  BMF_sole_mean=BMF_sole_n=BMF_sole_sd = BMF_sole_min =BMF_sole_max =list()
  
  for(p in 1:length(levels(soles_contam$species))){
    BMF_sole_mean[[p]] = BMF_sole_n[[p]] =BMF_sole_sd[[p]] =BMF_sole_min[[p]] =BMF_sole_max[[p]] = 
     rep.int(NA, times = length(levels(benthos_contam$species)))
    for(z in 1:length(levels(benthos_contam$species))){
        sole_tmps = soles_contam[which(soles_contam$species==levels(soles_contam$species)[p]),chemical_sole]
        
        benth_tmp = benthos_contam[which(benthos_contam$species==levels(benthos_contam$species)[z]),chemical_benth]
        
        BMF_tmp =c()
        for(i in 1:length(sole_tmps)){
          for(j in 1: length(benth_tmp)){
            if(is.na(sole_tmps[i])==T|is.na(benth_tmp[i])==T){
              BMF_tmp =c(BMF_tmp, NA)
            }else{
              BMF_tmp =c(BMF_tmp, sole_tmps[i]/benth_tmp[j])
            }
 
        }}
        
        BMF_sole_mean[[p]][z] = mean(BMF_tmp, na.rm=T)
        BMF_sole_n[[p]][z] = length(BMF_tmp)
        BMF_sole_sd[[p]][z] = sd(BMF_tmp, na.rm=T)
        BMF_sole_min[[p]][z] = min(BMF_tmp, na.rm=T)
        BMF_sole_max[[p]][z] = max(BMF_tmp, na.rm=T)
    }
    names(BMF_sole_mean[[p]]) = names(BMF_sole_n[[p]]) =names(BMF_sole_sd[[p]]) =
      names(BMF_sole_min[[p]]) = names(BMF_sole_max[[p]]) = levels(benthos_contam$species)
    
    res_p = cbind(BMF_sole_mean[[p]], BMF_sole_n[[p]], BMF_sole_sd[[p]], BMF_sole_min[[p]], BMF_sole_max[[p]])
    colnames(res_p) = c("mean", "n", "sd", "min", "max")
    rownames(res_p) = levels(benthos_contam$species)
    
    write.csv2(res_p, 
               file = paste("C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/ANALYSES TROPHIQUES/BMF_sole_",
                            chemical_sole,levels(soles_contam$species)[p],".csv", sep=""))
  }
  
  
  names(BMF_sole_mean) = names(BMF_sole_n) =names(BMF_sole_sd) =names(BMF_sole_min) =names(BMF_sole_max) =levels(soles_contam$species)
  print(BMF_sole_mean)
  
  
  res_BMF_soles = cbind("mean"=lapply(X = BMF_sole_mean, FUN = mean, na.rm=T),
                         "n_cmb"=lapply(X = BMF_sole_n, FUN = sum, na.rm=T),
                         "min"=lapply(X = BMF_sole_min, FUN = min, na.rm=T),
                         "max"=lapply(X = BMF_sole_max, FUN = max, na.rm=T))
  print(res_BMF_soles)
  write.csv2(res_BMF_soles, 
             file = paste("C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/ANALYSES TROPHIQUES/BMF_sole_",
                          chemical_sole,".csv", sep=""))
}

FUN_BMF_sole(chemical_sole="CB153_ng_g.1ps", chemical_benth="CB153_ng_g.1ps")
FUN_BMF_sole(chemical_sole="L.PFOS", chemical_benth="L.PFOS.")
FUN_BMF_sole(chemical_sole="a.HBCDD_ng_g.1ps", chemical_benth="a.HBCDD_ng_g.1ps")

