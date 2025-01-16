####################################
##         PROJET CHOPIN
## 
##       FONCTION ANALYSE 
##   STATISTIQUES SUR LES SOMMES
####################################


#-------------------------------------------------
# EFFECTIFS analyses et detectes/quantifies

stat_somme_facteur = function(dataset,facteur,title){
  
  data.frame("Facteur" = paste(title,levels(as.factor(dataset[,facteur]))),
    
             "N total" = summary(as.factor(dataset[,facteur])),
             
             # PCB

             "N analysed PCB" = ifelse(test = length(which(is.na(dataset$sommePCB_ng_gdw)==T))==0,
                                       yes = tapply(na.omit(dataset$sommePCB_ng_gdw), 
                                                    INDEX = dataset[,facteur], FUN = length),
                                       no = tapply(na.omit(dataset$sommePCB_ng_gdw), 
                                                   INDEX = dataset[,facteur][-which(is.na(dataset$sommePCB_ng_gdw)==T)], FUN = length))
             ,
             "Mediane PCB" = round(tapply(dataset$sommePCB_ng_gdw, INDEX = dataset[,facteur], FUN = median, na.rm=T),digits = 0),
             "Moyenne PCB" = round(tapply(dataset$sommePCB_ng_gdw, INDEX = dataset[,facteur], FUN = mean, na.rm=T),digits = 0),
             "Min PCB" = round(tapply(dataset$sommePCB_ng_gdw, INDEX = dataset[,facteur], FUN = min, na.rm=T),digits = 0),
             "Max PCB" = round(tapply(dataset$sommePCB_ng_gdw, INDEX = dataset[,facteur], FUN = max, na.rm=T),digits = 0),
             
             # PFAS
             "N analysed PFAS" = tapply(na.omit(dataset$sommePFAS_ng_gdw), 
                                        dataset[,facteur][-which(is.na(dataset$sommePFAS_ng_gdw)==T)], FUN = length),
             "Mediane PFAS" = round(tapply(dataset$sommePFAS_ng_gdw, dataset[,facteur], FUN = median, na.rm=T),digits = 1),
             "Moyenne PFAS" = round(tapply(dataset$sommePFAS_ng_gdw, dataset[,facteur], FUN = mean, na.rm=T),digits = 1),
             "Min PFAS" = round(tapply(dataset$sommePFAS_ng_gdw, dataset[,facteur], FUN = min, na.rm=T),digits = 1),
             "Max PFAS" = round(tapply(dataset$sommePFAS_ng_gdw, dataset[,facteur], FUN = max, na.rm=T),digits = 1),
             
             # HBCDD
             "N analysed HBCDD" = tapply(na.omit(dataset$sommeHBCDD_ng_gdw), 
                                         dataset[,facteur][-which(is.na(dataset$sommeHBCDD_ng_gdw)==T)], FUN = length),
             "Mediane HBCDD" = round(tapply(dataset$sommeHBCDD_ng_gdw, dataset[,facteur], FUN = median, na.rm=T),digits = 2),
             "Moyenne HBCDD" = round(tapply(dataset$sommeHBCDD_ng_gdw, dataset[,facteur], FUN = mean, na.rm=T),digits = 2),
             "Min HBCDD" = round(tapply(dataset$sommeHBCDD_ng_gdw, dataset[,facteur], FUN = min, na.rm=T),digits = 2),
             "Max HBCDD" = round(tapply(dataset$sommeHBCDD_ng_gdw, dataset[,facteur], FUN = max, na.rm=T),digits = 2)
  )
}
