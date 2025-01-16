####################################
##         PROJET CHOPIN
## 
##           ANALYSES
##           BENTHOS
##      CORRELATION LIPIDES
##      
####################################



plot(benthos_contam$lip_PS_percent, benthos_contam$sommePCB_ng_gdw,
     ylab = "Somme des PCB (ng.g-1 ps)",
     xlab = "Taux de lipides (% ps)")
lm1 = lm(benthos_contam$sommePCB_ng_gdw ~ benthos_contam$lip_PS_percent)
if(summary(lm1)$coef[2,4]<0.05){
  abline(lm1)
}



plot(benthos_contam$lip_PS_percent, benthos_contam$sommeHBCDD_ng_gdw,
     ylab = "Somme des HBCDD (ng.g-1 ps)",
     xlab = "Taux de lipides (% ps)")
lm2 = lm(benthos_contam$sommeHBCDD_ng_gdw ~benthos_contam$lip_PS_percent)
summary(lm2)
if(summary(lm2)$coef[2,4]<0.05){
  abline(lm2)
}
