# Description: new ldr_query function...now yet implemented in package inkili
# Author: Alice Ziegler
# Date: 2017-10-04 15:37:09

rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 
library(pls)
#Sources: 
inpath <- "E:/Projekte/Kili/data/"
outpath <- "E:/Projekte/Kili/data/"

########################################################################################
###Settings
########################################################################################
load(paste0(inpath,"ldr_all_points_10m.RData"))
# load(paste0(inpath, "ldr_SR.RData"))

########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################

str_var_plt <- lapply(unique(as.character(pnts$plotID)), function(i){
  pnts_tmp <- pnts[which(pnts$plotID == i),]
  qntl <- quantile(pnts_tmp$h_rel, probs=seq(0,1,0.25))
  qntl_0 <- qntl[[1]]
  qntl_25 <- qntl[[2]]
  qntl_50 <- qntl[[3]]
  qntl_75 <- qntl[[4]]
  qntl_100 <- qntl[[5]]
  qntl_rng <- qntl_75 - qntl_25
  
  ###########################################################################2Do an anderer Stelle entfernen???
  # #remove outliers  ##########################################################doppelt ganze Tabelle überschreiben ist ungeschickt
  # out_rng <- 1.5 * qntl_rng
  # #y <- pnts_tmp$h_rel
  # pnts_rdc <- pnts_tmp[pnts_tmp$h_rel > (qntl_25 - out_rng),]
  # pnts_rdc <- pnts_rdc[pnts_rdc$h_rel < (qntl_75 + out_rng),]
  # outs <- nrow(pnts_tmp)-nrow(pnts_rdc)
  # nmbr_pnts_org <- nrow(pnts_tmp)
  # perc_outs <- round(outs/nmbr_pnts_org, digits = 2)
  # pntsc_tmp <- pnts_rdc
  # rm(pnts_rdc)
  
###structural measures
  #calculate maximal height
  max_hght <- max(pnts_tmp$z-pnts_tmp$h_min)
  #calculate standard deviation
  sd_hght <- sd(pnts_tmp$z)
  #calculate median of total number of returns for each coordinate
  mdn_rtrn <- median(pnts_tmp$returns)
  

  #calculate density coefficients (questionable!!! should be differentiated between landuseforms)
  # dnst_vals <- density(pnts_tmp$h_rel)
  # dnst <- data.frame(cbind(x=dnst_vals$x, y=dnst_vals$y))
  # d = dnst[dnst$x > 10 & dnst$y >=0.005, ]
  # plot(d$x, d$y, type = "l")
  # t = plsr(y ~ x^2, data = d)
  # summary(t)
  # str(t)
  # plot(t)
  # 
  # dnst_fun <- glm(formula = y ~ x + I(x^2) + I(x^3) + I(x^4), data = dnst)
  # smmry_dnst <- summary(dnst_fun)
  # cffnt_intcpt <- smmry_dnst$coefficients[1,1]
  # cffnt_x <- smmry_dnst$coefficients[2,1]
  # cffnt_x2 <- smmry_dnst$coefficients[3,1]
  # cffnt_x3 <- smmry_dnst$coefficients[4,1]
  #calculate return characteristics
  max_rtrn <- max(pnts_tmp$returns)
  sd_rtrn_1 <- sd(pnts_tmp$z[which(pnts_tmp$returnNumber == 1)])
  sd_nmbr_rtrn <- sd(pnts_tmp$returns)
  
  ###quality measures
  #calculate maximum scan angle within plot
  layer <- pnts_tmp$layer[1]
  max_angl <- max(abs(pnts_tmp$scanAngleRank))
  av_angl <- mean(abs(pnts_tmp$scanAngleRank))
  nmbr_pnts_org <- nrow(pnts_tmp)
  
  
  return(list(plotID = i, max_hght = max_hght, sd = sd_hght, mdn_rtrn = mdn_rtrn, max_rtrn = max_rtrn, 
              sd_rtrn_1 = sd_rtrn_1, sd_nmbr_rtrn = sd_nmbr_rtrn, qntl_0 = qntl_0, qntl_25 = qntl_25, 
              qntl_50 = qntl_50, qntl_75 = qntl_75, qntl_100 = qntl_100, qntl_rng = qntl_rng, 
              #cffnt_intcpt = cffnt_intcpt, cffnt_x = cffnt_x, cffnt_x2 = cffnt_x2, cffnt_x3 = cffnt_x3, 
              layer = layer, max_angl = max_angl, av_angl = av_angl, nmbr_pnts_org = nmbr_pnts_org))
              #, outs = outs, perc_outs = perc_outs))
})
ldr_str_pnts <- as.data.frame(do.call(rbind, str_var_plt))
for (i in 1:ncol(ldr_str_pnts)) {
  ldr_str_pnts[, i] <- unlist(ldr_str_pnts[, i])
}
#merge with ldr_SR table and therefore structure variables that were directly derived from db
#ldr_SR_str_pnts <- merge(ldr_SR, ldr_str_pnts, by.x = "plotID", by.y = "plotID")
#save(ldr_SR_str_pnts, file = paste0(outpath, "ldr_SR_str_pnts.RData"))

save(ldr_str_pnts, file = paste0(outpath, "ldr_str_pnts.RData"))

########################################################################################
#snipplets_:

