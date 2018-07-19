# Description: 
# Author: Alice Ziegler
# Date: 2018-03-07 12:08:34
# to do: ###optimieren: mod_all muss vermieden werden. jeder loop muss Ã¼ber die einzelmodelle laufen!
#########werden hier wirklich nur die resp verwendet, die im ffs rauskommen???
rm(list=ls())
########################################################################################
###Presettings
########################################################################################
#Packages: 
library(raster)
library(RColorBrewer)
library(reshape2)
library(lattice)
library(rasterVis)
library(grid)
library(compositions)
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "jul18_50m/2018-07-16_ffs_pls_cv_noForest/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}

###############################################
varimp_lst <- readRDS(paste0(inpath, "varimp_lst.rds"))
stats <- readRDS(paste0(outpath, "stats_troph.RDS"))
troph_grp <- unique(data.frame(stats$resp, stats$troph))
colnames(troph_grp) <- c("resp", "troph")
pred_grp <- readRDS(file = paste0(inpath, "pred_grp.RDS"))####################woanders hinlegen und woanders hernehmen.

#####plotting properties
clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))

########################################
####alpha, beta, resid, SR names
########################################
trait_nm <- c("abundance", "body_mass", "richness", names(varimp_lst)[grep("index", names(varimp_lst))])
alpha_nm <- names(varimp_lst)[-grep("NMDS", names(varimp_lst))]
alpha_nm <- alpha_nm[-which(alpha_nm %in% trait_nm)]
beta_nm <- names(varimp_lst)[grepl("NMDS", names(varimp_lst))]

###divide alpha and beta by residuals and SR
alpha_nm_SR <- alpha_nm[-grep("resid", alpha_nm)]
alpha_nm_resid <- alpha_nm[grepl("resid", alpha_nm)]

beta_nm_SR <- beta_nm[-grep("resid", beta_nm)]
beta_nm_resid <- beta_nm[grepl("resid", beta_nm)]

###divide by different beta measures (jac, jtu, jne) and NMDS1 oder NMDS2
#NMDS1
beta_nm_SR_jac1 <- beta_nm_SR[-grep("jac", beta_nm_SR)&grepl("NMDS1", beta_nm_SR)]
beta_nm_SR_jtu1 <- beta_nm_SR[-grep("jtu", beta_nm_SR)&grepl("NMDS1", beta_nm_SR)]
beta_nm_SR_jne1 <- beta_nm_SR[-grep("jne", beta_nm_SR)&grepl("NMDS1", beta_nm_SR)]

beta_nm_resid_jac1 <- beta_nm_resid[-grep("jac", beta_nm_resid)&grepl("NMDS1", beta_nm_SR)]
beta_nm_resid_jtu1 <- beta_nm_resid[-grep("jtu", beta_nm_resid)&grepl("NMDS1", beta_nm_SR)]
beta_nm_resid_jne1 <- beta_nm_resid[-grep("jne", beta_nm_resid)&grepl("NMDS1", beta_nm_SR)]

#NMDS2
beta_nm_SR_jac2 <- beta_nm_SR[-grep("jac", beta_nm_SR)&grepl("NMDS2", beta_nm_SR)]
beta_nm_SR_jtu2 <- beta_nm_SR[-grep("jtu", beta_nm_SR)&grepl("NMDS2", beta_nm_SR)]
beta_nm_SR_jne2 <- beta_nm_SR[-grep("jne", beta_nm_SR)&grepl("NMDS2", beta_nm_SR)]

beta_nm_resid_jac2 <- beta_nm_resid[-grep("jac", beta_nm_resid)&grepl("NMDS2", beta_nm_SR)]
beta_nm_resid_jtu2 <- beta_nm_resid[-grep("jtu", beta_nm_resid)&grepl("NMDS2", beta_nm_SR)]
beta_nm_resid_jne2 <- beta_nm_resid[-grep("jne", beta_nm_resid)&grepl("NMDS2", beta_nm_SR)]

# variations <- c("trait_nm", "alpha_nm_SR", "alpha_nm_resid", 
#                 "beta_nm_SR_jac1", "beta_nm_SR_jtu1", "beta_nm_SR_jne1", 
#                 "beta_nm_resid_jac1", "beta_nm_resid_jtu1", "beta_nm_resid_jne1", 
#                 "beta_nm_SR_jac2", "beta_nm_SR_jtu2", "beta_nm_SR_jne2", 
#                 "beta_nm_resid_jac2", "beta_nm_resid_jtu2", "beta_nm_resid_jne2")

variations <- list(trait_nm = trait_nm, alpha_nm_SR = alpha_nm_SR, alpha_nm_resid = alpha_nm_resid, 
                   beta_nm_SR_jac1 = beta_nm_SR_jac1, beta_nm_SR_jtu1 = beta_nm_SR_jtu1, beta_nm_SR_jne1 = beta_nm_SR_jne1, 
                   beta_nm_resid_jac1 = beta_nm_resid_jac1, beta_nm_resid_jtu1 = beta_nm_resid_jtu1, beta_nm_resid_jne1 = beta_nm_resid_jne1, 
                   beta_nm_SR_jac2 = beta_nm_SR_jac2, beta_nm_SR_jtu2 = beta_nm_SR_jtu2, beta_nm_SR_jne2 = beta_nm_SR_jne2, 
                   beta_nm_resid_jac2 = beta_nm_resid_jac2, beta_nm_resid_jtu2 = beta_nm_resid_jtu2, beta_nm_resid_jne2 = beta_nm_resid_jne2)
###############################################
##plot_df creation
###############################################
for (i in seq(length(varimp_lst))){
  colnames(varimp_lst[[i]]) <- names(varimp_lst[i])
  varimp_lst[[i]]$pred <- rownames(varimp_lst[[i]])
}
#create crosstable with responses as colnames and predictors as rows
varimp_df <- Reduce(function(x, y) {merge(x, y, by = "pred", all=T)}, varimp_lst)
varimp_df[is.na(varimp_df)] <- 0
#saveRDS(varimp_df, file = paste0(outpath, "varimp_df.RDS"))
varimp_df[2:ncol(varimp_df)] <- varimp_df[,2:ncol(varimp_df)]/100

#sort varimp_df nach trophischen levels
#transpose um nach Spalten zu sortieren. nach levels sortieren, wenn die in verschiedenen spalten stehen geht nicht/
#weiß ich nciht wie/ist extrem umständlich
varimp_df_t <- data.frame(t(varimp_df))
colnames(varimp_df_t) <- as.character(unlist(varimp_df_t[1,]))
varimp_df_t$resp <- rownames(varimp_df_t)
varimp_troph_t <- merge(varimp_df_t[-1,], troph_grp, by = "resp")
varimp_troph_srt <- varimp_troph_t[with(varimp_troph_t, order(troph, resp)),]
#transpose wieder zurück
rownames(varimp_troph_srt) <- varimp_troph_srt[,1]
varimp_troph_meta <- t(varimp_troph_srt)
varimp_troph_meta_pred <- data.frame(pred = rownames(varimp_troph_meta), varimp_troph_meta)
varimp_troph <- varimp_troph_meta_pred[c(2:(nrow(varimp_troph_meta_pred)-1)),
                                       -which(colnames(varimp_troph_meta_pred) == "pred")]
for (i in seq(ncol(varimp_troph))){
  varimp_troph[,i] <- as.numeric(levels(varimp_troph[,i]))[varimp_troph[,i]]
}
# test <- as.data.frame(sapply(varimp_troph, as.character ))
# test2 <- as.data.frame(sapply(test, as.numeric))

varimp_tmp_lst <- as.list(varimp_troph)
varimp_mat <- do.call("cbind", varimp_tmp_lst[])
######################
######################
##Einschub
######################
######################
###var imp
# tmp <- varimp_df[as.character(troph_grp$resp)]
# varimp_df <- data.frame(pred = varimp_df$pred, tmp)
#count number of times variable was included in model
# var_cnt_c <- (ncol(varimp_df) -1)- rowSums(varimp_df == 0)
# var_cnt <- data.frame(pred = varimp_df$pred, count = var_cnt_c)
# var_cnt <- var_cnt[with(var_cnt, order(count, decreasing = T)),]
# varimp_df <- varimp_df[match(as.character(var_cnt$pred), varimp_df$pred),]
#
#
#
#
### funktion rasterplot
lvlplt <- function(mat, filename, wdth = 10, hght = 7, lbl_x, lbl_y, rnge = seq(0,1,0.1)){
  xdim <- dim(mat)[2]
  ydim <- dim(mat)[1]
  rst <- raster(mat, xmn = 0.5, xmx = (xdim+0.5), ymn = 0.5, ymx = ydim+0.5)
  pdf(file = filename, width = wdth, height = hght)
  print(levelplot(rst, scales = list(x = list(rot=90, cex = 0.35, at = 1:xdim, labels = lbl_x), 
                               y = list(at = c(ydim:1), cex = 0.35, labels = lbl_y)), 
            margin = FALSE, 
            main = list(filename,side=1,line=0.5), 
            col.regions = clr(101), 
            at = rnge))
  dev.off()
}


#for loops über "variations"

###standard plot
for (i in seq(variations)){
  mat <- varimp_mat[,which(colnames(varimp_mat) %in% variations[[i]]), drop = FALSE]
  lvlplt(mat = mat, filename = paste0(outpath, "heat_std_", names(variations[i]), ".pdf"), 
         lbl_x = colnames(mat), lbl_y = varimp_troph$pred)
}


###normalised (R2)
varimp_nrm_df <- varimp_df
for (i in c(2:ncol(varimp_df))){
  varimp_nrm_df[,i] <- varimp_df[,i]*stats[which(stats$resp == names(varimp_df[i]))[1],"meanR2"]
}
varimp_tmp_lst <- as.list(varimp_nrm_df[,-which(colnames(varimp_nrm_df) == "pred")])
varimp_mat <- do.call("cbind", varimp_tmp_lst[])
for (i in seq(variations)){
  mat <- varimp_mat[,which(colnames(varimp_mat) %in% variations[[i]]), drop = FALSE]
  lvlplt(mat = mat, filename = paste0(outpath, "heat_nrmR2_", names(variations[i]), ".pdf"), 
         lbl_x = colnames(mat), lbl_y = varimp_nrm_df$pred, rnge = seq(0, 0.1, 0.001))
}
####normalisieren andere möglichkeiten


###gruppieren
# an df anfügen Spalte mit entsprechenden Levels
# oder auch ldr Variablen zusammenfügen (nach ähnlichkeit oder layern)
varimp_df_t <- varimp_df
row.names(varimp_df_t) <- varimp_df_t$pred
varimp_df_t <- varimp_df_t[,-which(colnames(varimp_df_t) == "pred")]
varimp_t <- t(varimp_df_t)
varimp_t <- data.frame(varimp_t)
varimp_t$resp <- row.names(varimp_t)



###group by responses trophic level
varimp_troph <- merge(varimp_t, troph_grp, by = "resp")
for (i in seq(variations)){
  varimp_troph_tmp <- varimp_troph[which(varimp_troph$resp %in% variations[[i]]),]
  agg_troph <- aggregate(varimp_troph_tmp, by = list(varimp_troph_tmp$troph), FUN = mean, na.rm = T) #######was ist hier los....14 NAs...fehlen da werte=? (bei noForest)
  agg_troph <- agg_troph[,-c(2, ncol(agg_troph))]
  colnames(agg_troph)[1] <- "troph_grp"
  
  agg_troph_lst <- as.list(agg_troph[, -which(colnames(agg_troph) == "troph_grp")])
  agg_troph_mat <- do.call("cbind", agg_troph_lst[])
  lvlplt(mat = agg_troph_mat, filename = paste0(outpath, "heat_varimp_grp_troph_", names(variations[i]), ".pdf"), 
         lbl_x = colnames(agg_troph_mat), lbl_y = agg_troph$troph_grp, rnge = seq(0,0.1,0.001))
}


###group by predictorsand then by predictors and trophic levels

varimp_pred_grp <- merge(varimp_df, pred_grp, by = "pred")
for (i in seq(variations)){
  varimp_pred_grp_tmp <- varimp_pred_grp[,which(colnames(varimp_pred_grp) %in% c(variations[[i]], "pred", "grp")), drop = F]
  agg_pred <- aggregate(varimp_pred_grp_tmp, by = list(varimp_pred_grp_tmp$grp), FUN = mean, na.rm = T)########woher kommen die ganzen NAs
  agg_pred <- agg_pred[,-c(2, ncol(agg_pred))]
  colnames(agg_pred)[1] <- "pred_grp"
  
  agg_pred_lst <- as.list(agg_pred[, -which(colnames(agg_pred) == "pred_grp")])
  agg_pred_mat <- do.call("cbind", agg_pred_lst[])
  lvlplt(mat = agg_pred_mat, filename = paste0(outpath, "heat_varimp_grp_pred_", names(variations[i]), ".pdf"), 
         lbl_x = colnames(agg_pred_mat), lbl_y = agg_pred$pred_grp)
  
  ###group by trophic levels and predictors
  agg_tbl <- agg_pred
  row.names(agg_tbl) <- agg_tbl$pred_grp
  agg_all_df_t <- data.frame(t(agg_pred))
  colnames(agg_all_df_t) <- rownames(agg_tbl)
  agg_all_df_t <- agg_all_df_t[-1,]
  agg_all_df_t$resp <- rownames(agg_all_df_t)
  agg_all_df_mrg <- merge(agg_all_df_t, troph_grp, by = "resp")
  agg_all_df_mrg_rdc <- agg_all_df_mrg[,-1]
  for (k in seq(ncol(agg_all_df_mrg_rdc)-1)){
    agg_all_df_mrg_rdc[,k] <- as.numeric(agg_all_df_mrg_rdc[,k])
  }
  agg_all_df_mrg_rdc$troph <- as.character(agg_all_df_mrg_rdc$troph)
  agg_all_df <- aggregate(agg_all_df_mrg_rdc, by = list(agg_all_df_mrg_rdc$troph), FUN = mean, na.rm = T)############woher kommen viele NAS
  agg_all_df <- agg_all_df[,-which(colnames(agg_all_df) == "troph")]
  colnames(agg_all_df)[1] <- "troph"
  agg_vi_lst <- as.list(agg_all_df[, -which(colnames(agg_all_df) == "troph")])
  agg_vi_mat <- do.call("cbind", agg_vi_lst[])
  agg_vi_mat <- agg_vi_mat/max(agg_vi_mat, na.rm = T)
  lvlplt(mat = agg_vi_mat, filename = paste0(outpath, "heat_varimp_grp_", names(variations[i]), ".pdf"), 
         lbl_x = colnames(agg_vi_mat), lbl_y = as.character(agg_all_df$troph), rnge = seq(0, 1, 0.1))
}



