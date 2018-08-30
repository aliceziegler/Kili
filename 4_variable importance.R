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
sub <- "aug18/2018-08-24_ffs_pls_cv_onlyForest_alpha_elev_dstrb/"
# sub <- "jul18_50m/2018-07-16_ffs_pls_cv_noForest/"
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
pred_grp <- readRDS(file = paste0(inpath, "../pred_grp.RDS"))####################woanders hinlegen und woanders hernehmen.
#####plotting properties
clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))

########################################
####alpha, beta, resid, SR names
########################################
#trait_nm <- c("abundance", "body_mass", "richness", unique(as.character(stats$resp[grep("index", stats$resp)])))
#alpha_nm <- unique(stats$resp[-grep("NMDS", stats$resp)])
alpha_nm <- unique(stats$resp)

beta_nm <- unique(stats$resp[grepl("NMDS", stats$resp)])

###divide alpha and beta by residuals and SR
# alpha_nm_SR <- alpha_nm[-grep("resid", alpha_nm)]
alpha_nm_SR <- alpha_nm[!grepl("resid", alpha_nm)&!grepl("sum", alpha_nm)]
alpha_nm_resid <- alpha_nm[grepl("resid", alpha_nm)&!grepl("sum", alpha_nm)]
alpha_SR_sum <- alpha_nm[grepl("sum", alpha_nm)&!grepl("resid", alpha_nm)]
alpha_resid_sum <- alpha_nm[grepl("sum", alpha_nm)&grepl("resid", alpha_nm)]

beta_nm_SR <- beta_nm[!grepl("resid", beta_nm)]
beta_nm_resid <- beta_nm[grepl("resid", beta_nm)]

###divide by different beta measures (jac, jtu, jne) and NMDS1 oder NMDS2
#NMDS1
beta_nm_SR_jac1 <- beta_nm_SR[grepl("jac", beta_nm_SR)&grepl("NMDS1", beta_nm_SR)]
beta_nm_SR_jtu1 <- beta_nm_SR[grepl("jtu", beta_nm_SR)&grepl("NMDS1", beta_nm_SR)]
beta_nm_SR_jne1 <- beta_nm_SR[grepl("jne", beta_nm_SR)&grepl("NMDS1", beta_nm_SR)]

beta_nm_resid_jac1 <- beta_nm_resid[grepl("jac", beta_nm_resid)&grepl("NMDS1", beta_nm_SR)]
beta_nm_resid_jtu1 <- beta_nm_resid[grepl("jtu", beta_nm_resid)&grepl("NMDS1", beta_nm_SR)]
beta_nm_resid_jne1 <- beta_nm_resid[grepl("jne", beta_nm_resid)&grepl("NMDS1", beta_nm_SR)]

#NMDS2
beta_nm_SR_jac2 <- beta_nm_SR[grepl("jac", beta_nm_SR)&grepl("NMDS2", beta_nm_SR)]
beta_nm_SR_jtu2 <- beta_nm_SR[grepl("jtu", beta_nm_SR)&grepl("NMDS2", beta_nm_SR)]
beta_nm_SR_jne2 <- beta_nm_SR[grepl("jne", beta_nm_SR)&grepl("NMDS2", beta_nm_SR)]

beta_nm_resid_jac2 <- beta_nm_resid[grepl("jac", beta_nm_resid)&grepl("NMDS2", beta_nm_SR)]
beta_nm_resid_jtu2 <- beta_nm_resid[grepl("jtu", beta_nm_resid)&grepl("NMDS2", beta_nm_SR)]
beta_nm_resid_jne2 <- beta_nm_resid[grepl("jne", beta_nm_resid)&grepl("NMDS2", beta_nm_SR)]

###responses you wish the plots for 
#variations <- list (trait = trait_nm, ...)
variations <- list(alpha_SR = alpha_nm_SR, alpha_resid = alpha_nm_resid, 
                   alpha_SR_sum = alpha_SR_sum, alpha_resid_sum = alpha_resid_sum, 
                   beta_SR_jac1 = beta_nm_SR_jac1, beta_SR_jtu1 = beta_nm_SR_jtu1, 
                   beta_SR_jne1 = beta_nm_SR_jne1, beta_resid_jac1 = beta_nm_resid_jac1, 
                   beta_resid_jtu1 = beta_nm_resid_jtu1, beta_resid_jne1 = beta_nm_resid_jne1, 
                   beta_SR_jac2 = beta_nm_SR_jac2, beta_SR_jtu2 = beta_nm_SR_jtu2, 
                   beta_SR_jne2 = beta_nm_SR_jne2, beta_resid_jac2 = beta_nm_resid_jac2, 
                   beta_resid_jtu2 = beta_nm_resid_jtu2, beta_resid_jne2 = beta_nm_resid_jne2)

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

#saveRDS(varimp_df, file = paste0(outpath, "varimp_df.rds"))

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
lvlplt <- function(mat, name, font_sz = 0.35, #filename, 
                   wdth = 10, hght = 7, lbl_x, lbl_y, rnge = seq(0,1,0.1), main = ""){
  xdim <- dim(mat)[2]
  ydim <- dim(mat)[1]
  rst <- raster(mat, xmn = 0.5, xmx = (xdim+0.5), ymn = 0.5, ymx = ydim+0.5)
  # pdf(file = filename, width = wdth, height = hght)
  print(levelplot(rst, scales = list(x = list(rot=90, cex = font_sz, at = 1:xdim, labels = lbl_x), 
                               y = list(at = c(ydim:1), cex = font_sz, labels = lbl_y)), 
            margin = FALSE, 
            #main = list(name,side=1,line=0.5), 
            main = main, 
            col.regions = clr(101), 
            at = rnge))
  # dev.off()
}


#for loops über "variations"

###standard plot
pdf(file = paste0(outpath, "heat_std.pdf"), width = 10, height = 7)
for (i in seq(variations)){
  mat <- varimp_mat[,which(colnames(varimp_mat) %in% variations[[i]]), drop = FALSE]
  lvlplt(mat = mat, lbl_x = colnames(mat), lbl_y = rownames(varimp_troph))
}
dev.off()

###sorted standard plot: 
pdf(file = paste0(outpath, "heat_std_sort.pdf"), width = 10, height = 7)
for (i in seq(variations)){
  mat <- varimp_mat[,which(colnames(varimp_mat) %in% variations[[i]]), drop = FALSE]
  rownames(mat) <- rownames(varimp_troph)
  mat_rsort <- mat[order(rowMeans(mat), decreasing = T), ]
  Rsq_tbl <- unique(stats[which(stats$resp %in% c(as.character(variations[[i]]))),c("resp", "meanR2")])
  Rsq_srt <- Rsq_tbl[order(Rsq_tbl$meanR2, decreasing = T), ]
  mat_srt <- as.matrix(as.data.frame(mat_rsort)[c(as.character(Rsq_srt$resp))])
  lvlplt(mat = mat_srt, lbl_x = colnames(mat_srt), lbl_y = rownames(mat_srt))
}
dev.off()

###normalised (R2)
varimp_nrm_df <- varimp_df
for (i in c(2:ncol(varimp_df))){
  varimp_nrm_df[,i] <- varimp_df[,i]*stats[which(stats$resp == names(varimp_df[i]))[1],"meanR2"]
}
varimp_tmp_lst <- as.list(varimp_nrm_df[,-which(colnames(varimp_nrm_df) == "pred")])
varimp_mat <- do.call("cbind", varimp_tmp_lst[])

pdf(file = paste0(outpath, "heat_nrmR2.pdf"), width = 10, height = 7)
for (i in seq(variations)){
  mat <- varimp_mat[,which(colnames(varimp_mat) %in% variations[[i]]), drop = FALSE]
  lvlplt(mat = mat, lbl_x = colnames(mat), lbl_y = rownames(varimp_troph), rnge = seq(0, 0.1, 0.001))
}
dev.off()
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


###auskommentiert weil aggregiert auf einzelvorhersagen und jetzt ist gesamtvorhersage auf trophisches level mit drin
# ###group by responses trophic level
# varimp_troph <- merge(varimp_t, troph_grp, by = "resp") #############################################ungünstig, weil variablenname vorher schon vergeben ist...absicht?
# pdf(file = paste0(outpath, "heat_varimp_grp_troph.pdf"), width = 10, height = 7)
# for (i in seq(variations)){
#   varimp_troph_tmp <- varimp_troph[which(varimp_troph$resp %in% variations[[i]]),]
#   agg_troph <- aggregate(varimp_troph_tmp, by = list(varimp_troph_tmp$troph), FUN = mean, na.rm = T) #######was ist hier los....14 NAs...fehlen da werte=? (bei noForest)
#   agg_troph <- agg_troph[,-c(2, ncol(agg_troph))]
#   colnames(agg_troph)[1] <- "troph_grp"
#   
#   agg_troph_lst <- as.list(agg_troph[, -which(colnames(agg_troph) == "troph_grp")])
#   agg_troph_mat <- do.call("cbind", agg_troph_lst[])
#   lvlplt(mat = agg_troph_mat, lbl_x = colnames(agg_troph_mat), lbl_y = agg_troph$troph_grp, 
#          rnge = seq(0,0.1,0.001), main = paste0(names(variations)[i]))
# }
# dev.off()
# 


###group by predictors (grp) and then by predictors and trophic levels

varimp_pred_grp <- merge(varimp_df, pred_grp, by = "pred")
pdf(file = paste0(outpath, "heat_varimp_grp_pred.pdf"), width = 10, height = 7)
for (i in seq(variations)){############################################################################diese zusammenlegung der beiden gruppierungen sollte auf dauer aufgelöst werden!
  if(length(variations[[i]] != 0)){
    varimp_pred_grp_tmp <- varimp_pred_grp[,which(colnames(varimp_pred_grp) %in% c(as.character(variations[[i]]), "pred", "grp")), drop = F]
    agg_pred <- aggregate(varimp_pred_grp_tmp, by = list(varimp_pred_grp_tmp$grp), FUN = mean, na.rm = T)########woher kommen die ganzen NAs
    agg_pred <- agg_pred[,-c(2, ncol(agg_pred))]
    colnames(agg_pred)[1] <- "pred_grp"

    agg_pred_lst <- as.list(agg_pred[, -which(colnames(agg_pred) == "pred_grp")])
    agg_pred_mat <- do.call("cbind", agg_pred_lst[])
    lvlplt(mat = agg_pred_mat, main = names(variations)[i], font_sz = 0.5, lbl_x = colnames(agg_pred_mat), lbl_y = agg_pred$pred_grp)


    ##auskommentiert, weil aggregiert von einzelvorhersagen und nicht vorhersage auf aggregiertes!
  
    # ###group by trophic levels and predictors
    # agg_tbl <- agg_pred
    # row.names(agg_tbl) <- agg_tbl$pred_grp
    # agg_all_df_t <- data.frame(t(agg_pred))
    # colnames(agg_all_df_t) <- rownames(agg_tbl)
    # agg_all_df_t <- agg_all_df_t[-1,]
    # agg_all_df_t$resp <- rownames(agg_all_df_t)
    # agg_all_df_mrg <- merge(agg_all_df_t, troph_grp, by = "resp")
    # agg_all_df_mrg_rdc <- agg_all_df_mrg[,-1]
    # for (k in seq(ncol(agg_all_df_mrg_rdc)-1)){
    #   agg_all_df_mrg_rdc[,k] <- as.numeric(agg_all_df_mrg_rdc[,k])
    # }
    # agg_all_df_mrg_rdc$troph <- as.character(agg_all_df_mrg_rdc$troph)
    # agg_all_df <- aggregate(agg_all_df_mrg_rdc, by = list(agg_all_df_mrg_rdc$troph), FUN = mean, na.rm = T)############woher kommen viele NAS
    # agg_all_df <- agg_all_df[,-which(colnames(agg_all_df) == "troph")]
    # colnames(agg_all_df)[1] <- "troph"
    # agg_vi_lst <- as.list(agg_all_df[, -which(colnames(agg_all_df) == "troph")])
    # agg_vi_mat <- do.call("cbind", agg_vi_lst[])
    # agg_vi_mat <- agg_vi_mat/max(agg_vi_mat, na.rm = T)
    # agg_vi_mat_t <- t(agg_vi_mat)
    # lvlplt(mat = agg_vi_mat_t, main = names(variations)[i], font_sz = 0.5, lbl_x = as.character(agg_all_df$troph), lbl_y = colnames(agg_vi_mat), rnge = seq(0, 1, 0.1))
    # 
  }
 }
dev.off()


###sorted group by predictors
rownames(mat) <- rownames(varimp_troph)
mat_rsort <- mat[order(rowMeans(mat), decreasing = T), ]
Rsq_tbl <- unique(stats[which(stats$resp %in% c(as.character(variations[[i]]))),c("resp", "meanR2")])
Rsq_srt <- Rsq_tbl[order(Rsq_tbl$meanR2, decreasing = T), ]
mat_srt <- as.matrix(as.data.frame(mat_rsort)[c(as.character(Rsq_srt$resp))])
lvlplt(mat = mat_srt, lbl_x = colnames(mat_srt), lbl_y = rownames(mat_srt))

varimp_pred_grp <- merge(varimp_df, pred_grp, by = "pred")
pdf(file = paste0(outpath, "heat_varimp_grp_pred_sort.pdf"), width = 10, height = 7)
for (i in seq(variations)){############################################################################diese zusammenlegung der beiden gruppierungen sollte auf dauer aufgelöst werden!
  if(length(variations[[i]] != 0)){
    varimp_pred_grp_tmp <- varimp_pred_grp[,which(colnames(varimp_pred_grp) %in% c(as.character(variations[[i]]), "pred", "grp")), drop = F]
    agg_pred <- aggregate(varimp_pred_grp_tmp, by = list(varimp_pred_grp_tmp$grp), FUN = mean, na.rm = T)########woher kommen die ganzen NAs
    agg_pred <- agg_pred[,-c(2, ncol(agg_pred))]
    colnames(agg_pred)[1] <- "pred_grp"
    
    agg_pred_lst <- as.list(agg_pred[, -which(colnames(agg_pred) == "pred_grp")])
    agg_pred_mat <- do.call("cbind", agg_pred_lst[])
    rownames(agg_pred_mat) <- agg_pred$pred_grp
    agg_pred_mat_rsrt <- agg_pred_mat[order(rowMeans(agg_pred_mat), decreasing = T), ]
    Rsq_tbl <- unique(stats[which(stats$resp %in% c(as.character(variations[[i]]))),c("resp", "meanR2")])
    Rsq_srt <- Rsq_tbl[order(Rsq_tbl$meanR2, decreasing = T), ]
    agg_pred_mat_srt <- as.matrix(as.data.frame(agg_pred_mat_rsrt)[c(as.character(Rsq_srt$resp))])
    lvlplt(mat = agg_pred_mat_srt, main = names(variations)[i], font_sz = 0.5, lbl_x = colnames(agg_pred_mat_srt), lbl_y = rownames(agg_pred_mat_srt))
  }
}
dev.off()
