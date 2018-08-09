# Description: 
# Author: Alice Ziegler
# Date: 2018-03-07 12:08:34
# to do: 
rm(list=ls())
#############
########################################################################################
###Presettings
########################################################################################
#Packages: 
library(ggplot2)
library(RColorBrewer)

#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "jul18_50m/2018-08-02_ffs_pls_cv_onlyForest_noslpasp/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}

########################################################################################
###actual plotting
########################################################################################
trophic_tbl <- get(load(paste0(inpath_general, "trophic_tbl.RData")))
stats <- get(load(paste0(inpath, "stats.RData")))
#colnames(stats)[which(colnames(stats) == "Rsquared")] <- "R2"
R2_df <- unique(data.frame(stats$resp, stats$meanR2))
R2_df <- R2_df[order(R2_df$stats.meanR2, decreasing = T),]
#write.csv(R2_df, file = paste0(outpath, "R2_df.csv"))
###rename levels for naming of xlab-ticks ############ACHTUNG RÂ² in spaltenÃ¼berschrift o_O############LÃ¶sung finden: gibt es eine andere LÃ¶sung fÃ¼r ggpot2??
# levels(stats$resp) <- c( "all plants (resid)", "all animals (resid)", "ants (resid)", 
#                                "asterids (resid)", "bats (resid)", "bees (resid)", 
#                                "birds (resid)", "collembola (resid)", "conifers (resid)", 
#                                "dungbeetles (resid)", "eudicots (resid)", "ferns (resid)", 
#                                "lycopodiopsida (resid)", "magnoliids (resid)", 
#                                "mammals (resid)", "millipedes (resid)", "monocots (resid)", 
#                                "moths (resid)", "orthoptera (resid)", 
#                                "other aculeata (resid)", "other coleoptera (resid)", 
#                                "parasitoids (resid)", "rosids (resid)", "snails (resid)", 
#                                "spiders (resid)", "syrphids (resid)", "all plants", 
#                                "all animals", "ants", "asterids", "bats", "bees", 
#                                "birds", "collembola", "conifers", "dungbeetles", 
#                                "eudicots", "ferns", "lycopodiopsida", 
#                                "magnoliids", "mammals", "millipedes", "monocots", 
#                                "moths", "orthoptera", "other aculeata", 
#                                "other coleoptera", "parasitoids", "rosids", 
#                                "snails", "spiders", "syrphids")


###########################################
###trophic levels for every row
###########################################

for (x in seq(nrow(stats))){
  trop <- NA
  for (i in trophic_tbl$Taxon){
    match <- grep(i, stats[x,"resp"], value=TRUE)
    if (length(match) != 0){
      trop <- trophic_tbl$diet[trophic_tbl$Taxon == i]
    }
    #print(trop)
  }
  stats$troph[x] <- as.character(trop)
}
stats$troph <- factor(stats$troph, levels = c("generalist", 
                                              "predator", 
                                              "herbivore", 
                                              "decomposer", 
                                              "plant", 
                                              "birds", 
                                              "bats", 
                                              "summary", 
                                              "trait"))
stats <- stats[with(stats, order(troph, resp)),] ####sortierung nach alphabet resp ist nicht sooo optimal, weil resids lfter zusammenstehen und nicht abwechselnd reisid und das entsprechende SR- eventuell "resid" hinten an namen dranschreiben
stats$resp <- factor(stats$resp, levels = unique(stats$resp))

saveRDS(stats, file = paste0(outpath, "stats_troph.RDS"))

###colour settings
df_col <- data.frame(troph = levels(stats$troph), col = c("yellow2", "red2", "darkgreen", "sienna2", "green2", "royalblue2", "grey30", "grey100", "purple2"))
#myColors <- brewer.pal(9,"Set1")
myColors <- c("yellow2", "red2", "darkgreen", "sienna2", "green2", "royalblue2", "grey30", "grey100", "purple2")
names(myColors) <- levels(stats$troph)
legend_order <- levels(stats$troph)
fillscale_std <- scale_fill_manual(name = "troph_col",values = myColors, breaks = legend_order)


plot_trop <- function(df, var, names, resp_title, smmry = "resp", path = outpath, comm, 
                      fillscale = fillscale_std){
  #df$resp = reorder(df$resp, df[[var]], median)
  p <- ggplot(aes_string(x = smmry, y = var, fill = "troph_col"), data = df) +
    geom_boxplot(aes(fill = troph), lwd = 0.3) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
    xlab(resp_title) +
    ylab(var)+
    guides(fill=guide_legend(title="trophic level"))+
    fillscale
  print(p)
}

# #testing
# alpha_nm <- unique(stats$resp[-grep("NMDS", stats$resp)])
# alpha_nm_SR <- alpha_nm[-grep("resid", alpha_nm)]
# tmp_stats <- stats[which(stats$resp %in% alpha_nm_SR),]
# plot_trop(df = tmp_stats, var = "Rsquared", resp_title = "alpha_test")


trait_nm <- c("abundance", "body_mass", "richness", unique(as.character(stats$resp[grep("index", stats$resp)])))
alpha_nm <- unique(stats$resp[-grep("NMDS", stats$resp)])
alpha_nm <- alpha_nm[-which(alpha_nm %in% trait_nm)]
beta_nm <- unique(stats$resp[grepl("NMDS", stats$resp)])

###divide alpha and beta by residuals and SR
alpha_nm_SR <- alpha_nm[-grep("resid", alpha_nm)]
alpha_nm_resid <- alpha_nm[grepl("resid", alpha_nm)]

beta_nm_SR <- beta_nm[-grep("resid", beta_nm)]
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
#variations <- list (trait = trait_nm)
variations <- list(trait = trait_nm, alpha_SR = alpha_nm_SR, alpha_resid = alpha_nm_resid, 
                   beta_SR_jac1 = beta_nm_SR_jac1, beta_SR_jtu1 = beta_nm_SR_jtu1, 
                   beta_SR_jne1 = beta_nm_SR_jne1, beta_resid_jac1 = beta_nm_resid_jac1, 
                   beta_resid_jtu1 = beta_nm_resid_jtu1, beta_resid_jne1 = beta_nm_resid_jne1, 
                   beta_SR_jac2 = beta_nm_SR_jac2, beta_SR_jtu2 = beta_nm_SR_jtu2, 
                   beta_SR_jne2 = beta_nm_SR_jne2, beta_resid_jac2 = beta_nm_resid_jac2, 
                   beta_resid_jtu2 = beta_nm_resid_jtu2, beta_resid_jne2 = beta_nm_resid_jne2)

plots <- c("Rsquared", "RMSE", "RMSE_norm_by_sd", "RMSE_norm_by_mean")



for (j in plots){
  pdf(file = paste0(outpath, "boxplot_", j, ".pdf"), width = 12, height = 12)
  for (i in seq(variations)){
    tmp_stats <- stats[which(stats$resp %in% variations[[i]]),]
    plot_trop(df = tmp_stats, var = j, resp_title = names(variations[i]))
  }
  dev.off()
}

##mean plots
df_mean <- aggregate(stats, by = list(stats$resp), FUN = mean)
df_mean <- cbind(df_mean$Group.1, df_mean[,3:ncol(df_mean)])
colnames(df_mean)[1] <- "resp"
#df_mean <- aggregate(Rsquared~resp+troph, stats, FUN = mean)
# troph_df <- data.frame(stats$resp, stats$troph)
# colnames(troph_df) <- c("resp", "troph")

for (x in seq(nrow(df_mean))){
  trop <- NA
  for (i in trophic_tbl$Taxon){
    match <- grep(i, df_mean[x,"resp"], value=TRUE)
    if (length(match) != 0){
      trop <- trophic_tbl$diet[trophic_tbl$Taxon == i]
    }
  }
  df_mean$troph[x] <- as.character(trop)
}
df_mean$troph <- factor(df_mean$troph, levels = c("generalist", 
                                              "predator", 
                                              "herbivore", 
                                              "decomposer", 
                                              "plant", 
                                              "birds", 
                                              "bats", 
                                              "summary", 
                                              "trait"))
##############################################################
# bymedian <- with(df_mean, reorder(troph, -Rsquared, median))
# boxplot(troph ~ bymedian, data = df_mean)
# 
# df_R2 <- df_mean[-which(is.na(df_mean$Rsquared)),c("resp", "Rsquared", "troph")]
# 
# bymed <- with(df_R2, reorder(troph, -Rsquared, median))
# #boxplot(troph ~ bymed, data = df_R2)
# 
# 
# df_R2$troph <- factor(df_R2$troph, levels = levels(bymed))
# ggplot(df_R2, aes(troph,Rsquared))+
#   geom_boxplot(aes(fill = troph), lwd = 0.3)

# 
# # df_R2 <- df_R2[,c(1: (ncol(df_R2)-1))]
# # colnames(df_R2) <- c("resp", "Rsquared", "troph", "meanR2")
# 
# R2_aggregate <- aggregate(df_R2, by = list(df_R2$troph), FUN = median)
# R2_sort <- R2_aggregate[with(R2_aggregate, order(R2_aggregate$Rsquared, decreasing = T)),]
# R2_df <- df_mean
# R2_df$troph[] <- lapply(R2_df$troph, as.character)
# 

#plot mean by troph sorted by median
# variations <- c("Rsquared", "RMSE_norm_by_mean", "RMSE_norm_by_sd")
# for(i in variations){
#   df_tmp <- df_mean[-which(is.na(df_mean[,which(colnames(df_mean) == i)])),c("resp", i, "troph")]
#   #assign("x", 5)
#   bymedian <- with(df_tmp, reorder(troph, -Rsquared, median)) #####################################################hier müsste i rein, aber als variable, nicht als character! 
# }


pdf(file = paste0(outpath, "boxplot_Rsquared_mean.pdf"), width = 12, height = 12)
  for (i in seq(variations)){
    df_mean_tmp <- df_mean[which(df_mean$resp %in% variations[[i]]),]
    df_R2 <- df_mean_tmp[,c("resp", "Rsquared", "troph")]
    # df_R2 <- df_mean_tmp[-which(is.na(df_mean_tmp$Rsquared)),c("resp", "Rsquared", "troph")]
    bymed_R2 <- with(df_R2, reorder(troph, -Rsquared, median, na.rm = T))
    df_R2$troph <- factor(df_R2$troph, levels = levels(bymed_R2))
    plot_trop(df = df_R2, smmry = "troph", var = "Rsquared", 
              resp_title = paste0("mean_Rsquared_", names(variations)[i]))
  }
dev.off()




###alle in einen plot packen, aber trotzdem nur zb predator_jac1 werden für eine Box betrachtet. 
###########################################################################################2do: farben sind falsch zugeordnet aber passen in gruppen, legende ist zu lang..sollte nur trophische level haben
df_mean_all <- data.frame()
for (i in seq(variations)){
  df_mean_tmp <- df_mean[which(df_mean$resp %in% variations[[i]]),]
  df_R2 <- df_mean_tmp[,c("resp", "Rsquared", "troph")]
  df_R2$troph_unq <- paste0(df_R2$troph, "_", names(variations)[i])
  df_mean_all <- rbind(df_mean_all, df_R2)}

# df_R2 <- df_mean_tmp[-which(is.na(df_mean_tmp$Rsquared)),c("resp", "Rsquared", "troph")]
bymed_R2_all <- with(df_mean_all, reorder(troph_unq, -Rsquared, median, na.rm = T)) ####wie kann es da überhaupt NA geben? Gibt es bei Pflanzen!
df_mean_all$troph_unq <- factor(df_mean_all$troph_unq, levels = levels(bymed_R2_all))

#df_mean_col_all <- merge(df_mean_all, df_col, by = "troph")
troph_unq_col_rw <- merge(df_mean_all, df_col, by = "troph")
troph_unq_col <- unique(troph_unq_col_rw[,c("troph", "troph_unq", "col")])

troph_unq_col_srt <- with(troph_unq_col, troph_unq_col[order(troph_unq),])

myColors_all <- troph_unq_col_srt$col
names(myColors_all) <- levels(troph_unq_col$troph_unq)
legend_order_all <- levels(df_mean_all$troph_unq)
fillscale_all <- scale_fill_manual(name = "troph_col_all",values = myColors_all, breaks = legend_order_all)

###########################################################################################2do: farben sind falsch zugeordnet aber passen in gruppen, legende ist zu lang..sollte nur trophische level haben
plt <- ggplot(aes_string(x = "troph_unq", y = "Rsquared", fill = "troph_col_all"), data = df_mean_all) +
  geom_boxplot(aes(fill = troph_unq), lwd = 0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  xlab(paste0("mean_Rsquared_", names(variations)[i])) +
  ylab("Rsquared")+
  #guides(fill=guide_legend(title="trophic level"))+
  guides(fill=F)+
  fillscale_all
pdf(file = paste0(outpath, "boxplot_Rsquared_mean_all.pdf"), width = 12, height = 12)
print(plt)
dev.off()
