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

#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "jul18_50m/2018-07-18_ffs_pls_cv_onlyForest/"
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
###rename levels for naming of xlab-ticks ############ACHTUNG R² in spaltenüberschrift o_O############Lösung finden: gibt es eine andere Lösung für ggpot2??
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
toMatch <- trophic_tbl$Taxon
for (x in seq(nrow(stats))){
  trop <- NA
  for (i in toMatch){
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

# ##only resid alpha: 
# df_plt_resid <- stats[grepl("resid", stats$resp)&grepl("SR", stats$resp),]
# #only species alpha:
# df_plt_SR_tmp <- stats[-grep("resid", stats$resp),]
# df_plt_SR <- df_plt_SR_tmp[grep("SR", df_plt_SR_tmp$resp),]
# 
# ###resid
# # only jac1
# df_plt_jac_1_resid <- stats[(grepl("jac", stats$resp)&grepl("NMDS1", stats$resp)&grepl("resid", stats$resp)),]
# # only jac2
# df_plt_jac_2_resid <- stats[(grepl("jac", stats$resp)&grepl("NMDS2", stats$resp)&grepl("resid", stats$resp)),]
# # only jtu1
# df_plt_jtu_1_resid <- stats[(grepl("jtu", stats$resp)&grepl("NMDS1", stats$resp)&grepl("resid", stats$resp)),]
# # only jtu2
# df_plt_jtu_2_resid <- stats[(grepl("jtu", stats$resp)&grepl("NMDS2", stats$resp)&grepl("resid", stats$resp)),]
# # only jne1
# df_plt_jne_1_resid <- stats[(grepl("jne", stats$resp)&grepl("NMDS1", stats$resp)&grepl("resid", stats$resp)),]
# # only jne2
# df_plt_jne_2_resid <- stats[(grepl("jne", stats$resp)&grepl("NMDS2", stats$resp)&grepl("resid", stats$resp)),]
# 
# ##SR
# # only jac1
# df_plt_jac_1_SR <- stats[(grepl("jac", stats$resp)&grepl("NMDS1", stats$resp)&!grepl("resid", stats$resp)),]
# # only jac2
# df_plt_jac_2_SR <- stats[(grepl("jac", stats$resp)&grepl("NMDS2", stats$resp)&!grepl("resid", stats$resp)),]
# # only jtu1
# df_plt_jtu_1_SR <- stats[(grepl("jtu", stats$resp)&grepl("NMDS1", stats$resp)&!grepl("resid", stats$resp)),]
# # only jtu2
# df_plt_jtu_2_SR <- stats[(grepl("jtu", stats$resp)&grepl("NMDS2", stats$resp)&!grepl("resid", stats$resp)),]
# # only jne1
# df_plt_jne_1_SR <- stats[(grepl("jne", stats$resp)&grepl("NMDS1", stats$resp)&!grepl("resid", stats$resp)),]
# # only jne2
# df_plt_jne_2_SR <- stats[(grepl("jne", stats$resp)&grepl("NMDS2", stats$resp)&!grepl("resid", stats$resp)),]
# 


plot_trop <- function(df, var, names, resp_title, path = outpath, comm){
  #df$resp = reorder(df$resp, df[[var]], median)
  p <-ggplot(aes_string(x = "resp", y = var), data = df) +
    geom_boxplot(aes(fill = troph), lwd = 0.3) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5)) +
    xlab(resp_title) +
    ylab(var)+
    guides(fill=guide_legend(title="trophic level"))
  #pdf(file = paste0(outpath, "plot_", var, "_", resp_title, ".pdf"), width = 25, height = 25)
  print(p)
  #ggsave(filename = paste0(outpath, "plot_", var, "_", comm, ".pdf"), plot = p, width = 25,
         #height = 25, units = "cm")
  #dev.off()
}

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

