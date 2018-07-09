# Description: 
# Author: Alice Ziegler
# Date: 2018-03-07 12:08:34
# to do: !!rmse normalisieren auf SD oder mean
rm(list=ls())
##############ACHTUNG: bei plotten hochgestellte zwei in Spaltenüberschrift von Dataframe...unschön!
########################################################################################
###Presettings
########################################################################################
#Packages: 
library(ggplot2)


#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "jun18_50m/60erFRST/2018-06-12_ffs_pls_cv_onlyForest/"
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
R2_df <- unique(data.frame(stats$resp, stats$meanR2))
R2_df <- R2_df[order(R2_df$stats.meanR2, decreasing = T),]
#write.csv(R2_df, file = paste0(outpath, "R2_df.csv"))
#colnames(stats)[which(colnames(stats) == "Rsquared")] <- "R2"

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
                                              "summary", 
                                              "trait"))
stats <- stats[with(stats, order(troph, resp)),] ####sortierung nach alphabet resp ist nicht sooo optimal, weil resids lfter zusammenstehen und nicht abwechselnd reisid und das entsprechende SR- eventuell "resid" hinten an namen dranschreiben
stats$resp <- factor(stats$resp, levels = unique(stats$resp))

saveRDS(stats, file = paste0(outpath, "stats_troph.RDS"))

##only resid: 
df_plt_resid <- stats[grep("resid", stats$resp),]
#only species
df_plt_SR_tmp <- stats[-grep("resid", stats$resp),]
df_plt_SR <- df_plt_SR_tmp[grep("SR", df_plt_SR_tmp$resp),]
# only jac1
df_plt_jac_1 <- stats[(grepl("jac", stats$resp)&grepl("NMDS1", stats$resp)),]
# only jac2
df_plt_jac_2 <- stats[(grepl("jac", stats$resp)&grepl("NMDS2", stats$resp)),]
# only jtu1
df_plt_jtu_1 <- stats[(grepl("jtu", stats$resp)&grepl("NMDS1", stats$resp)),]
# only jtu2
df_plt_jtu_2 <- stats[(grepl("jtu", stats$resp)&grepl("NMDS2", stats$resp)),]
# only jne1
df_plt_jne_1 <- stats[(grepl("jne", stats$resp)&grepl("NMDS1", stats$resp)),]
# only jne2
df_plt_jne_2 <- stats[(grepl("jne", stats$resp)&grepl("NMDS2", stats$resp)),]



plot_trop <- function(df, var, names, resp_title, path = outpath, comm){
  #df$resp = reorder(df$resp, df[[var]], median)
  
  
  p <-ggplot(aes_string(x = "resp", y = var), data = df) +
    geom_boxplot(aes(fill = troph), lwd = 0.3) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5)) +
    xlab(resp_title) +
    ylab(var)+
    guides(fill=guide_legend(title="trophic level"))
  print(p)
  ggsave(filename = paste0(outpath, "plot_", var, "_", comm, ".pdf"), plot = p, width = 25,
         height = 25, units = "cm")
  
  dev.off()
}




plot_trop(df = df_plt_resid, var = "RMSE", resp_title = "Taxa (Residuals)", comm = "res")
plot_trop(df = df_plt_resid, var = "Rsquared", resp_title = "Taxa (Residuals)", comm = "res")
plot_trop(df = df_plt_SR, var = "RMSE", resp_title = "Taxa", comm = "SR")
plot_trop(df = df_plt_SR, var = "Rsquared", resp_title = "Taxa", comm = "SR")

plot_trop(df = df_plt_SR, var = "RMSE_norm_by_sd", resp_title = "Taxa", comm = "SR")
plot_trop(df = df_plt_resid, var = "RMSE_norm_by_sd", resp_title = "Taxa (Residuals)", comm = "res")
plot_trop(df = df_plt_jac_1, var = "RMSE_norm_by_sd", resp_title = "jac1", comm = "jac1")
plot_trop(df = df_plt_jac_2, var = "RMSE_norm_by_sd", resp_title = "jac2", comm = "jac2")
plot_trop(df = df_plt_jtu_1, var = "RMSE_norm_by_sd", resp_title = "jtu1", comm = "jtu1")
plot_trop(df = df_plt_jtu_2, var = "RMSE_norm_by_sd", resp_title = "jtu2", comm = "jtu2")
plot_trop(df = df_plt_jne_1, var = "RMSE_norm_by_sd", resp_title = "jne1", comm = "jne1")
plot_trop(df = df_plt_jne_2, var = "RMSE_norm_by_sd", resp_title = "jne2", comm = "jne2")

plot_trop(df = stats, var = "Rsquared", resp_title = "Taxa_all", comm = "taxa_all")
##norm by mean
#jac einzeln raus
#jtu
#jne

     