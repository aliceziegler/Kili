# Description: 
# Author: Alice Ziegler
# Date: 2018-03-07 12:08:34
# to do: 
rm(list=ls())
##############ACHTUNG: bei plotten hochgestellte zwei in Spaltenübershrift von Dataframe...unschön!
########################################################################################
###Presettings
########################################################################################
#Packages: 
library(ggplot2)
library(dplyr)
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "mai18_50m_resid_nrmlz_newDB_rad/2018-05-30_ffs_pls_ffs_indxINOUT/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}
########################################################################################
###actual plotting
########################################################################################

df_resp_all <- get(load(paste0(inpath, "df_resp_all_for_plotting.RData")))
colnames(df_resp_all)[which(colnames(df_resp_all) == "Rsquared")] <- "R2"

###rename levels for naming of xlab-ticks ############ACHTUNG R² in spaltenüberschrift o_O############Lösung finden: gibt es eine andere Lösung für ggpot2??
# levels(df_resp_all$resp) <- c( "all plants (resid)", "all animals (resid)", "ants (resid)", 
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


##only resid: 
df_plt_resid <- df_resp_all[grep("resid", df_resp_all$resp),]
#only species
df_plt_SR <- df_resp_all[-grep("resid", df_resp_all$resp),]


plot_resp <- function(df, var, names, resp_title, path = outpath, comm){
  df$resp = reorder(df$resp, df[[var]], median)
  p <- ggplot(aes_string(x = "resp", y = var), data = df) + 
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    xlab(resp_title) + 
    ylab(var)
  print(p)
  ggsave(filename = paste0(path, "plot_", var, "_", comm, ".pdf"), plot = p, width = 25,
         height = 25, units = "cm")
  
  dev.off()
}
#ggplot(d, aes(x=reorder(fac, y, mean), y=y))

plot_resp(df = df_plt_resid, var = "RMSE", resp_title = "Taxa (Residuals)", comm = "res")
plot_resp(df = df_plt_resid, var = "R2", resp_title = "Taxa (Residuals)", comm = "res")
plot_resp(df = df_plt_SR, var = "RMSE", resp_title = "Taxa", comm = "")
plot_resp(df = df_plt_SR, var = "R2", resp_title = "Taxa", comm = "")



stats <- as.data.frame(ddply(df_resp_all,~resp,summarise,
                             meanR2 = mean(R2),
                             medianR2 = median(R2),
                             sdR2 = sd(R2), 
                             meanRMSE = mean(RMSE),
                             medianRMSE = median(RMSE),
                             sdRMSE = sd(RMSE)))


save(stats, file = paste0(outpath, "stats.RData"))
write.csv(stats, file = paste0(outpath, "stats.csv"))
     