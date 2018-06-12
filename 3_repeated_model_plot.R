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
library(plyr)
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
trophic_tbl <- get(load(paste0(inpath, "../trophic_tbl.RData")))
df_resp_all <- get(load(paste0(inpath, "df_resp_all_for_plotting.RData")))
#colnames(df_resp_all)[which(colnames(df_resp_all) == "Rsquared")] <- "R2"

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


###########################################
###trophic levels for every row
###########################################
toMatch <- trophic_tbl$Taxon
for (x in seq(nrow(df_resp_all))){
  trop <- NA
  for (i in toMatch){
    match <- grep(i, df_resp_all[x,"resp"], value=TRUE)
    if (length(match) != 0){
      trop <- trophic_tbl$diet[trophic_tbl$Taxon == i]
    }
    #print(trop)
  }
  df_resp_all$troph[x] <- as.character(trop)
}
df_resp_all$troph <- factor(df_resp_all$troph, levels = c("decomposer", "herbivore", "predator", "generalist"))
df_resp_all <- df_resp_all[with(df_resp_all, order(troph, resp)),] ####sortierung nach alphabet resp ist nicht sooo optimal, weil resids lfter zusammenstehen und nicht abwechselnd reisid und das entsprechende SR- eventuell "resid" hinten an namen dranschreiben
df_resp_all$resp <- factor(df_resp_all$resp, levels = unique(df_resp_all$resp))

#   #print(x["resp"])})
#   trophic_tbl$Taxon %in%x["resp"]
#   for (i in toMatch){
#     a <- grep(i, x["resp"], value=TRUE)
#     if (length(a) != 0){
#       trop <- trophic_tbl$diet[trophic_tbl$Taxon == i]
#     }
#     
#   }
#   
#   grep(paste(toMatch,collapse="|"), x["resp"], value=TRUE)
#   
#   df_resp_all$resp %in% trophic_tbl$Taxon
#   grep("SRmammals", df_resp_all$resp)
#   grep("mammals", df_resp_all$resp)
#   #toMatch <- c("mammals", "dung")
#   toMatch <- trophic_tbl$Taxon
#   grep(paste(toMatch,collapse="|"), df_resp_all$resp, value=TRUE)
#   
# })



##only resid: 
df_plt_resid <- df_resp_all[grep("resid", df_resp_all$resp),]
#only species
df_plt_SR <- df_resp_all[-grep("resid", df_resp_all$resp),]


# plot_resp <- function(df, var, names, resp_title, path = outpath, comm){
#   df$resp = reorder(df$resp, df[[var]], median)
#   p <- ggplot(aes_string(x = "resp", y = var), data = df) + 
#     geom_boxplot() + 
#     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#     xlab(resp_title) + 
#     ylab(var)
#   print(p)
#   ggsave(filename = paste0(path, "plot_", var, "_", comm, ".pdf"), plot = p, width = 25,
#          height = 25, units = "cm")
#   
#   dev.off()
# }
###ggplot(d, aes(x=reorder(fac, y, mean), y=y))



plot_trop <- function(df, var, names, resp_title, path = outpath, comm){
  #df$resp = reorder(df$resp, df[[var]], median)
  
  
  p <- ggplot(aes_string(x = "resp", y = var), data = df) +
    geom_boxplot(aes(fill = troph), lwd = 0.3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab(resp_title) +
    ylab(var)
##########gruppierung der trophischen level hier drunter schreiben
  print(p)
  ggsave(filename = paste0(outpath, "plot_", var, "_", comm, ".pdf"), plot = p, width = 25,
         height = 25, units = "cm")
  
  dev.off()
}



plot_trop(df = df_plt_resid, var = "RMSE", resp_title = "Taxa (Residuals)", comm = "res")
plot_trop(df = df_plt_resid, var = "Rsquared", resp_title = "Taxa (Residuals)", comm = "res")
plot_trop(df = df_plt_SR, var = "RMSE", resp_title = "Taxa", comm = "")
plot_trop(df = df_plt_SR, var = "Rsquared", resp_title = "Taxa", comm = "")



stats <- as.data.frame(ddply(df_resp_all,~resp,summarise,
                             meanR2 = mean(Rsquared),
                             medianR2 = median(Rsquared),
                             sdR2 = sd(Rsquared), 
                             meanRMSE = mean(RMSE),
                             medianRMSE = median(RMSE),
                             sdRMSE = sd(RMSE)))


save(stats, file = paste0(outpath, "stats.RData"))
write.csv(stats, file = paste0(outpath, "stats.csv"))
     