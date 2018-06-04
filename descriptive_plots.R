###general stuff
library(rPointDB)
library(dplyr)
library(ggplot2)

###Settings

inpath <- "E:/Projekte/Kili/data/"
outpath <- "E:/Projekte/Kili/out/vars_resp_per_elevation/"

###############################################################################
#don't change anything past this point except you know, what you are doing
###############################################################################

# open remote sensing database connection

###read data
#pnts <- read.csv(paste0(inpath,"ldr_all_points.csv"), header=T, sep=",")
load(paste0(inpath,"ldr_all_points_10m.RData"))
load(paste0(inpath, "ldr_SR_str.RData"))

#pars <- c(names(ldr_SR)[10:75]) #####namen vergeben

###design stuff
landuse_col <- c(cof = "burlywood4", fer = "darkolivegreen", flm = "darkseagreen", foc = "chartreuse",
                 fod = "chartreuse3", fpd = "aquamarine3", fpo = "aquamarine", gra = "palegreen1",
                 hel = "#333BFF", hom = "red3", mai = "yellow", sav = "darkorange1")
landuse_names <- c(cof = "Coffee Plantation (cof)", fer = "Erica Forest (fer)", 
                   flm = "Lower Mountain Forest (flm)", foc = "Ocotea Forest (foc)", 
                   fod = "Ocotea Forest dist. (foc)", fpd = "Podocarpus Forest dist. (fpd)", 
                   fpo = "Podocarpus Forest (fpo)", gra = "Grassland (gra)", hel = "Helichrysum (hel)", 
                   hom = "Homegarden (hom)", mai = "Maize Plantation (mai)", sav = "Savanna (sav)")

# plots <- lapply(pars, function(i){
plots_par <- lapply(which(names(ldr_SR_str) == "BE_ELEV_ASPECT"):
                      which(names(ldr_SR_str) == "gap_frac"), function(i){
  y <- ldr_SR_str[,i]
  nam <- names(ldr_SR_str)[i]
  ggplot(ldr_SR_str, aes(x = elevation, y = y, colour = cat)) +
    geom_point() +
    geom_text(aes(label=plotID),hjust=-0.1, vjust=-0.1, show.legend = F) +
    labs(colour = "landuse", y = nam) + 
    scale_colour_manual(values = landuse_col, name = "landuse", labels = landuse_names)
  ggsave(filename = paste0(outpath, "elev_", nam, ".pdf"), plot = last_plot())
})
