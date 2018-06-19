library(gpm)
library(foreach)

setwd("C:/Users/iotte/Desktop/results")

####
filepath_gpm = "C:/Users/iotte/Desktop/results"

###### alpha diversity ######
alpha = readRDS(paste0(filepath_gpm,
                       "/gpm_hara_species_richness_model_pls_ffs_180525.rds"))
# residues
alpha_res = readRDS(paste0(filepath_gpm,
                           "/gpm_hara_res_species_richness_model_pls_ffs_180524.rds"))

###### beta diversity ######
beta = readRDS(paste0(filepath_gpm,
                      "/gpm_hara_species_betadiv_model_pls_ffs_180528.rds"))
# residues
beta_res = readRDS(paste0(filepath_gpm,
                          "/gpm_hara_species_betadiv_model_pls_ffs_res_180528.rds"))


#### results
mrg_tbl_gpm_model = beta_res

var_imp_scale = compVarImp(mrg_tbl_gpm_model@model[[1]], scale = TRUE)

var_imp_scale_jac = var_imp_scale[c(1,2,7,8,13,14,19,20,25,26,
                                    31,32,37,38,43,44,49,50,55,
                                    56,61,62,67,68,73,74,79,80,85,86,91,92,
                                    97,98,103,104,109,110,115,
                                    116,121,122,127,128)]


var_imp_scale_jne = var_imp_scale[c(3,4,9,10,15,16,21,22,27,28,
                                    33,34,39,40,45,46,51,52,57,58,
                                    63,64,69,70,75,76,81,82,87,88,
                                    93,94,99,100,105,106,111,112,
                                    117,118,123,124,129,130)]

var_imp_scale_jtu = var_imp_scale[c(5,6,11,12,17,18,23,24,29,30,
                                    35,36,41,42,47,48,53,54,59,60,
                                    65,66,71,72,77,78,83,84,89,90,
                                    95,96,101,102,107,108,113,114,
                                    119,120,125,126,131,132)]


####
#var_imp_scale_08 = foreach(i = seq(var_imp_scale)) %do% {
#  subset(var_imp_scale[[i]], 
#         var_imp_scale[[i]]$mean >= 0.8)
#}

#var_final_alpha_res = unlist(lapply(seq(var_imp_scale_08), function(i){
#  unique(var_imp_scale_08[[i]]$VARIABLE)
#}
#))
#un = as.character(unique(var_final_alpha_res))
#####
#var_imp_plot = plotVarImp(var_imp)
#var_imp_heat = plotVarImpHeatmap(var_imp_scale, 
#                                 xlab = "Species", ylab = "Band")

temp <- do.call("rbind", var_imp_scale_jac)
#temp_2 <- str_sort(temp$VARIABLE, temp$RESPONSE, temp$mean, temp$n)
#write.csv2(temp_2, "temp_beta.csv")
#var_imp_scale = read.csv2("temp_alpha_res.csv")

#var_imp_scale = var_imp_scale[,-5]


##############
#levels(temp$VARIABLE)
temp_3 = temp
#library(stringr)
#tst2 = str_sort(temp_3$VARIABLE)
#unique(tst2)


#### rename levels for plotting
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_CARI"] <- "mean_cellulose"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_MCARI"] <- "mean_cellulose"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Carter"] <- "mean_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Carter2"] <- "mean_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Carter3"] <- "mean_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Carter4"] <- "mean_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Carter6"] <- "mean_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_DWSI4"] <- "mean_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_SR2"] <- "mean_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_SR3"] <- "mean_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_SR6"] <- "mean_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_SR7"] <- "mean_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_SR8"] <- "mean_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PWI"] <- "mean_plant_stress"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_CRI1"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_CRI2"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_CRI3"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_CRI4"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_ClAInt"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Datt"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Datt4"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Datt5"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Datt6"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_DD"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_GDVI_2"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_GDVI_3"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_GDVI_4"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_GI"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Gitelson"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Gitelson2"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Green.NDVI"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Maccioni"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_mND705"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_mNDVI"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_mSR705"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_MTCI"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_NDVI2"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_NPCI"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_OSAVI"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_SPVI"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_TCARI.OSAVI"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_TCARI2"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_TCARI2.OSAVI2"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_TGI"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Vogelmann"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Vogelmann2"] <- "mean_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Vogelmann4"] <- "mean_chlorophyll"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PRI.CI2"] <- "mean_carotenoid"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PSRI"] <- "mean_carotenoid"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PSSR"] <- "mean_carotenoid"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_SRPI"] <- "mean_carotenoid"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PSND"] <- "mean_pigment"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_EVI"] <- "mean_structure"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_RDVI"] <- "mean_structure"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_mSR"] <- "mean_photosynthese"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_mSR2"] <- "mean_photosynthese"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PARS"] <- "mean_photosynthese"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PRI"] <- "mean_photosynthese"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PRI_norm"] <- "mean_photosynthese"


levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_CARI"] <- "sd_cellulose"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_MCARI"] <- "sd_cellulose"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Carter"] <- "sd_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Carter2"] <- "sd_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Carter3"] <- "sd_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Carter4"] <- "sd_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Carter6"] <- "sd_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_DWSI4"] <- "sd_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_SR2"] <- "sd_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_SR3"] <- "sd_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_SR6"] <- "sd_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_SR7"] <- "sd_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_SR8"] <- "sd_plant_stress"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PWI"] <- "sd_plant_stress"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_CRI1"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_CRI2"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_CRI3"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_CRI4"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_ClAInt"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Datt"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Datt4"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Datt5"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Datt6"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_DD"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_GDVI_2"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_GDVI_3"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_GDVI_4"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_GI"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Gitelson"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Gitelson2"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Green.NDVI"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Maccioni"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_mND705"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_mNDVI"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_mSR705"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_MTCI"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_NDVI2"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_NPCI"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_OSAVI"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_SPVI"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_TCARI.OSAVI"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_TCARI2"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_TCARI2.OSAVI2"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_TGI"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Vogelmann"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Vogelmann2"] <- "sd_chlorophyll"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Vogelmann4"] <- "sd_chlorophyll"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PRI.CI2"] <- "sd_carotenoid"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PSRI"] <- "sd_carotenoid"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PSSR"] <- "sd_carotenoid"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_SRPI"] <- "sd_carotenoid"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PSND"] <- "sd_pigment"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_EVI"] <- "sd_structure"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_RDVI"] <- "sd_structure"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_mSR"] <- "sd_photosynthese"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_mSR2"] <- "sd_photosynthese"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PARS"] <- "sd_photosynthese"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PRI"] <- "sd_photosynthese"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PRI_norm"] <- "sd_photosynthese"


###############################################
temp_4 = subset(temp_3, temp_3$RESPONSE != "lui")
var_imp_scale = temp_4

plotVarImpHeatmapAlpha <- function (var_imp_scale, xlab = "Variable", ylab = "Method", vis_range = "minmax") 
    {
    #temp <- do.call("rbind", var_imp_scale)
    temp <- var_imp_scale  
    temp$VARIABLE <- factor(temp$VARIABLE, levels = sort(as.character(unique(temp$VARIABLE))))
    temp$RESPONSE <- factor(temp$RESPONSE, 
                          levels(temp$RESPONSE)[c(3,17,7,21,4,6,
                                                 8:9,11,14,18,20,
                                                 22:23,26,28,24,27,
                                                 2,5,10,12:13,15,
                                                 16,19,25,1)])
    temp$trophLev = ifelse(temp$RESPONSE %in% c("SRmammals","SRanimals",
                                                "SRsyrphids","SRants","SRsnails"), 1,
                           ifelse(temp$RESPONSE %in% c("SRdungbeetles","SRmillipedes","SRcollembola"), 2,
                                  ifelse(temp$RESPONSE %in% c("SRbees","SRmoths","SRorthoptera"), 3,
                                         ifelse(temp$RESPONSE %in% c("SRbirds","SRbats"), 5,
                                                 ifelse(temp$RESPONSE %in% c("SRallplants",
                                                                             "SRasterids",
                                                                             "SRconifers",
                                                                             "SReudicots",
                                                                             "SRferns",
                                                                             "SRlycopodiopsida",
                                                                             "SRmagnoliids",
                                                                             "SRmonocots",
                                                                             "SRrosids"), 6, 
                                                       ifelse(temp$RESPONSE %in% "lui", 6, 4))))))
    temp$trophLev <- as.factor(temp$trophLev)
    levels(temp$trophLev)[levels(temp$trophLev) == 1] <- "generalist"
    levels(temp$trophLev)[levels(temp$trophLev) == 2] <- "decomposer"
    levels(temp$trophLev)[levels(temp$trophLev) == 3] <- "herbivore"
    levels(temp$trophLev)[levels(temp$trophLev) == 4] <- "predator"
    levels(temp$trophLev)[levels(temp$trophLev) == 5] <- "birds_bats"
    levels(temp$trophLev)[levels(temp$trophLev) == 6] <- "plants"
    
    temp = temp %>%
      group_by(VARIABLE,trophLev) %>%
      summarise(avg = mean(mean)) %>%
      arrange(avg)
    colnames(temp) = c("VARIABLE", "trophLev", "mean")
    
    tempTrop = temp %>%
      group_by(trophLev) %>%
      summarise(avg = mean(mean)) %>%
      arrange(avg)
    levels_trophLev = as.character(tempTrop$trophLev)
    temp$trophLev <- factor(temp$trophLev,levels = levels_trophLev)
    
    tempVar = temp %>%
      group_by(VARIABLE) %>%
      summarise(avg = mean(mean)) %>%
      arrange(avg)
    levels_Var = as.character(tempVar$VARIABLE)
    temp$VARIABLE <- factor(temp$VARIABLE, levels = c(levels_Var))
    
    if (vis_range == "minmax") {
      vis_range <- c(min(temp$mean), max(temp$mean))
      }
    clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))
    lattice::levelplot(mean ~ trophLev * VARIABLE, data = temp, 
    #lattice::levelplot(mean ~ trophLev * VARIABLE, data = temp, 
                     col.regions = clr(101), at = seq(vis_range[1], vis_range[2], 
                                                      length.out = 101), asp = 1, as.table = TRUE, ylab = ylab, 
                     xlab = xlab, scales = list(x = list(rot = 0)), main = "Variable Importance Residues Species Richness", 
                     cex.title = 1, cex.axis.text = 0.5, colorkey = list(space = "top", width = 1, 
                                                    height = 0.75), panel = function(...) {
                                                      grid::grid.rect(gp = grid::gpar(col = NA, fill = "grey60"))
                                                      panel.levelplot(...)
                                                    })
}



var_imp_heat = plotVarImpHeatmapAlpha(var_imp_scale, 
                                 xlab = "", ylab = "")



#####
var_imp_scale_jac = temp_3

plotVarImpHeatmapBeta <- function (var_imp_scale_jac, xlab = "Variable", ylab = "Method", vis_range = "minmax") 
{
  #temp <- do.call("rbind", var_imp_scale_jac)
  temp <- var_imp_scale_jac
  temp$VARIABLE <- factor(temp$VARIABLE, levels = sort(as.character(unique(temp$VARIABLE))))
  temp$RESPONSE <- factor(temp$RESPONSE, 
                          levels(temp$RESPONSE)[c(79:84,25:30,109:114,1:12,19:24,31:42,
                                                  49:54,67:72,85:90,103:108,115:120,
                                                  127:132,13:18,43:48,55:66,73:78,97:102,
                                                  91:96,121:126)])
  temp$RESPONSE <- factor(temp$RESPONSE,
                          levels(temp$RESPONSE)[c(3:4,15:16,31:32,
                                                  7:8,9:10,13:14,17:18,
                                                  37:38,41:42,23:24,5:6,
                                                  29:30,33:34,43:44,
                                                  11:12,39:40,19:22,1:2,
                                                  25:26,35:36)])
  temp$trophLev = ifelse(substr(temp$RESPONSE, 1, 7) %in% "mammals", 1, 
                         ifelse(substr(temp$RESPONSE, 1, 11) %in% "dungbeetles", 2,
                                ifelse(substr(temp$RESPONSE, 1, 4) %in% "bats", 5,
                                       ifelse(substr(temp$RESPONSE, 1, 5) %in% "birds", 5,
                                              ifelse(substr(temp$RESPONSE, 1, 3) %in% "bees", 3,
                                                     ifelse(substr(temp$RESPONSE, 1, 10) %in% "orthoptera", 3,
                                                            ifelse(substr(temp$RESPONSE, 1, 10) %in% "millipedes", 2,
                                                                   ifelse(substr(temp$RESPONSE, 1, 10) %in% "collembola", 2, 
                                                                          ifelse(substr(temp$RESPONSE, 1, 8) %in% "syrphids", 1,
                                                                                 ifelse(substr(temp$RESPONSE, 1, 4) %in% "ants", 1,
                                                                                        ifelse(substr(temp$RESPONSE, 1, 5) %in% "moths", 3,
                                                                                               ifelse(substr(temp$RESPONSE, 1, 6) %in% "snails", 1,
                                                                                                     ifelse(substr(temp$RESPONSE, 1, 8) %in% "asterids", 6,
                                                                                                            ifelse(substr(temp$RESPONSE, 1, 8) %in% "conifers", 6,
                                                                                                                   ifelse(substr(temp$RESPONSE, 1, 8) %in% "eudicots", 6,
                                                                                                                          ifelse(substr(temp$RESPONSE, 1, 5) %in% "ferns", 6,
                                                                                                                                 ifelse(substr(temp$RESPONSE, 1, 6) %in% "mosses", 6,
                                                                                                                                        ifelse(substr(temp$RESPONSE, 1, 10) %in% "magnoliids", 6,
                                                                                                                                               ifelse(substr(temp$RESPONSE, 1, 8) %in% "monocots", 6,
                                                                                                                                                      ifelse(substr(temp$RESPONSE, 1, 6) %in% "rosids", 6,
                                                                                                                                                             4))))))))))))))))))))
  temp$trophLev <- as.factor(temp$trophLev)
  levels(temp$trophLev)[levels(temp$trophLev) == 1] <- "generalist"
  levels(temp$trophLev)[levels(temp$trophLev) == 2] <- "decomposer"
  levels(temp$trophLev)[levels(temp$trophLev) == 3] <- "herbivore"
  levels(temp$trophLev)[levels(temp$trophLev) == 4] <- "predator"
  levels(temp$trophLev)[levels(temp$trophLev) == 5] <- "birds_bats"
  levels(temp$trophLev)[levels(temp$trophLev) == 6] <- "plants"
                                                                   
  temp = temp %>%
    group_by(VARIABLE,trophLev) %>%
    summarise(avg = mean(mean)) %>%
    arrange(avg)
  colnames(temp) = c("VARIABLE", "trophLev", "mean")
  
  tempTrop = temp %>%
    group_by(trophLev) %>%
    summarise(avg = mean(mean)) %>%
    arrange(avg)
  levels_trophLev = as.character(tempTrop$trophLev)
  temp$trophLev <- factor(temp$trophLev,levels = levels_trophLev)
  
  tempVar = temp %>%
    group_by(VARIABLE) %>%
    summarise(avg = mean(mean)) %>%
    arrange(avg)
  levels_Var = as.character(tempVar$VARIABLE)
  temp$VARIABLE <- factor(temp$VARIABLE, levels = c(levels_Var))
  
  if (vis_range == "minmax") {
    vis_range <- c(min(temp$mean), max(temp$mean))
  }
  clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))
  lattice::levelplot(mean ~ trophLev * VARIABLE, data = temp, 
                     col.regions = clr(101), at = seq(vis_range[1], vis_range[2], 
                                                      length.out = 101), asp = 1, as.table = TRUE, ylab = ylab, 
                     xlab = xlab, scales = list(x = list(rot = 0)), main = "Variable Importance Residues Beta Diversity", 
                     cex.title = 1, colorkey = list(space = "top", width = 1, 
                                                    height = 0.75), panel = function(...) {
                                                      grid::grid.rect(gp = grid::gpar(col = NA, fill = "grey60"))
                                                      panel.levelplot(...)
                                                    })
}

var_imp_heat = plotVarImpHeatmapBeta(var_imp_scale_jac, 
                                      xlab = "", ylab = "")


mrg_tbl_gpm_model@model$pls_ffs[[65]][[1]]$response
mrg_tbl_gpm_model@model$pls_ffs[[6]][[1]]$model

mean(mrg_tbl_gpm_model@model$pls_ffs[[1]][[1]]$model$resample$Rsquared)

