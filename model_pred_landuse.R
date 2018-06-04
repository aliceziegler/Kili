###To do: 
#Pflanzen species richnes (SR) auch mit benutzen (siehe Paper: Nature Communications)
#fix columns with NAs - find proper solution
#gra5 kann wieder rein!? Ist mit fehlenden Spalten abgedeckt

###settings
inpath <- "H:/Kili/data/"
model_path <- "H:/Kili/models/"
###setup###
library(gpm)
library(stringr)

ldr_SR_all <- read.csv(paste0(inpath, "ldr_SR.csv"), header = T, sep = ",")
#for now subset: only columns/rows without NA
#colnames(ldr_SR_all)[colSums(is.na(ldr_SR_all)) > 0]
#ldr_SR_all$plotID[rowSums(is.na(ldr_SR_all)) > 0]
ldr_SR <- ldr_SR_all[(-which(ldr_SR_all$plotID %in% c("mai1", "mai2", "mai4", "gra2", "gra5", "hel1", "hel2", "hel3", 
                                                      "hel4", "fer1"))), 
                     (-which(colnames(ldr_SR_all) %in% c("BE_FHD", "BE_PR_CAN", "BE_PR_UND", "BE_RD_CAN", "BE_RD_UND", 
                                                         "SRdungbeetles", "SRspiders", "SRheteroptera", "SRcollembola", 
                                                         "SRanimals")))]
### to change class of columns to factor (categorical prediction) #for testing reasons because perhaps
###there are not enough different values to predict continuously
#ldr_SR[c(which(colnames(ldr_SR) %in% "BE_ELEV_ASPECT") : which(colnames(ldr_SR) %in% "SRallplants"))] <- 
#  lapply(ldr_SR[c(which(colnames(ldr_SR) %in% "BE_ELEV_ASPECT") : which(colnames(ldr_SR) %in% "SRallplants"))], as.factor)

##numeric, because with factor, entries(=values) that appear only once are always put to training in splitting. which means, 
#they are never tested! gpm denies that by giving back an empty dataframe, which results in no prediction at all
ldr_SR[which(colnames(ldr_SR)=="BE_ELEV_ASPECT"):ncol(ldr_SR)] <- lapply(
  ldr_SR[which(colnames(ldr_SR)=="BE_ELEV_ASPECT"):ncol(ldr_SR)], as.numeric)

str(ldr_SR)
list_class <-lapply(ldr_SR, class)

#colnames(ldr_SR)[colSums(is.na(ldr_SR)) > 0]
meta <- createGPMMeta(ldr_SR, type = "input",
                      selector = NULL, 
                      # response = c(which(colnames(ldr_SR) %in% "SRmammals") : 
                      #                which(colnames(ldr_SR) %in% "SRallplants")), 
                      response = which(colnames(ldr_SR) %in% "SRmammals"),
                      predictor = which(colnames(ldr_SR) %in% "cat"), 
                      # predictor = c(which(colnames(ldr_SR) %in% "BE_ELEV_ASPECT") : 
                      #                 which(colnames(ldr_SR) %in% "surface_intensity_sd")), 
                      meta = c(which(colnames(ldr_SR) %in% "plotID") : 
                                 which(colnames(ldr_SR) %in% "bbox_area")))
SR_gpm <- gpm(ldr_SR, meta)


SR_gpm <- resamplingsByVariable(x = SR_gpm, resample = 20)
SR_gpm <- splitMultResp(x = SR_gpm, p = 0.9)

#save(SR_gpm, file = paste0(model_path, "170516_before_training.rda"))

### has to happen in two steps, as the predicted plant-species-richness is ment to function as a 
### new independetvariable for the animal prediction. 

SR_gpm <- trainModel(x = SR_gpm,
                     n_var = NULL, 
                     #n_var = c(seq(5)),
                     mthd = "rf", 
                     mode = "rfe",
                     seed_nbr = 11, 
                     cv_nbr = 5,
                     var_selection = "indv",
                     filepath_tmp = NULL)

###systemzeit einbauen zum Speichern
mod_time <- str_replace_all(Sys.time(), "[ ]", "_")
mod_time <- str_replace_all(mod_time, "[-:]", "")
mod_time <- substr(mod_time, 1, nchar(mod_time)-2)

save(SR_gpm, file = paste0(model_path, mod_time, "_model.rda"))

#load(paste0(model_path, "20170313_2135_model.rda"))
