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
                      predictor = c(which(colnames(ldr_SR) %in% "BE_ELEV_ASPECT") : 
                                        which(colnames(ldr_SR) %in% "surface_intensity_sd")), 
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

# ###Testing
# var_imp <- compVarImp(SR_gpm@model$rf_rfe, scale = TRUE)
# 
# var_imp_plot <- plotVarImp(var_imp)
# 
# var_imp_heat <- plotVarImpHeatmap(var_imp, xlab = "Species", ylab = "Band")
# 
# ###resort variable importance values for further calculation
# imp_resp <-lapply(seq(1,34,1), function(i){
# #imp_resp <-lapply(seq(var_imp[[1]]$mean), function(i){
#   imp_j <- lapply((var_imp), function(j){
#     sngl_entry <- j$mean[[i]]
#   })
#   imp_j <- do.call("cbind", imp_j)
# })
# imp_resp <- do.call("rbind", imp_resp)
# imp_resp <- cbind(imp_resp, rowMeans(imp_resp))
# ###label variale importance df
# #colnames
# col_nm <- lapply(seq(var_imp), function (spec){
#   levels(var_imp[[spec]]$RESPONSE)
# })
# col_nm <- do.call("rbind", col_nm)
# colnames(imp_resp) <- c(col_nm, "var_imp_mean")
# #rownames
# row_nm <- levels(var_imp[[1]]$VARIABLE)
# row_nm <- row_nm[1:34]#########################################nur zum testen! Datensatz ist dann Falsch! Werte gehören nicht zwangsläufig zu entsprechendem Eintrag.
# rownames(imp_resp) <- row_nm
# imp_resp_df <- as.data.frame(imp_resp)
# 
# var_imp_srt <- imp_resp_df[with(imp_resp_df, order(imp_resp_df$var_imp_mean, decreasing = T)), ]
# 
# #write.csv(var_imp_srt, file = paste0("var_imp_srt_", mod_date, ".csv"))
# 
# tests <- compRegrTests(SR_gpm@model$rf_rfe, per_model = TRUE, per_selector = TRUE,
#                        details = TRUE)
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ###to do: ##################################################################################
# # change number of resamples to number that makes sense
# 
# 
# ###dies und das getestet
# 
# for (i in seq(1,20,1)){
#   #print(SR_gpm@model$rf_rfe[[12]][[i]]$training$PREDICTOR$scan_angle_sd)
#   print(sum(is.na(SR_gpm@model$rf_rfe[[12]][[i]]$training$PREDICTOR$scan_angle_sd)), na.rm = T)
# }
# 
# ###falls Sortierung/Zuordnung der Zeilen ein Problem ist: 
# for (i in seq(1,20,1)){
#   for (j in seq(SR_gpm@meta$input$RESPONSE)){
#     print(sum(is.na(SR_gpm@model$rf_rfe[[12]][[i]]$training$PREDICTOR[j])))
#   }
# }
# 
# 
# 
# 
# SR_gpm@model$rf_rfe[[12]][[1]]$model$variables
# t <- SR_gpm@model$rf_rfe[[12]][[1]]$model$variables[SR_gpm@model$rf_rfe[[12]][[1]]$model$variables$var == "dtm_elevation_max",]
# aggregate(t$Overall, by = list(t$var), FUN = mean)
# 
# 
# SR_gpm@model$rf_rfe[[12]][[1]]$model$fit$importance
# 
# SR_gpm@model$rf_rfe[[12]][[1]]$model
# 
# 
# SR_gpm@model$rf_rfe[[12]][[1]]$model$fit$importance
# randomForest::importance(SR_gpm@model$rf_rfe[[12]][[1]]$model$fit)
# 
# SR_gpm@model$rf_rfe[[12]][[1]]$model$variables
# 
# 
# #Final (for randomForest)
# SR_gpm@model$rf_rfe[[12]][[1]]$model$fit$importance
# 
# t1[order(t1$x, decreasing = TRUE), ]
# 
# 
# 
# caret::varImp(SR_gpm@model$rf_rfe[[12]][[1]]$model)
# 
# 
# SR_gpm@model$rf_rfe[[12]][[1]]$model$variables
# SR_gpm@model$rf_rfe[[12]][[1]]$model$metric
# r <- SR_gpm@model$rf_rfe[[12]][[1]]$model$resample
# idx <- SR_gpm@model$rf_rfe[[12]][[1]]$model$control$indexOut
# f1 <- SR_gpm@model$rf_rfe[[12]][[1]]$model$variables[SR_gpm@model$rf_rfe[[12]][[1]]$model$variables$Resample == "Fold1",]
# f2 <- SR_gpm@model$rf_rfe[[12]][[1]]$model$variables[SR_gpm@model$rf_rfe[[12]][[1]]$model$variables$Resample == "Fold2",]
# 
# 
# folds <- c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")
# test <- lapply(seq(length(folds)), function(i){
#   f <- SR_gpm@model$rf_rfe[[12]][[1]]$model$variables[SR_gpm@model$rf_rfe[[12]][[1]]$model$variables$Resample == folds[i],]
#   r <- SR_gpm@model$rf_rfe[[12]][[1]]$model$resample[SR_gpm@model$rf_rfe[[12]][[1]]$model$resample$Resample == folds[i],]
#   
#   # f <- f[f$Variables == length(idx[[i]]), ]
#   # f[idx[[i]],]
#   a <- aggregate(f$Overall, by = list(f$var), FUN = max) #ACHTUNG, hier ist max
#   a <- a[order(a$x, decreasing = TRUE),]
#   size = r$Variables[which(r$Rsquared == max(r$Rsquared))] #Variablen bis zum Maximalen Rsq auswählen
#   a[1:size, ]
#   # a[1:length(idx[[i]]),]
# })
# 
# #var_chosen <- c()
# test_vars <- lapply(seq(length(folds)), function(j){
#   vars <- test[[j]]$Group.1
#   })
# unique_test <- unique(unlist(unique_test))
# 
# test <- do.call("rbind", test)
# # aggregate(test$Overall, by = list(test$var), FUN = mean)
# vi <- aggregate(test$x, by = list(test$Group.1), FUN = mean)
# vi[order(vi$x, decreasing = TRUE),]
# 
# 
# #############################testing stuff
# ldr_SR_test <- ldr_SR
# ldr_SR_test$SRmammals <- paste0("A", ldr_SR_test$SRmammals)
# ldr_SR_test$SRmammals <- as.factor(ldr_SR_test$SRmammals)
# 
# meta_test <- createGPMMeta(ldr_SR_test, type = "input",
#                       selector = NULL, 
#                       response = c(which(colnames(ldr_SR_test) %in% "SRmammals")), 
#                       predictor = c(which(colnames(ldr_SR_test) %in% "BE_ELEV_ASPECT") : 
#                                       which(colnames(ldr_SR_test) %in% "lui")), 
#                       meta = c(which(colnames(ldr_SR_test) %in% "plotID") : 
#                                  which(colnames(ldr_SR_test) %in% "bbox_area")))
# SR_gpm_test <- gpm(ldr_SR_test, meta_test)
# 
# SR_gpm_test <- resamplingsByVariable(x = SR_gpm_test, resample = 2)
# SR_gpm_test <- splitMultResp(x = SR_gpm_test, p = 0.9)
# 
# SR_gpm_test <- trainModel(x = SR_gpm_test,
#                      n_var = NULL, 
#                      #n_var = c(seq(5)),
#                      mthd = "rf", 
#                      mode = "rfe",
#                      seed_nbr = 11, 
#                      cv_nbr = 5,
#                      var_selection = "indv",
#                      filepath_tmp = NULL)
# 
# 
# SR_gpm_test@data$input$SRmammals
# SR_gpm_test@meta$input$PREDICTOR_FINAL
# SR_gpm_test@meta$input$RESPONSE
# SR_gpm_test@data$input$SRmammals <- substr(SR_gpm_test@data$input$SRmammals, 2, 3)
# SR_gpm_test@data$input$SRmammals <- as.numeric(SR_gpm_test@data$input$SRmammals)
# SR_gpm_test@data$input$SRmammals <- as.factor(SR_gpm_test@data$input$SRmammals)
