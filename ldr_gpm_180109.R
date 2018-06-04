library(gpm)
library(doParallel)


## Read files and build GPM object
# filepath = "C:/Users/tnauss/permanent/plygrnd/insa/"
# mrg_tbl = read.table(paste0(filepath, "rs_vegindex_biodiv_df.csv"), header = TRUE, sep = ";", dec = ",")

filepath <- "F:/Projekte/Kili/data/"
load(paste0(filepath, "dat_ldr_mrg.RData"))
mrg_tbl <- cbind(mrg_tbl[1:7], mrg_tbl[14:40], mrg_tbl[135:143], mrg_tbl[41:129], mrg_tbl[131:134])

mrg_tbl$selID = paste0("id_", substr(mrg_tbl$plotID, 4, 4))
col_selector = which(names(mrg_tbl) == "selID")
col_diversity = seq(grep("SRmammals", names(mrg_tbl)),
                      grep("abundance", names(mrg_tbl)))
col_precitors = seq(grep("BE_ELEV_ASPECT", names(mrg_tbl)),
                      grep("gap_frac", names(mrg_tbl)))
col_meta <- seq(length(mrg_tbl))[-c(col_selector, col_diversity, col_precitors)]

meta <- createGPMMeta(mrg_tbl, type = "input",
                      selector = col_selector,
                      response = col_diversity,
                      predictor = col_precitors,
                      meta = col_meta)

for(i in seq(ncol(mrg_tbl))){
  print(paste0("###########", colnames(mrg_tbl)[i]))
  print(class(mrg_tbl[,i]))
}

mrg_tbl_gpm <- gpm(mrg_tbl, meta, scale = TRUE)
  


## Set predictor variables!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
sel = colnames(mrg_tbl_gpm@data$input)[c(which(colnames(mrg_tbl_gpm@data$input) == "BE_ELEV_ASPECT"):
                                           which(colnames(mrg_tbl_gpm@data$input) == "gap_frac"))]
mrg_tbl_gpm@meta$input$PREDICTOR_FINAL = sel

mrg_tbl_gpm@meta$input$PREDICTOR_FINAL

mrg_tbl_gpm <- cleanPredictors(x = mrg_tbl_gpm, nzv = TRUE,
                               highcor = TRUE, cutoff = 0.90)

# # Add mean and standard deviation for all leftover precitors
# # Add rs_sd and spec_div_mean_dis in any case
# wmean = which(substr(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL, 1, 4) == "mean")
# wmean = substr(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wmean], 
#                6, nchar(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wmean]))
# wsd = which(substr(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL, 1, 2) == "sd")
# wsd = substr(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wsd], 
#              4, nchar(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wsd]))
# 
# mrg_tbl_gpm@meta$input$PREDICTOR_FINAL = 
#   c("rs_sd", "spec_div_mean_dis", 
#     paste0("mean_", unique(c(wmean, wsd))), 
#     paste0("sd_", unique(c(wmean, wsd))))

## Check for NAs in predictor values
any(is.na(mrg_tbl_gpm@data$input[, mrg_tbl_gpm@meta$input$PREDICTOR_FINAL]))


## Iterate over all response variables
responses = mrg_tbl_gpm@meta$input$RESPONSE_FINAL[1:3]

cl <- makeCluster(detectCores())
registerDoParallel(cl)

mrg_tbl_gpm_list = lapply(responses, function(rsp){
  mrg_tbl_gpm@meta$input$RESPONSE_FINAL = rsp
  
  ## Remove NAs and compute resamples
  mrg_tbl_gpm@data$input = mrg_tbl_gpm@data$input[complete.cases(mrg_tbl_gpm@data$input[, mrg_tbl_gpm@meta$input$RESPONSE_FINAL]), ]
  mrg_tbl_gpm <- splitMultRespLSO(x = mrg_tbl_gpm, nbr = 1)
  
  
  mrg_tbl_gpm_model <- trainModel(x = mrg_tbl_gpm,
                                  n_var = NULL, 
                                  mthd = "pls",
                                  mode = "rfe",
                                  seed_nbr = 11, 
                                  cv_nbr = 5,
                                  var_selection = "indv",
                                  filepath_tmp = NULL)
  
  filepath = paste0(filepath, "mrg_tbl_gpm_model_", rsp, "_.rds")
  saveRDS(mrg_tbl_gpm_model, file = filepath)
})


## Combine models in gpm objct
models = list.files(filepath, pattern = glob2rx("mrg_tbl_gpm_model_*"), full.names = TRUE)

mrg_tbl_gpm_model = readRDS(models[1])

for(i in seq(2, length(models))){
  temp = readRDS(models[i])
  mrg_tbl_gpm_model@model$pls_rfe[[i]] = temp@model$pls_rfe[[1]]
}
saveRDS(mrg_tbl_gpm_model, file = paste0(filepath, "mrg_tbl_gpm_model_pls_2018-01-08.rds"))


var_imp <- compVarImp(mrg_tbl_gpm_model@model[[1]], scale = FALSE)

var_imp_scale <- compVarImp(mrg_tbl_gpm_model@model[[1]], scale = TRUE)

var_imp_plot <- plotVarImp(var_imp)

var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")

tstat <- compRegrTests(mrg_tbl_gpm_model@model[[1]])

# tstat_mean <- merge(tstat[[1]], mrg_tbl_gpm_model@model[[1]], 
#                     by.x = "Response", by.y="names")
aggregate(tstat$r_squared, by = list(tstat$model_response), mean)
tstat_mean[order(tstat_mean$Kappa_mean, decreasing = TRUE),]
