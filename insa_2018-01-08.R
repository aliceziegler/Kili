library(gpm)
library(doParallel)


## Read files and build GPM object
filepath = "F:/Projekte/Kili/data/"
insa_tbl = read.table(paste0(filepath, "rs_vegindex_biodiv_df.csv"), header = TRUE, sep = ";", dec = ",")

insa_tbl$min_ClAInt = as.numeric(insa_tbl$min_ClAInt)

insa_tbl$selID = paste0("id_", substr(insa_tbl$plotID, 4, 4))
col_selector = which(names(insa_tbl) == "selID")
col_diversity = seq(grep("lui_biomass_removal", names(insa_tbl)), 
                      grep("SRallplants", names(insa_tbl)))
col_precitors = seq(grep("rs_sd", names(insa_tbl)), 
                      grep("IQR_M_Vogelmann4", names(insa_tbl)))
col_precitors = col_precitors[-which(col_precitors %in% 725)]
col_meta <- seq(length(insa_tbl))[-c(col_selector, col_diversity, col_precitors)]

meta <- createGPMMeta(insa_tbl, type = "input",
                      selector = col_selector,
                      response = col_diversity,
                      predictor = col_precitors,
                      meta = col_meta)

insa_tbl_gpm <- gpm(insa_tbl, meta, scale = TRUE)
  

## Set predictor variables
sel = c("rs_sd", "spec_div_mean_dis", 
        colnames(insa_tbl_gpm@data$input)[
          grep(glob2rx("mean*"), 
               colnames(insa_tbl_gpm@data$input))],
        colnames(insa_tbl_gpm@data$input)[
          grep(glob2rx("sd*"), 
               colnames(insa_tbl_gpm@data$input))])
insa_tbl_gpm@meta$input$PREDICTOR_FINAL = sel

insa_tbl_gpm@meta$input$PREDICTOR_FINAL

insa_tbl_gpm <- cleanPredictors(x = insa_tbl_gpm, nzv = TRUE,
                               highcor = TRUE, cutoff = 0.90)

# Add mean and standard deviation for all leftover precitors
# Add rs_sd and spec_div_mean_dis in any case
wmean = which(substr(insa_tbl_gpm@meta$input$PREDICTOR_FINAL, 1, 4) == "mean")
wmean = substr(insa_tbl_gpm@meta$input$PREDICTOR_FINAL[wmean], 
               6, nchar(insa_tbl_gpm@meta$input$PREDICTOR_FINAL[wmean]))
wsd = which(substr(insa_tbl_gpm@meta$input$PREDICTOR_FINAL, 1, 2) == "sd")
wsd = substr(insa_tbl_gpm@meta$input$PREDICTOR_FINAL[wsd], 
             4, nchar(insa_tbl_gpm@meta$input$PREDICTOR_FINAL[wsd]))

insa_tbl_gpm@meta$input$PREDICTOR_FINAL = 
  c("rs_sd", "spec_div_mean_dis", 
    paste0("mean_", unique(c(wmean, wsd))), 
    paste0("sd_", unique(c(wmean, wsd))))

## Check for NAs in predictor values
any(is.na(insa_tbl_gpm@data$input[, insa_tbl_gpm@meta$input$PREDICTOR_FINAL]))


## Iterate over all response variables
responses = insa_tbl_gpm@meta$input$RESPONSE_FINAL[7:33]

cl <- makeCluster(detectCores())
registerDoParallel(cl)

insa_tbl_gpm_list = lapply(responses, function(rsp){
  insa_tbl_gpm@meta$input$RESPONSE_FINAL = rsp
  
  ## Remove NAs and compute resamples
  insa_tbl_gpm@data$input = insa_tbl_gpm@data$input[complete.cases(insa_tbl_gpm@data$input[, insa_tbl_gpm@meta$input$RESPONSE_FINAL]), ]
  insa_tbl_gpm <- splitMultRespLSO(x = insa_tbl_gpm, nbr = 1)
  
  
  insa_tbl_gpm_model <- trainModel(x = insa_tbl_gpm,
                                  n_var = NULL, 
                                  mthd = "pls",
                                  mode = "rfe",
                                  seed_nbr = 11, 
                                  cv_nbr = 5,
                                  var_selection = "indv",
                                  filepath_tmp = NULL)
  
  filepath = paste0(filepath, "insa_tbl_gpm_model_", rsp, "_.rds")
  saveRDS(insa_tbl_gpm_model, file = filepath)
})


## Combine models in gpm objct
models = list.files(filepath, pattern = glob2rx("insa_tbl_gpm_model_*"), full.names = TRUE)

insa_tbl_gpm_model = readRDS(models[1])

for(i in seq(2, length(models))){
  temp = readRDS(models[i])
  insa_tbl_gpm_model@model$pls_rfe[[i]] = temp@model$pls_rfe[[1]]
}
saveRDS(insa_tbl_gpm_model, file = paste0(filepath, "insa_tbl_gpm_model_pls_2018-01-08.rds"))


var_imp <- compVarImp(insa_tbl_gpm_model@model[[1]], scale = FALSE)

var_imp_scale <- compVarImp(insa_tbl_gpm_model@model[[1]], scale = TRUE)

var_imp_plot <- plotVarImp(var_imp)

var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")

tstat <- compRegrTests(insa_tbl_gpm_model@model[[1]])

# tstat_mean <- merge(tstat[[1]], insa_tbl_gpm_model@model[[1]], 
#                     by.x = "Response", by.y="names")
aggregate(tstat$r_squared, by = list(tstat$model_response), mean)
tstat_mean[order(tstat_mean$Kappa_mean, decreasing = TRUE),]
