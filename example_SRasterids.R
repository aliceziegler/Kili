## extremvorhersage
library(CAST)
library(caret)

setwd("C:/Users/Alice Ziegler/Uni/Projekte/Kili/src/")
sub <- "okt18/2018-10-29_ffs_plsnofrst_no_elevelev2lui_cvindex/"

inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)

i <- "../data/okt18/2018-10-29_ffs_plsnofrst_no_elevelev2lui_cvindex/indv_model_run1_ffs_pls_sumdecomposerN3.RData"
# i <- "../data/okt18/2018-10-29_ffs_plsnofrst_no_elevelev2lui_cvindex/indv_model_run1_ffs_pls_residSRasterids.RData"

mod <- get(load(i))
outs_lst <- readRDS(paste0(inpath, "../outs_lst.rds"))
tbl_scl <- readRDS(file = paste0(inpath, "tbl_scl.rds"))
mrg_tbl <- readRDS(paste0(inpath, "../dat_ldr_mrg.rds"))

nm <- strsplit(x = i, split = "_|\\.")
resp_as_nm <- nm[[1]][length(nm[[1]])-1]####################sollte auf dauer anders (relativ) angegeben werden!
run <- nm[[1]][length(nm[[1]])-4]
run_indx <- as.numeric(gsub("[[:alpha:]]", "", run))

resp <- gsub("jne", "_jne_", resp_as_nm) #####################sollte auf dauer in ein gsub statement zsuammengefasst werden
resp <- gsub("jac", "_jac_", resp)
resp <- gsub("jtu", "_jtu_", resp)
resp <- gsub("height", "_height_", resp)
resp <- gsub("width", "_width_", resp)
resp <- gsub("body", "body_", resp)
resp <- gsub("kipps", "kipps_", resp)
resp <- gsub("tarsus", "_tarsus_", resp)
resp <- gsub("wing", "_wing_", resp)
resp <- gsub("^sum", "sum_", resp)
resp <- gsub("residsum", "residsum_", resp)
resp <- gsub("N(\\d+)", "_N\\1", resp)



###hier var imp und eventuell anderes für heatmap berechnen!
varimp <- varImp(mod)$importance
selvars <- mod$selectedvars
nmbr_selvars <- length(selvars)
selvars_perf <- mod$selectedvars_perf
selvars_perf_SE <- mod$selectedvars_perf_SE
perf_all <- mod$perf_all

out_plt <- outs_lst[[run_indx]]$plotID 

tbl_in <- tbl_scl[-which(tbl_scl$plotID %in% out_plt),]
tbl_out <- tbl_scl[which(tbl_scl$plotID %in% out_plt),]

prediction <- predict(mod, newdata = tbl_out)

prdct <- data.frame(plotID = tbl_out$plotID, 
                    plotUnq = tbl_out$plotUnq, 
                    run = run, 
                    resp = prediction)


##WHY???
mod
prdct
# tbl_out$SRasterids
# mrg_tbl$SRasterids
mod$trainingData
mod$selectedvars
tbl_out$plotID
tbl_out[,c(which(colnames(tbl_out) %in% mod$selectedvars))]
# mrg_tbl[which(mrg_tbl$plotID %in% tbl_out$plotID), "SRasterids"]
# mrg_tbl[which(mrg_tbl$plotID %in% tbl_in$plotID), "SRasterids"]

# crss <- readRDS("C:/Users/Alice Ziegler/Uni/Projekte/Kili/out/okt18/2018-10-29_ffs_plsnofrst_no_elevelev2lui_cvindex/selvars_crsstbl.rds")
