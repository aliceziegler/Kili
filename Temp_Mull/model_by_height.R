###Settings

inpath <- "G:/Kili/data/"
outpath <- "G:/Kili/data/"


ldr_SR <- read.csv(paste0(inpath, "ldr_SR.csv"))
  
model <- lm(SRmammals ~ elevation, ldr_SR)

plot(ldr_SR$SRmammals)
abline(model)

ldr_SR$SRmammals_pred_by_height <- model$coefficients[1] + model$coefficients[2]*ldr_SR$SRmammals
#predict geht auch
#predict(model, ldr_SR)
