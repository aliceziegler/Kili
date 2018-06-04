library(caret)
library(proxy)
###settings
inpath <- "H:/Kili/data/"
model_path <- "H:/Kili/models/"
set.seed(90)
###setup###


ldr_SR_all <- read.csv(paste0(inpath, "ldr_SR.csv"), header = T, sep = ",")
#for now subset: only columns/rows without NA
#colnames(ldr_SR_all)[colSums(is.na(ldr_SR_all)) > 0]
#ldr_SR_all$plotID[rowSums(is.na(ldr_SR_all)) > 0]
ldr_SR <- ldr_SR_all[(-which(ldr_SR_all$plotID %in% c("mai1", "mai2", "mai4", "gra2", "gra5", "hel1", "hel2", "hel3", 
                                                      "hel4", "fer1"))), 
                     (-which(colnames(ldr_SR_all) %in% c("BE_FHD", "BE_PR_CAN", "BE_PR_UND", "BE_RD_CAN", "BE_RD_UND", 
                                                         "SRdungbeetles", "SRspiders", "SRheteroptera", "SRcollembola", 
                                                         "SRanimals")))]

#######################################################################################################################
#####Visualisation for regression#####
regVar <- c(colnames(ldr_SR[, 10:70]))

theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = ldr_SR[, regVar], 
            y = ldr_SR$SRmammals, 
            plot = "scatter", 
            layout = c(10, 6))

featurePlot(x = ldr_SR[, regVar], 
            y = ldr_SR$SRmammals,
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(10, 6))

featurePlot(x = ldr_SR[, regVar], 
            y = ldr_SR$SRbirds,
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(10, 6))

############################
###checking for near zero (Problematisch, wenn in einem Prediktor (fast) alle Werte gleich sind (Zero-variance))
###die, die hier bei ldr_SR ausgespuckt werden, sind nur welche, die al spredictoren sowieso sinnlos sind: 
### area, bbox_area, point_coverage, pulse_returns_min
nzv <- nearZeroVar(ldr_SR, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]

#######################################
###some of the steps in the caret tutorial are missing at this point
#########################################

#####################################
###data splitting
#####################################
###based on output (random)
trainIndex <- createDataPartition(ldr_SR$SRbirds, p = .8, 
                                  list = FALSE, 
                                  times = 1)
#summary(factor(ldr_SR$SRbirds))
Train <- ldr_SR[ trainIndex,]
Test  <- ldr_SR[-trainIndex,]

###based on predictors (maximum dissimilarity)

testing_ran <- scale(ldr_SR[, c("SRbirds", "BE_H_SD")])
## A random sample of 5 data points
startSet <- sample(1:dim(testing_ran)[1], 5)
samplePool <- testing_ran[-startSet,]
start <- testing_ran[startSet,]
newSamp <- maxDissim(start, samplePool, n = 20)
head(newSamp)

training <- ldr_SR[-newSamp]
testing <- ldr_SR[newSamp]

###based on user choice could be passed by argument "index"
#######################################
###training
#######################################
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)


rfFit_mamm <- train(training[,c(4:29, 31:36, 39:42, 47:50)], training[,c(57)], 
                method = "rf", 
                trControl = fitControl, 
                verbose = FALSE, 
                tuneLength = 5, importance = T) #5 verschiedene mtrys
#rfe = training mit rfe (zusätzliche Argumente: sizes = variablen anzahl zusätzlich volle variablen)
#rfeControl: zusätzlich zu trainControl

rfFit_mamm

varImp(rfFit_mamm)
#############################################
###plotting resampling performance
#############################################
trellis.par.set(caretTheme())
plot(rfFit)  
