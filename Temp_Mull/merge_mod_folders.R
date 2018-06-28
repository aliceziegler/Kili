# Description: 
# Author: Alice Ziegler
# Date: 2018-03-07 12:08:34
# to do: !!rmse normalisieren auf SD oder mean
rm(list=ls())
##############ACHTUNG: bei plotten hochgestellte zwei in Spaltenüberschrift von Dataframe...unschön!
########################################################################################
###Presettings
########################################################################################
#Packages: 



#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "jun18_50m/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}

###folder with models: 
list.files(inpath, pattern = "nm_resp.RData", full.names = T, recursive = T)
