# Description: 
# Author: Alice Ziegler
# Date: 2018-05-14 08:33:55
# to do: 
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 
library(raster)
library(mapview)
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "/"
inpath <- "E:/Projekte/Kili/data/"
outpath <- paste0("../data/", sub)

########################################################################################
###Settings
########################################################################################

########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################

poles <- shapefile(paste0(inpath, "PlotPoles_WGS1984_mod_20140807_final_lidar_2.shp"))
mapview(poles, label = poles$PlotID)
