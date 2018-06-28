library(raster)
library(rgdal)
shp <- shapefile("D:/Uni/Projekte/Kili/data/lidar_mid_poles.shp")
mrg_tbl <- get(load("D:/Uni/Projekte/Kili/data/dat_ldr_mrg.RData"))

layers <- c()
for (i in shp@data$PlotID){
  tmp <- mrg_tbl$layer[which(i == mrg_tbl$plotID)]
  if (i == "foc1"){
    tmp <- "2015/2016"
  }
  else if (tmp == "kili_campaign1_lidar_classified_2015"){
    tmp <- "2015"}
  else if (tmp == "kili_campaign2_lidar_classified_2016"){
    tmp <- "2016"}
  layers <- c(layers, tmp)
}

shp@data$layer <- layers
shapefile(shp, file = "D:/Uni/Projekte/Kili/data/lidar_mid_poles_layers.shp", overwrite = TRUE)

writeOGR(shp, dsn = "D:/Uni/Projekte/Kili/data/lidar_mid_poles_layers_OGR.shp", 
         driver = "ESRI Shapefile", layer = "lidar_mid_poles_layers_OGR")

