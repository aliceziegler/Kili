# This script file contains usage examples to calculate indices of LiDAR data with rPointDB.

# **** init ***

library(data.table)
library(rPointDB)

# help("rPontDB-package", package="rPointDB") # show main manual page
# help(package="rPointDB") # show all manual pages
# packageDescription("rPointDB") # show package Description
# packageVersion("rPointDB") # show package version

#remotesensing <- RemoteSensing$new("http://localhost:8081") # open local remote sensing database connection
remotesensing <- RemoteSensing$new("http://192.168.191.183:8081") # open remote sensing database connection

#remotesensing$web() #open web interface in browser

remotesensing$lidar_layers() #query names of lidar layers

pointdb <- remotesensing$lidar("kili") #open one lidar layer
#fs <-  pointdb$process_functions() #get all lidar index functions


# **** util ***

subset_arg_poi_square <- function(pois, group, edge=10) {
  poisFull <- paste0(group, "/", pois)
  poisFinal <- paste0(poisFull, collapse="&")
  r <- paste0("square(poi(", poisFinal, "),", edge, ")")
  return(r)
}

# **** processing ***

process_layer <- function(layer_name) {


  pointdb <- remotesensing$lidar(layer_name) #open one lidar layer

  poi_group <- layer_name

  pois <- remotesensing$poi_group(poi_group)

  poi_names <- pois$name

  subsets <- subset_arg_poi_square(pois=poi_names, group=poi_group, edge=10)

  #script <- c("BE_H_MAX","BE_H_MEAN")
  script <- pointdb$process_functions()$name

  df <- pointdb$process(subset=subsets, script=script)
}



layer_names <- c("alb", "hai", "sch")

result_list <- lapply(layer_names, process_layer)

df <- rbindlist(result_list)


cor(df$BE_H_MAX, df$BE_H_MEAN, use="complete.obs")

df$subset <- NULL
corMatrix <- cor(as.matrix(df), use="pairwise.complete.obs")
#corMatrix <- cor(df$BE_ELEV_ASPECT, as.matrix(df), use="pairwise.complete.obs")

levelplot(abs(corMatrix), col.regions=heat.colors(100), scales=list(x=list(rot=90)))


# ######################################
# library(rPointDB)
# 
# #   **** Remote Sensing base *****
# 
# # help("rPontDB-package", package="rPointDB") # show main manual page
# # help(package="rPointDB") # show all manual pages
# 
# #remotesensing <- RemoteSensing$new("http://localhost:8081") # open local remote sensing database connection
# remotesensing <- RemoteSensing$new("http://192.168.191.183:8081") # open remote sensing database connection
# 
# #remotesensing$web() #open web interface in browser
# 
# remotesensing$lidar_layers() #query names of lidar layers
# 
# poi_groups <- remotesensing$poi_groups() #query names of POI-groups
# 
# poi_group_kili <- remotesensing$poi_group("kili") #get POIs of one POI-group
# 
# #pos <- c(305667, 9641386) # position of cof1
# #pos <- c(328402.84375, 9636951) # gra2
# #ext <- extent_radius(x=pos[1], y=pos[2], r=50) # rectangle around pos with radius 50 meter
# #ext <- extent_radius(x=pos[1], y=pos[2], r=10) # rectangle around pos with radius 10 meter
# 
pos_gra2 <- remotesensing$poi("kili", "gra2") # get one POI
pos_flm2 <- remotesensing$poi("kili", "flm2")
ext <- extent_radius(x=pos_flm2$x, y=pos_flm2$y, r=50) # rectangle around pos with radius 50 meter
# 
# 
# 
# 
#   **** LiDAR *****


pointdb <- remotesensing$lidar("kili") #open one lidar layer

pointdb$get_info() # get meta data of this lidar layer

df <- pointdb$query(ext=ext) # get data.frame with point data of rectangle

library(rgl)
plot3d(df$x, df$y, df$z, zlim = c(1750, 1800)) # visualise LiDAR points in interactive 3d view

# 
# layer <- pointdb$query_raster(type="dtm", ext=ext) # processing: RasterLayer of digital terrain model
# plot(layer) # view layer
# 
# layer <- pointdb$query_raster(type="slope", ext=ext) # processing: RasterLayer of slope of ground surface
# plot(layer) # view layer
# 
# layer <- pointdb$query_raster(type="dsm", ext=ext) # processing: RasterLayer of digital surface model
# plot(layer) # view layer
# 
# layer <- pointdb$query_raster(type="chm", ext=ext) # processing: RasterLayer of canopy height model
# plot(layer) # view layer
# 
# library(mapview)
# plainview(layer) # visualise processed raster
# 
# library(rgl)
# visualise_raster(as.matrix(layer)) # visualise processed raster in interactive 3d surface-view
# 
# df <- pointdb$query(ext=ext, filter="return=2", columns="x,y,z", normalise="extremes") # get all points of second return, with columns x,y,z and removed outliers
# plot3d(df$x, df$y, df$z)


