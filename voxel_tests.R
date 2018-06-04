library(rPointDB)
library(mapview)
library(rgl)

#remotesensing <- RemoteSensing$new("http://localhost:8081", "user:password") # open local remote sensing database connection
remotesensing <- RemoteSensing$new("http://192.168.191.183:8081", "user:password") # open remote sensing database connection


pointdb <- remotesensing$lidar("kili")
ext_cof <- extent_radius(x=313596.17, y=9637194.83, r=50) #cof4
points_cof <- pointdb$query(ext=ext_cof)
points_cof$landuse = "cof"


ext_flm <- extent_radius(x=315917.97, y=9644852.31, r=50)# flm2 
points_flm <- pointdb$query(ext=ext_flm)
points_flm$landuse = "flm"
hist(points_flm$z, main = "Forest Lower Mountain", xlab = "elevation")

pointdb <- remotesensing$lidar("kili2")
ext_fer <- extent_radius(x=310263.39, y=9659343.53, r=50) #fer0
points_fer <- pointdb$query(ext=ext_fer)
hist(points_fer$z, main = "Erica Forest 0", xlab = "elevation")


points_fer$landuse = "fer"
ext_hel <- extent_radius(x=313778.74, y=9658122.62, r=50) #hel2 
points_hel <- pointdb$query(ext=ext_hel)
points_hel$landuse = "hel"
hist(points_hel$z, main = "Helicrysum 2", xlab = "elevation")


plot((density(points_cof$z)), xlab = "elevation", main = "Coffee", xlim = c(min(points_cof$z),min(points_cof$z)+80), ylim = c(0, 0.16))
plot((density(points_flm$z)), xlab = "elevation", main = "Forest Lower Mountain", xlim = c(min(points_flm$z),min(points_flm$z)+80), ylim = c(0, 0.16))
plot((density(points_fer$z)), xlab = "elevation", main = "Erica Forest", xlim = c(min(points_fer$z),min(points_fer$z)+80), ylim = c(0, 0.16))
plot((density(points_hel$z)), xlab = "elevation", main = "Helichrysum", xlim = c(min(points_hel$z),min(points_hel$z)+80), ylim = c(0, 0.16))



#plot3d(points$x, points$y, points$z)

plot(density(points$z, n=20))

# dtm <- pointdb$query_raster(type="dtm2", ext=ext)
# 
# #plot(dtm)
# plainview(dtm)
# visualise_raster(as.matrix(dtm))
# 
# 
# dsm <- pointdb$query_raster(type="dsm2", ext=ext)
# 
# #plot(dsm)
# plainview(dsm)
# #visualise_raster(as.matrix(dsm))
# 
# 
# chm <- pointdb$query_raster(type="chm2", ext=ext)
# 
# #plot(chm)
# plainview(chm)
# #visualise_raster(as.matrix(chm))


voxel <- pointdb$query_raster(type="voxel", ext=ext_fer) #does not show in RStudio --> open in web browser (in RStudio button "show in new window")
cubeView(voxel)

# 
# pointdb <- remotesensing$lidar("hai")
# ext <- extent_radius(x=599798.5, y=5657795, r=200)
# voxel <- pointdb$query_raster(type="voxel", ext=ext)
# cubeView(voxel)

pl <- viridis::plasma(1000, alpha = 1, begin = 0, end = 1)
col <- c("#000000FF", pl[100:1000])

