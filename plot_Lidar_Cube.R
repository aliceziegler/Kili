library(rPointDB)
library(mapview)
library(rgl)

#remotesensing <- RemoteSensing$new("http://localhost:8081", "user:password") # open local remote sensing database connection
remotesensing <- RemoteSensing$new("http://192.168.191.183:8081", "user:password") # open remote sensing database connection


pointdb <- remotesensing$lidar("kili")

#########################################
ext_cof <- extent_radius(x=313596.17, y=9637194.83, r=20) #cof4
points_cof <- pointdb$query(ext=ext_cof)
plot3d(points_cof$x, points_cof$y, points_cof$z, xlab = "coffee", ylab = "", zlab = "", col = rainbow(7)[points_cof$returnNumber])
legend3d("topright", legend = paste('Return', c('1', '2', '3', '4', '5', '6', '7')), pch = 16, col = rainbow(7), cex=1, inset=c(0.02))
