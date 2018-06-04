ldr_SR <-read.csv("G:/Kili/data/ldr_SR.csv")
ldr_tbl <-read.csv("G:/Kili/data/ldr_tbl.csv")

plot(ldr_SR$SRanimals, ldr_tbl$surface_intensity_mean, col = ldr_SR$cat)
plot(ldr_SR$SRallplants, ldr_tbl$surface_intensity_mean, col = ldr_SR$cat)

# colnm <- colnames(ldr_SR)

for (i in seq(10, 75, 1)){
  plot(ldr_SR$SRanimals, ldr_SR[,i], col = ldr_SR$cat, ylab = colnames(ldr_SR[i]))
  plot(ldr_SR$SRallplants, ldr_SR[,i], col = ldr_SR$cat)
}
