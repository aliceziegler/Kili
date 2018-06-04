###settings
inpath <- "G:/Kili/data/"
outpath <- "G:/Kili/out/"

###boxplot Plotverteilung von LiDAR Parametern
ldr_tbl <- read.csv(paste0(inpath, "ldr_tbl.csv"), header = T, sep = ",")
ldr_tbl$landuse <- substr(ldr_tbl$plotID, 1, 3)
ldr_tbl$landuse <- factor(ldr_tbl$landuse, levels = c("mai", "sav", "cof", "hom", "gra", "flm", "fod", 
                                          "foc", "fpo", "fpd", "fed", "fer", "hel"))
#foc1 aus alter befliegung fliegt raus, weil doppelt beflogen
for (i in seq(2,ncol(ldr_tbl)-1)){
  png(paste0(outpath, colnames(ldr_tbl[i]), "_plot.png"), width = 800, height = 600)
  boxplot(split(ldr_tbl[,i], ldr_tbl$landuse), main = colnames(ldr_tbl[i]))
  dev.off()
}

#Reihenfolge von unten nach oben: 
#MAI-SAV-COF-HOM-GRA-FLM-FOD-FOC-FPO/FPD-FED-FER-HEL 
i <- 2
