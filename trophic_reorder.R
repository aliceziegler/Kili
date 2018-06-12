trophic_tbl <- get(load(paste0(inpath, "../trophic_tbl.RData")))

trophic_tbl <- trophic_tbl[with(trophic_tbl, order(diet, Taxon)),]
trophic_tbl <- rbind(trophic_tbl[which(trophic_tbl$diet == "decomposer"), ], 
                     trophic_tbl[which(trophic_tbl$diet == "herbivore"), ], 
                     trophic_tbl[which(trophic_tbl$diet == "predator"), ], 
                     trophic_tbl[which(trophic_tbl$diet == "generalist"), ])
levels(trophic_tbl$diet) <- c("decomposer", "herbivore", "predator", "generalist")


save(trophic_tbl, file = "D://Uni/Projekte/Kili/data/trophic_tbl.RData")
