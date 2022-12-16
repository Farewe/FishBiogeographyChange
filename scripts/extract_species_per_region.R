library(sf)

fish.all <- readRDS("./data/fish_db_with_migratory.RDS")

basinshp <- st_read("./data/sig data/basinshpwithbaikal")

native.net <- readRDS("./data/nativenetwork.RDS")

bioregions.lvl2 <- unique(native.net[, c("clvl2", "col.native.lvl2")])
bioregions.lvl2$name <-  c("Sino-Oriental",
                           "Ethiopian",
                           "Palearctic",
                           "Australian",
                           "Neotropical",
                           "Nearctic",
                           "CA transition zone",
                           "Minor clusters")
native.net$bioregion.lvl2 <- bioregions.lvl2$name[match(native.net$clvl2,
                                                        bioregions.lvl2$clvl2)]

sp_per_region <- native.net[which(native.net$nodetype == "species"), c("Name", "bioregion.lvl2")]



head(fish.all)
fish.all$bioregion.lvl2 <- native.net$bioregion.lvl2[match(fish.all$X1.Basin.Name,
                                         native.net$Name)]
fish.all <- fish.all[-which(fish.all$X3.Native.Exotic.Status != "native"), ]

sp_region_table <- table(fish.all$X6.Fishbase.Valid.Species.Name,
                         fish.all$bioregion.lvl2)
sp_region_table_bin <- sp_region_table
sp_region_table_bin[sp_region_table_bin > 0 ] <- 1

sp_region_occ <- rowSums(sp_region_table_bin)
sp_more_one_reg <- sp_region_occ[sp_region_occ > 1]


function(x, row) {
  
}
"Ablennes hians"
a <- sp_region_table[1:7, ]
region_list <- apply(sp_region_table, 1, function(row) {
  regs <- names(which(row != 0))
  regs[order(regs, row[which(row != 0)], decreasing = TRUE)]
  })

library(data.table)
obs1 <- data.table(t(obs1))
obs2 <- data.table(t(obs2))
df <- rbindlist(list(obs1,obs2),fill=T)

region_df <- rbind.fill(lapply(region_list,function(y){as.data.frame(t(y),stringsAsFactors=FALSE)}))
rownames(region_df) <- names(region_list)

colnames(region_df) <- c("Main native region", 
                         "Native region 2",
                         "Native region 3",
                         "Native region 4", 
                         "Native region 5",
                         "Native region 6",
                         "Native region 7")
library(xlsx)
write.xlsx2(region_df, "./data/native_regions_with_migratory_sp.xlsx", sheetName = "Native regions",
            col.names = TRUE, row.names = TRUE, append = FALSE)
