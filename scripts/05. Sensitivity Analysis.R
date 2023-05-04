# ---- load packages ----
library(sf)
library(biogeonetworks)
library(ggplot2)
library(tidyr)
library(broom)
library(rgdal)
library(tmap)
library(RColorBrewer)
# Maps
basinshp <- st_read("./data/sig data/basinshp")

fish.native <- as.data.frame(readRDS("./data/fish_native.rds"))
fish.native$X1.Basin.Name <- as.factor(fish.native$X1.Basin.Name)
fish.native$X6.Fishbase.Valid.Species.Name <- as.factor(fish.native$X6.Fishbase.Valid.Species.Name)


fish.all <- as.data.frame(readRDS("./data/fish_anthropocene.rds"))
fish.all$X1.Basin.Name <- as.factor(fish.all$X1.Basin.Name)
fish.all$X6.Fishbase.Valid.Species.Name <- as.factor(fish.all$X6.Fishbase.Valid.Species.Name)

library(lwgeom)
basinshp <- st_make_valid(basinshp)
wm <- st_read("./data/sig data/ne_50m_coastline.shp")

ref_native <- readInfomapTree("./data/infomapnetworks/fish.native.tree")
ref_all <- readInfomapTree("./data/infomapnetworks/fish.all.tree")

original_paper_colors <- readRDS("./data/NET_bioregions.lvl2")
ref_native$col.native.lvl2[ref_native$lvl2 == "1.1"] <- original_paper_colors$col.native.lvl2[original_paper_colors$lvl2 == "1.1"]
ref_native$col.native.lvl2[ref_native$lvl2 == "1.2"] <- original_paper_colors$col.native.lvl2[original_paper_colors$lvl2 == "1.2"]
ref_native$col.native.lvl2[ref_native$lvl2 == "1.3"] <- original_paper_colors$col.native.lvl2[original_paper_colors$lvl2 == "1.3"]
ref_native$col.native.lvl2[ref_native$lvl2 == "1.4"] <- original_paper_colors$col.native.lvl2[original_paper_colors$lvl2 == "1.4"]
ref_native$col.native.lvl2[ref_native$lvl2 == "2.1"] <- original_paper_colors$col.native.lvl2[original_paper_colors$lvl2 == "2.1"]
ref_native$col.native.lvl2[ref_native$lvl2 == "2.2"] <- original_paper_colors$col.native.lvl2[original_paper_colors$lvl2 == "2.2"]
ref_native$col.native.lvl2[is.na(ref_native$col.native.lvl2)] <- "#808080"

ref_native_sites <- getSiteTable(fish.native, site.field = "X1.Basin.Name",
                                 ref_native)
ref_all_sites <- getSiteTable(fish.all, site.field = "X1.Basin.Name",
                              ref_all)

bioregions.lvl2 <- readRDS("./data/bioregions_lvl2")[1:6, ]
bioregions.lvl2$main.basin <- c("Mekong", "Congo", "Shatt.al.Arab",
                                "Murray.Darling", "Amazon", "Mississippi")

find_main_basin <- function(network)
{
  if(length(which(network$Codelength == max(network$Codelength))) == 1)
  {
    resul <- data.frame(basin = network$Name,
                        main.basin = network$Name[which(network$Codelength == max(network$Codelength))])          
  } else
  {
    resul <- data.frame(basin = network$Name,
                        main.basin = paste0(network$Name[which(network$Codelength == max(network$Codelength))],
                                            collapse = ","))
  }
  return(resul)
}

basin_nat <- plyr::ddply(ref_native_sites, "lvl2",
                         .fun = find_main_basin
)
basin_all <- plyr::ddply(ref_all_sites, "lvl1",
                         .fun = find_main_basin
)

sens_df_nat <- data.frame(basin = ref_native_sites$Name,
                          main_basin = basin_nat$main.basin[match(ref_native_sites$Name,
                                                                  basin_nat$basin)])
sens_df_all <- data.frame(basin = ref_all_sites$Name,
                          main_basin = basin_all$main.basin[match(ref_all_sites$Name,
                                                                  basin_all$basin)])
sim <- 10
for (i in 1:sim) {
  seed <- as.integer(Sys.time())
  # Seed used in the final results 1663450780
  
  # Map equation clustering
  system(paste0("./infomap/Infomap_2_6_0 ./data/pajek/fish.native.net ./data/infomapnetworks/sensitivity_analysis --tree --ftree --clu -N 100 -s ",
                seed))
  system(paste0("./infomap/Infomap_2_6_0 ./data/pajek/fish.all.net ./data/infomapnetworks/sensitivity_analysis --tree --ftree --clu -N 100 -s ",
                seed))
  
  native.net <- readInfomapTree("./data/infomapnetworks/sensitivity_analysis/fish.native.tree",
                                replace.leaf.names = FALSE)
  all.net <- readInfomapTree("./data/infomapnetworks/sensitivity_analysis/fish.all.tree",
                             replace.leaf.names = FALSE)
  
  basinshp$native_lv2 <- native.net$lvl2[match(basinshp$BasinName, native.net$Name)]
  basinshp$col_native_lv2 <- brewer.pal(7, "Set2")[as.numeric(as.factor(basinshp$native_lv2))]
  basinshp$all_lv1 <- all.net$lvl1[match(basinshp$BasinName, all.net$Name)]
  basinshp$col_all_lv1 <- brewer.pal(7, "Set2")[as.numeric(as.factor(basinshp$all_lv1))]
  
  pnat <- ggplot() +
    geom_sf(data = basinshp, aes(fill = col_native_lv2))+
    scale_fill_identity() +
    ggtitle(paste0("Native, seed = ", seed))
  
  pall <- ggplot() +
    geom_sf(data = basinshp, aes(fill = col_all_lv1))+
    scale_fill_identity() +
    ggtitle(paste0("Anthropocene, seed = ", seed))
  
  png(paste0("outputs/sensitivity_analysis/run_", i, ".png"),
      height = 1200, width = 1200)
  egg::ggarrange(pnat, pall,
                 newpage = FALSE,
                 nrow = 2,
                 label.args = list(gp = grid::gpar(font = 4, cex = 3)))
  dev.off()
  
  basin_nat <- getSiteTable(fish.native, site.field = "X1.Basin.Name",
                            native.net)
  basin_all <- getSiteTable(fish.all, site.field = "X1.Basin.Name",
                            all.net)
  
  basin_nat <- plyr::ddply(basin_nat, "lvl2",
                           .fun = find_main_basin
  )
  basin_all <- plyr::ddply(basin_all, "lvl1",
                           .fun = find_main_basin
  )
  
  sens_df_nat[, paste0("sim", i)] <- basin_nat$main.basin[match(sens_df_nat$basin,
                                                                basin_nat$basin)]
  sens_df_all[, paste0("sim", i)] <- basin_all$main.basin[match(sens_df_all$basin,
                                                                basin_all$basin)]
  
  
}
saveRDS(sens_df_nat, "data/sensitivity_native.RDS")
saveRDS(sens_df_all, "data/sensitivity_all.RDS")

a <- sens_df_nat[, grep("sim", colnames(sens_df_nat))]
comps <- get_pairwise_membership(a)

pw_cluster_comps <- t(combn(seq_len(ncol(comps)), 2))
all_conf_matrices <- lapply(seq_len(nrow(pw_cluster_comps)), function(x) {
  get_confusion_matrix(comps[, pw_cluster_comps[x, 1]],
                       comps[, pw_cluster_comps[x, 2]])
})
names(all_conf_matrices) <- apply(pw_cluster_comps, 1, paste, collapse = "_")

RI_native <- vapply(all_conf_matrices,
             rand_index,
             numeric(1))
JI_native <- vapply(all_conf_matrices,
                    jaccard_index,
                    numeric(1))



a <- sens_df_all[, grep("sim", colnames(sens_df_all))]
system.time(comps <- get_pairwise_membership(a))

pw_cluster_comps <- t(combn(seq_len(ncol(comps)), 2))
all_conf_matrices <- lapply(seq_len(nrow(pw_cluster_comps)), function(x) {
  get_confusion_matrix(comps[, pw_cluster_comps[x, 1]],
                       comps[, pw_cluster_comps[x, 2]])
})
names(all_conf_matrices) <- apply(pw_cluster_comps, 1, paste, collapse = "_")

RI_all <- vapply(all_conf_matrices,
                    rand_index,
                    numeric(1))

boxplot(RI_all)


bioregions.lvl2

nb_basin_ref <- plyr::count(sens_df_nat$main_basin)
nb_basin_ref <- nb_basin_ref[nb_basin_ref$x %in% bioregions.lvl2$main.basin, ]

nb_basin_sim <- plyr::count(sens_df_nat$sim1)
nb_basin_sim <- nb_basin_sim[nb_basin_sim$x %in% bioregions.lvl2$main.basin, ]

table(sens_df_nat$main_basin, sens_df_nat$sim1)

basinshp

a <- lapply(sens_df_all[grep("sim", colnames(sens_df_all))],
            function(x) {
              plyr::count(x)
            }
)

a <- data.table::rbindlist(a)
library(dplyr)
a <- a %>%
  arrange(freq) %>%               # sort your dataframe
  mutate(x = factor(x, unique(x)))

a$x <- as.factor(a$x)
ggplot(a, aes(x = x, y = freq)) +
  geom_point() +
  geom_boxplot()

results_nat <- data.frame(sens_df_nat[, 1:2],
                          n_identical = sapply(1:nrow(sens_df_nat), function (x) {
                            length(which(sens_df_nat[x, 3:ncol(sens_df_nat)] == sens_df_nat[x, 2]))
                          }),
                          n_change = sapply(1:nrow(sens_df_nat), function (x) {
                            length(which(sens_df_nat[x, 3:ncol(sens_df_nat)] != sens_df_nat[x, 2]))
                          }))
results_nat$pc_identical <- results_nat$n_identical / sim

results_nat <- results_nat[which(results_nat$main_basin %in% bioregions.lvl2$main.basin), ]

ggplot(results_nat, aes(x = main_basin, y = pc_identical)) +
  geom_boxplot() + coord_flip() + geom_point() +
  scale_y_continuous(labels=scales::percent, limits = c(0, 1)) +
  xlab("Cluster") + ylab("Frequency of detection")
