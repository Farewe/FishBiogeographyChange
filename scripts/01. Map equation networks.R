# ---- load packages ----
library(sf)
library(biogeonetworks)
library(ggplot2)
library(tidyr)
library(broom)
library(rgdal)
library(tmap)
library(RColorBrewer)


# ---- load datasets ----
# Fish datasets
# Native without extinctions
fish.native <- as.data.frame(readRDS("./data/fish_native.rds"))
fish.native$X1.Basin.Name <- as.factor(fish.native$X1.Basin.Name)
fish.native$X6.Fishbase.Valid.Species.Name <- as.factor(fish.native$X6.Fishbase.Valid.Species.Name)
# Native with extinctions
fish.native.ext <- as.data.frame(readRDS("./data/fish_native_extinctions.rds"))
fish.native.ext$X1.Basin.Name <- as.factor(fish.native.ext$X1.Basin.Name)
fish.native.ext$X6.Fishbase.Valid.Species.Name <- as.factor(fish.native.ext$X6.Fishbase.Valid.Species.Name)
# Introduced only
fish.intro <- as.data.frame(readRDS("./data/fish_intro_only.rds"))
fish.intro$X1.Basin.Name <- as.factor(fish.intro$X1.Basin.Name)
fish.intro$X6.Fishbase.Valid.Species.Name <- as.factor(fish.intro$X6.Fishbase.Valid.Species.Name)
# Introduced + extinctions
fish.all <- as.data.frame(readRDS("./data/fish_anthropocene.rds"))
fish.all$X1.Basin.Name <- as.factor(fish.all$X1.Basin.Name)
fish.all$X6.Fishbase.Valid.Species.Name <- as.factor(fish.all$X6.Fishbase.Valid.Species.Name)

# Calculate richness per basin
dbcontin <- table(fish.native$X6.Fishbase.Valid.Species.Name, fish.native$X1.Basin.Name)
# Initialisation with anthropocene richness
richness <- tibble(basin = levels(fish.all$X1.Basin.Name),
                   anthro = colSums(table(fish.all$X6.Fishbase.Valid.Species.Name,
                                          fish.all$X1.Basin.Name)))

# Native richness
richness$native <- colSums(table(fish.native$X6.Fishbase.Valid.Species.Name,
                                 fish.native$X1.Basin.Name))[
  match(richness$basin, levels(fish.native$X1.Basin.Name))
]
richness$native[which(is.na(richness$native))] <- 0

# Introduced richness
richness$introduced <- colSums(table(fish.intro$X6.Fishbase.Valid.Species.Name,
                                     fish.intro$X1.Basin.Name))[
  match(richness$basin, levels(fish.intro$X1.Basin.Name))
]
richness$introduced[which(is.na(richness$introduced))] <- 0

# Native - extinct richness
richness$native.ext <- colSums(table(fish.native.ext$X6.Fishbase.Valid.Species.Name,
                                     fish.native.ext$X1.Basin.Name))[
  match(richness$basin, levels(fish.native.ext$X1.Basin.Name))
]
richness$native.ext[which(is.na(richness$native.ext))] <- 0

rich_matrix <- as.matrix(richness[,2:5])
rownames(rich_matrix) <- richness$basin
apply(rich_matrix, 2, mean)

rich_long <- pivot_longer(richness, cols = 2:5)
ggplot(rich_long, aes(x = name, y = value)) +
geom_violin()

# Maps
basinshp <- st_read("./data/sig data/basinshp")

library(lwgeom)
basinshp <- st_make_valid(basinshp)
basinshp <- basinshp[-which(!(basinshp$BasinName %in% richness$basin)), ]
wm <- st_read("./data/sig data/ne_50m_coastline.shp")

# Adding richness to map dataset
basinshp$richness.native <- richness$native[match(basinshp$BasinName,
                                                  richness$basin)]
basinshp$richness.anthro <- richness$anthro[match(basinshp$BasinName,
                                                  richness$basin)]
basinshp$richness.intro <- richness$introduced[match(basinshp$BasinName,
                                                  richness$basin)]
basinshp$richness.native.ext <- richness$native.ext[match(basinshp$BasinName,
                                                  richness$basin)]

# Difference richness native - anthropocene
basinshp$richness.diff.anthro <- basinshp$richness.anthro - basinshp$richness.native
basinshp$richness.pdiff.anthro <- basinshp$richness.diff.anthro / basinshp$richness.native
basinshp$richness.diff.intro <- basinshp$richness.intro - basinshp$richness.native
basinshp$richness.pdiff.intro <- basinshp$richness.diff.intro / basinshp$richness.native
basinshp$richness.diff.native.ext <- basinshp$richness.native.ext - basinshp$richness.native
basinshp$richness.pdiff.native.ext <- basinshp$richness.diff.native.ext / basinshp$richness.native


# ---- Map prerequisite ----
# Graticule
gr <- sf::st_graticule(lat = c(-89.9,seq(-80,80,20), 89.9))
gr <- st_transform_proj(gr,
                        crs = "+proj=wintri +datum=WGS84 +no_defs +over")


# Continents
wm <- st_read("./data/sig data/ne_50m_land.shp")
wm <- st_transform_proj(wm,
                        crs = "+proj=wintri +datum=WGS84 +no_defs +over")



# ---- Richness maps ----
basinshp_wt <- st_transform_proj(basinshp,
                                 crs = "+proj=wintri +datum=WGS84 +no_defs +over")
thememaps <- theme(legend.key.size = unit(2, "cm"),
                   legend.text = element_text(size = 18),
                   axis.text = element_text(size = 18),
                   axis.title = element_text(size = 18),
                   legend.title = element_text(size = 18),
                   panel.grid = element_blank(),
                   line = element_blank(),
                   rect = element_blank())

# RICHNESS NATIVE
prnative <- ggplot() +
  geom_sf(data = gr, color = 'grey',
          size = .5) +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9),
          size = .2) +
  geom_sf(data = basinshp_wt, aes(fill = richness.native),
          size = .05) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_gradientn(colours = brewer.pal(7, "Blues"),
                       trans = "log",
                       name = "Species richness\n(log scale)\n",
                       na.value = grey(.33),
                       breaks = c(1, 10, 50, 400, 2000)) +
  thememaps
ggsave("./outputs/richness_native.png",
       prnative,
       device = "png")



# RICHNESS ANTHROPOCENE
pranthro <-   ggplot() +
  geom_sf(data = gr, color = 'grey',
          size = .5) +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9),
          size = .2) +
  geom_sf(data = basinshp_wt, aes(fill = richness.anthro),
          size = .05) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_gradientn(colours = brewer.pal(7, "PuRd"),
                       trans = "log",
                       name = "Species richness\n(log scale)\n",
                       na.value = grey(.33),
                       breaks = c(1, 10, 50, 400, 2000)) +
  thememaps
ggsave("./outputs/richness_anthropocene.png",
       pranthro,
       device = "png")


# Difference native - introduced species
max(basinshp$richness.diff.intro) - 1 / 6
pdiff_intro <- ggplot() +
  geom_sf(data = gr, color = 'grey',
          size = .5) +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9),
          size = .2) +
  geom_sf(data = basinshp_wt, aes(fill = richness.diff.intro),
          size = .05) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_gradientn(colours = c("white",
                                   brewer.pal(7, "Reds")[2:7]),
                       name = "Nb. introduced\nspecies",
                       na.value = grey(.33),
                       breaks = c(0, 1, 12, 24, 36,
                                  48, 60, 72),
                       labels = c(0, "", 12, 24, 36, 46, 60, 72)) +
  thememaps

ggsave("./outputs/richness_diff_intro.png",
       pdiff_intro,
       width = 10,
       height = 6,
       device = "png")

# Difference in extinctions
min(basinshp$richness.diff.native.ext)
pdiff_ext <- ggplot() +
  geom_sf(data = gr, color = 'grey',
          size = .5) +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9),
          size = .2) +
  geom_sf(data = basinshp_wt, aes(fill = -richness.diff.native.ext),
          size = .05) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_gradientn(colours = c("white",
                                   brewer.pal(9, "Purples")[4:9]),
                       name = "Nb. extinctions",
                       na.value = grey(.33),
                       breaks = c(0, 1, 2, 4, 6, 8, 10, 12),
                       labels = c(0, "", 2, 4, 6, 8, 10, "")) +
  thememaps
ggsave("./outputs/richness_diff_extinct.png",
       pdiff_ext,
       width = 10,
       height = 6,
       device = "png")


png("./outputs/figure_diff_richness.png",
    h = 1800, w = 1000)
cowplot::plot_grid(pranthro, pdiff_ext, pdiff_intro,
          ncol = 1, align = "hv",
          labels = list("a. Anthropocene species richness",
                        "b. Number of extinctions per basin",
                        "c. Number of introductions per basin"),
          label_size = 22)
dev.off()
# ---- Map Equation ----
# Writing native network
fish.native <- droplevels(fish.native)
writePajek(fish.native,
           site.field = "X1.Basin.Name",
           species.field = "X6.Fishbase.Valid.Species.Name",
           filename = "./data/pajek/fish.native.net")

# Writing anthropocene network
fish.all <- droplevels(fish.all)
writePajek(fish.all,
           site.field = "X1.Basin.Name",
           species.field = "X6.Fishbase.Valid.Species.Name",
           filename = "./data/pajek/fish.all.net")

# Writing introduced network
fish.intro <- droplevels(fish.intro)
writePajek(fish.intro,
           site.field = "X1.Basin.Name",
           species.field = "X6.Fishbase.Valid.Species.Name",
           filename = "./data/pajek/fish.intro.net")

# Writing extinction network
fish.native.ext <- droplevels(fish.native.ext)
writePajek(fish.native.ext,
           site.field = "X1.Basin.Name",
           species.field = "X6.Fishbase.Valid.Species.Name",
           filename = "./data/pajek/fish.native.ext.net")

# Generate a random seed to reproduce predictions
# seed <- as.integer(Sys.time())
# Seed used in the final results 1663450780

# Map equation clustering
# system(paste0("./infomap/Infomap_2_6_0 ./data/pajek/fish.native.net ./data/infomapnetworks/ --tree --ftree --clu -N 1000 -s ",
#        seed))
# system(paste0("./infomap/Infomap_2_6_0 ./data/pajek/fish.all.net ./data/infomapnetworks/ --tree --ftree --clu -N 1000 -s ",
#               seed))
# system(paste0("./infomap/Infomap_2_6_0 ./data/pajek/fish.intro.net ./data/infomapnetworks/ --tree --ftree --clu -N 1000 -s ",
#        seed))
# system(paste0("./infomap/Infomap_2_6_0 ./data/pajek/fish.native.ext.net ./data/infomapnetworks/ --tree --ftree --clu -N 1000 -s ",
#        seed))



# ---- read outputs from map equation ----
native.net <- readInfomapTree("./data/infomapnetworks/fish.native.tree")
all.net <- readInfomapTree("./data/infomapnetworks/fish.all.tree")
intro.net <- readInfomapTree("./data/infomapnetworks/fish.intro.tree")
native.ext.net <- readInfomapTree("./data/infomapnetworks/fish.native.ext.tree")

# b <- st_as_sf(basinshp)
# b$native_lv2 <- native.net$lvl2[match(b$BasinName, native.net$Name)]
# b$col_native_lv2 <- brewer.pal(7, "Set2")[as.numeric(as.factor(b$native_lv2))]
# b$all_lv1 <- all.net$lvl1[match(b$BasinName, all.net$Name)]
# b$col_all_lv1 <- brewer.pal(7, "Set2")[as.numeric(as.factor(b$all_lv1))]
# 
# ggplot() +
#   geom_sf(data = b, aes(fill = col_native_lv2))+
#   scale_fill_identity()
# 
# ggplot() +
#   geom_sf(data = b, aes(fill = col_all_lv1))+
#   scale_fill_identity()

native.net$nodetype <- NA
native.net$nodetype[native.net$Name %in% fish.native[, "X1.Basin.Name"]] <- "site"
native.net$nodetype[native.net$Name %in% fish.native[, "X6.Fishbase.Valid.Species.Name"]] <- "species"

all.net$nodetype <- NA
all.net$nodetype[all.net$Name %in% fish.all[, "X1.Basin.Name"]] <- "site"
all.net$nodetype[all.net$Name %in% fish.all[, "X6.Fishbase.Valid.Species.Name"]] <- "species"

intro.net$nodetype <- NA
intro.net$nodetype[intro.net$Name %in% fish.intro[, "X1.Basin.Name"]] <- "site"
intro.net$nodetype[intro.net$Name %in% fish.intro[, "X6.Fishbase.Valid.Species.Name"]] <- "species"

native.ext.net$nodetype <- NA
native.ext.net$nodetype[native.ext.net$Name %in% fish.native.ext[, "X1.Basin.Name"]] <- "site"
native.ext.net$nodetype[native.ext.net$Name %in% fish.native.ext[, "X6.Fishbase.Valid.Species.Name"]] <- "species"



# Site table
site.native <- getSiteTable(fish.native, site.field = "X1.Basin.Name", network = native.net)
site.all <- getSiteTable(data.frame(fish.all), site.field = "X1.Basin.Name", network = all.net)
site.intro <- getSiteTable(fish.intro, site.field = "X1.Basin.Name", network = intro.net)
site.native.ext <- getSiteTable(fish.native.ext, site.field = "X1.Basin.Name", network = native.ext.net)

site.native$Surf_area <- basinshp$Surf_area[match(site.native$Name,
                                                  basinshp$BasinName)]
site.all$Surf_area <- basinshp$Surf_area[match(site.all$Name,
                                               basinshp$BasinName)]
site.intro$Surf_area <- basinshp$Surf_area[match(site.intro$Name,
                                                 basinshp$BasinName)]
site.native.ext$Surf_area <- basinshp$Surf_area[match(site.native.ext$Name,
                                                      basinshp$BasinName)]

# Function to compute total basin surface
countsurf <- function(site_table,
                      lvl)
{
  if(any(is.na(site_table[, lvl])))
  {
    site_table <- site_table[-which(is.na(site_table[, lvl])), ]
  }
  a <- plyr::count(site_table[, lvl])
  a$surface <- NA
  for (i in 1:nrow(a))
  {
    a$surface[i] <- sum(site_table$Surf_area[which(site_table[, lvl] == a$x[i])])
  }
  return(a)
}

# ---- Level 1 number of clusters ----
# Minor cluster color
minor.color <- grey(.43)
tiny.color <- grey(.67)


# Major limit: limit (number of sites or site area) for major clusters
lim <- 1e6
# Minor limit: lower limit to distinguish small clusters from tiny clusters
# 0 here to only have one set
minor.lim <- 0

## NATIVE
# native.lvl1 <- plyr::count(site.native$lvl1)
native.lvl1 <- countsurf(site.native,
                         "lvl1")

# Defining major and minor clusters
native.net$cluster.type.lvl1 <- "tiny"
native.net$cluster.type.lvl1[which(native.net$lvl1 %in%
                                     native.lvl1$x[which(native.lvl1$surface>= minor.lim &
                                                           native.lvl1$surface< lim)])] <- "minor"

native.net$cluster.type.lvl1[which(native.net$lvl1 %in%
                                     native.lvl1$x[which(native.lvl1$surface>= lim)])] <- "major"
if(minor.lim == 0)
{
  native.net$cluster.type.lvl1[which(native.net$cluster.type.lvl1 == "tiny")] <- "minor"
}

# Excluding individual basins from cluster list
native.net$cluster.type.lvl1[which(as.character(native.net$lvl1) == as.character(native.net$Name))] <- "minor"


# Number of species & sites per cluster
plyr::count(native.net$cluster.type.lvl1)


# Color attribution
native.net <- as.data.frame(native.net)
native.net <- attributeColors(native.net, lvl = "lvl1",
                              # nb.max.colors = length(which(native.lvl1$freq >= lim)),
                              nb.max.colors = length(which(native.lvl1$surface >= lim)),
                              palette = "Accent",
                              colname = "col.native.lvl1",
                              db = fish.native,
                              site.field = "X1.Basin.Name")

# Manual change to colors
native.net$col.native.lvl1[native.net$cluster.type.lvl1 == "minor"] <- minor.color
native.net$col.native.lvl1[native.net$cluster.type.lvl1 == "tiny"] <- tiny.color


## ANTHROPOCENE

# Defining major and minor clusters
all.lvl1 <- countsurf(site.all,
                      "lvl1")
all.net$cluster.type.lvl1 <- "tiny"
all.net$cluster.type.lvl1[which(all.net$lvl1 %in%
                                  all.lvl1$x[which(all.lvl1$surface>= minor.lim &
                                                     all.lvl1$surface< lim)])] <- "minor"

all.net$cluster.type.lvl1[which(all.net$lvl1 %in%
                                  all.lvl1$x[which(all.lvl1$surface>= lim)])] <- "major"
if(minor.lim == 0)
{
  all.net$cluster.type.lvl1[which(all.net$cluster.type.lvl1 == "tiny")] <- "minor"
}
# Excluding individual basins from cluster list
all.net$cluster.type.lvl1[which(as.character(all.net$lvl1) == as.character(all.net$Name))] <- "minor"


plyr::count(all.net$cluster.type.lvl1)

# Color attribution
all.net <- as.data.frame(all.net)

all.net$col.all.lvl1 <- minor.color

# level 1
original_paper_colors <- readRDS("./data/NET_bioregions.lvl2")
# Anthropocenian
all.net$col.all.lvl1[all.net$lvl1 == 1] <- strtrim(viridisLite::inferno(20)[9], 7)
# Neotropical
all.net$col.all.lvl1[all.net$lvl1 == 2] <- original_paper_colors$col.native.lvl2[original_paper_colors$lvl2 == "2.1"]
# Neo-Oriental
all.net$col.all.lvl1[all.net$lvl1 == 3] <- strtrim(viridisLite::inferno(20)[17], 7)
# Ethiopian
all.net$col.all.lvl1[all.net$lvl1 == 4] <- original_paper_colors$col.native.lvl2[original_paper_colors$lvl2 == "1.2"]
# small clusters
all.net$col.all.lvl1[all.net$cluster.type.lvl1 == "tiny"] <- tiny.color



## INTRODUCTIONS ONLY
# Defining major and minor clusters
intro.lvl1 <- countsurf(site.intro,
                      "lvl1")
intro.net$cluster.type.lvl1 <- "tiny"
intro.net$cluster.type.lvl1[which(intro.net$lvl1 %in%
                                    intro.lvl1$x[which(intro.lvl1$surface>= minor.lim &
                                                         intro.lvl1$surface< lim)])] <- "minor"

intro.net$cluster.type.lvl1[which(intro.net$lvl1 %in%
                                    intro.lvl1$x[which(intro.lvl1$surface>= lim)])] <- "major"
if(minor.lim == 0)
{
  intro.net$cluster.type.lvl1[which(intro.net$cluster.type.lvl1 == "tiny")] <- "minor"
}
# Excluding individual basins from cluster list
intro.net$cluster.type.lvl1[which(as.character(intro.net$lvl1) == as.character(intro.net$Name))] <- "minor"

plyr::count(intro.net$cluster.type.lvl1)



# Colors
intro.net <- as.data.frame(intro.net)
intro.net <- attributeColors(intro.net,
                             lvl = "lvl1",
                             # nb.max.colors = length(which(intro.lvl1$freq >= lim)),
                             nb.max.colors = length(which(intro.lvl1$surface >= lim)),
                             palette = "Dark2",
                             colname = "col.intro.lvl1",
                             db = fish.intro,
                             site.field = "X1.Basin.Name")


## EXTINCTIONS ONLY
# Defining major and minor clusters
native.ext.lvl1 <- countsurf(site.native.ext,
                             "lvl1")
native.ext.net$cluster.type.lvl1 <- "tiny"
native.ext.net$cluster.type.lvl1[which(native.ext.net$lvl1 %in%
                                         native.ext.lvl1$x[which(native.ext.lvl1$surface>= minor.lim &
                                                                   native.ext.lvl1$surface< lim)])] <- "minor"

native.ext.net$cluster.type.lvl1[which(native.ext.net$lvl1 %in%
                                         native.ext.lvl1$x[which(native.ext.lvl1$surface>= lim)])] <- "major"
if(minor.lim == 0)
{
  native.ext.net$cluster.type.lvl1[which(native.ext.net$cluster.type.lvl1 == "tiny")] <- "minor"
}
# Excluding individual basins from cluster list
native.ext.net$cluster.type.lvl1[which(as.character(native.ext.net$lvl1) == as.character(native.ext.net$Name))] <- "minor"

plyr::count(native.ext.net$cluster.type.lvl1)


# Colors
native.ext.net <- as.data.frame(native.ext.net)
native.ext.net <- attributeColors(native.ext.net,
                             lvl = "lvl1",
                             # nb.max.colors = length(which(native.ext.lvl1$freq >= lim)),
                             nb.max.colors = length(which(native.ext.lvl1$surface >= lim)),
                             palette = "Set3",
                             colname = "col.native.ext.lvl1",
                             db = fish.native.ext,
                             site.field = "X1.Basin.Name")


# ---- Level 2 number of clusters ----

## NATIVE
# Defining major and minor clusters
native.lvl2 <- countsurf(site.native,
                         "lvl2")

native.net$cluster.type.lvl2 <- "tiny"
native.net$cluster.type.lvl2[which(native.net$lvl2 %in%
                                     native.lvl2$x[which(native.lvl2$surface>= minor.lim &
                                                          native.lvl2$surface< lim)])] <- "minor"

native.net$cluster.type.lvl2[which(native.net$lvl2 %in%
                                     native.lvl2$x[which(native.lvl2$surface>= lim)])] <- "major"
if(minor.lim == 0)
{
  native.net$cluster.type.lvl2[which(native.net$cluster.type.lvl2 == "tiny")] <- "minor"
}
# Excluding individual basins from cluster list
native.net$cluster.type.lvl2[which(as.character(native.net$lvl2) == as.character(native.net$Name))] <- "minor"

plyr::count(native.net$cluster.type.lvl2)


# Colors
native.net <- as.data.frame(native.net)
native.net$col.native.lvl2 <- minor.color

original_paper_colors <- readRDS("./data/NET_bioregions.lvl2")
native.net$col.native.lvl2[native.net$lvl2 == "1.1"] <- original_paper_colors$col.native.lvl2[original_paper_colors$lvl2 == "1.1"]
native.net$col.native.lvl2[native.net$lvl2 == "1.2"] <- original_paper_colors$col.native.lvl2[original_paper_colors$lvl2 == "1.2"]
native.net$col.native.lvl2[native.net$lvl2 == "1.3"] <- original_paper_colors$col.native.lvl2[original_paper_colors$lvl2 == "1.3"]
native.net$col.native.lvl2[native.net$lvl2 == "1.4"] <- original_paper_colors$col.native.lvl2[original_paper_colors$lvl2 == "1.4"]
native.net$col.native.lvl2[native.net$lvl2 == "2.1"] <- original_paper_colors$col.native.lvl2[original_paper_colors$lvl2 == "2.1"]
native.net$col.native.lvl2[native.net$lvl2 == "2.2"] <- original_paper_colors$col.native.lvl2[original_paper_colors$lvl2 == "2.2"]
# native.net$col.native.lvl2[native.net$lvl2 == "2.3"] <- tiny.color
native.net$col.native.lvl2[native.net$lvl2 == "minor.clusters"] <- minor.color
native.net$col.native.lvl2[native.net$lvl2 == "tiny.clusters"] <- tiny.color


## ANTHROPOCENE

# Defining major and minor clusters
all.lvl2 <- countsurf(site.all,
                         "lvl2")


all.net <- as.data.frame(all.net)

all.net$cluster.type.lvl2 <- "tiny"
all.net$cluster.type.lvl2[which(all.net$lvl2 %in%
                                  all.lvl2$x[which(all.lvl2$surface>= minor.lim &
                                                     all.lvl2$surface< lim)])] <- "minor"

all.net$cluster.type.lvl2[which(all.net$lvl2 %in%
                                  all.lvl2$x[which(all.lvl2$surface>= lim)])] <- "major"
if(minor.lim == 0)
{
  all.net$cluster.type.lvl2[which(all.net$cluster.type.lvl2 == "tiny")] <- "minor"
}
# Excluding individual basins from cluster list
all.net$cluster.type.lvl2[which(as.character(all.net$lvl2) == as.character(all.net$Name))] <- "minor"

plyr::count(all.net$cluster.type.lvl2)

# Colors from lvl1
source("./scripts/colorfromtoplevel.R")
all.net <- colorFromTopLvl(all.net,
                           toplvl = "lvl1",
                           colortoplvl = "col.all.lvl1",
                           lvl = "lvl2",
                           clustertype = "cluster.type.lvl2",
                           colorlvl = "col.all.lvl2",
                           shuffle = TRUE)

# level 2
# Neo-holarctic
# all.net$col.all.lvl2[all.net$clvl2 == 1.1] <- strtrim(viridisLite::inferno(20)[11], 7)
# # Oriental
# all.net$col.all.lvl2[all.net$clvl2 == 1.2] <- strtrim(viridisLite::inferno(20)[6], 7)
#   # native.net$col.native.lvl2[native.net$clvl2 == "1.1"][1]
# # Sinean
# all.net$col.all.lvl2[all.net$clvl2 == 1.3] <- strtrim(viridisLite::inferno(20)[16], 7)
# # Australian
# all.net$col.all.lvl2[all.net$clvl2 == 1.4] <- native.net$col.native.lvl2[native.net$clvl2 == "1.4"][1]


## INTRODUCTIONS ONLY
# Number of clusters
intro.lvl2 <- countsurf(site.intro,
                      "lvl2")
intro.net <- as.data.frame(intro.net)

intro.net$cluster.type.lvl2 <- "tiny"
intro.net$cluster.type.lvl2[which(intro.net$lvl2 %in%
                                  intro.lvl2$x[which(intro.lvl2$surface>= minor.lim &
                                                     intro.lvl2$surface< lim)])] <- "minor"

intro.net$cluster.type.lvl2[which(intro.net$lvl2 %in%
                                  intro.lvl2$x[which(intro.lvl2$surface>= lim)])] <- "major"
if(minor.lim == 0)
{
  intro.net$cluster.type.lvl2[which(intro.net$cluster.type.lvl2 == "tiny")] <- "minor"
}
# Excluding individual basins from cluster list
intro.net$cluster.type.lvl2[which(as.character(intro.net$lvl2) == as.character(intro.net$Name))] <- "minor"

plyr::count(intro.net$cluster.type.lvl2)

# Colors from lvl1
intro.net <- colorFromTopLvl(intro.net,
                             toplvl = "lvl1",
                             colortoplvl = "col.intro.lvl1",
                             lvl = "lvl2",
                             clustertype = "cluster.type.lvl2",
                             colorlvl = "col.intro.lvl2")

## EXTINCTIONS ONLY
# Number of clusters
native.ext.lvl2 <- countsurf(site.native.ext,
                             "lvl2")

native.ext.net$cluster.type.lvl2 <- "tiny"
native.ext.net$cluster.type.lvl2[which(native.ext.net$lvl2 %in%
                                         native.ext.lvl2$x[which(native.ext.lvl2$surface>= minor.lim &
                                                                   native.ext.lvl2$surface< lim)])] <- "minor"

native.ext.net$cluster.type.lvl2[which(native.ext.net$lvl2 %in%
                                         native.ext.lvl2$x[which(native.ext.lvl2$surface>= lim)])] <- "major"
if(minor.lim == 0)
{
  native.ext.net$cluster.type.lvl2[which(native.ext.net$cluster.type.lvl2 == "tiny")] <- "minor"
}
# Excluding individual basins from cluster list
native.ext.net$cluster.type.lvl2[which(as.character(native.ext.net$lvl2) == as.character(native.ext.net$Name))] <- "minor"

plyr::count(native.ext.net$cluster.type.lvl2)

# Colors from lvl1
native.ext.net <- as.data.frame(native.ext.net)
native.ext.net <- attributeColors(native.ext.net,
                                  lvl = "lvl2",
                                  # nb.max.colors = length(which(native.ext.lvl2$freq >= lim)),
                                  nb.max.colors = length(which(native.ext.lvl2$surface >= lim)),
                                  palette = "Pastel2",
                                  colname = "col.native.ext.lvl2",
                                  db = fish.native.ext,
                                  site.field = "X1.Basin.Name",
                                  sh.grey = FALSE) # Ajouter à la fonction la possibilité de mettre en noir les groupes trop éloignés




# ---- Level 3 number of clusters ----

## NATIVE
# Defining major and minor clusters
native.lvl3 <- countsurf(site.native,
                         "lvl3")

native.net$cluster.type.lvl3 <- "tiny"
native.net$cluster.type.lvl3[which(native.net$lvl3 %in%
                                     native.lvl3$x[which(native.lvl3$surface>= minor.lim &
                                                           native.lvl3$surface< lim)])] <- "minor"

native.net$cluster.type.lvl3[which(native.net$lvl3 %in%
                                     native.lvl3$x[which(native.lvl3$surface>= lim)])] <- "major"
if(minor.lim == 0)
{
  native.net$cluster.type.lvl3[which(native.net$cluster.type.lvl3 == "tiny")] <- "minor"
}
# Excluding individual basins from cluster list
native.net$cluster.type.lvl3[which(as.character(native.net$lvl3) == as.character(native.net$Name))] <- "minor"

plyr::count(native.net$cluster.type.lvl3)
# Colors from lvl2
native.net <- colorFromTopLvl(native.net,
                             toplvl = "lvl2",
                             colortoplvl = "col.native.lvl2",
                             lvl = "lvl3",
                             clustertype = "cluster.type.lvl3",
                             colorlvl = "col.native.lvl3")

## ANTHROPOCENE
# Defining major and minor clusters
all.lvl3 <- countsurf(site.all,
                         "lvl3")

all.net$cluster.type.lvl3 <- "tiny"
all.net$cluster.type.lvl3[which(all.net$lvl3 %in%
                                     all.lvl3$x[which(all.lvl3$surface>= minor.lim &
                                                           all.lvl3$surface< lim)])] <- "minor"

all.net$cluster.type.lvl3[which(all.net$lvl3 %in%
                                     all.lvl3$x[which(all.lvl3$surface>= lim)])] <- "major"
if(minor.lim == 0)
{
  all.net$cluster.type.lvl3[which(all.net$cluster.type.lvl3 == "tiny")] <- "minor"
}
# Excluding individual basins from cluster list
all.net$cluster.type.lvl3[which(as.character(all.net$lvl3) == as.character(all.net$Name))] <- "minor"

plyr::count(all.net$cluster.type.lvl3)
# Colors from lvl1
all.net <- colorFromTopLvl(all.net,
                              toplvl = "lvl1",
                              colortoplvl = "col.all.lvl1",
                              lvl = "lvl3",
                              clustertype = "cluster.type.lvl3",
                              colorlvl = "col.all.lvl3")

## INTRO ONLY
# Defining major and minor clusters
intro.lvl3 <- countsurf(site.intro,
                        "lvl3")

intro.net$cluster.type.lvl3 <- "tiny"
intro.net$cluster.type.lvl3[which(intro.net$lvl3 %in%
                                  intro.lvl3$x[which(intro.lvl3$surface>= minor.lim &
                                                     intro.lvl3$surface< lim)])] <- "minor"

intro.net$cluster.type.lvl3[which(intro.net$lvl3 %in%
                                  intro.lvl3$x[which(intro.lvl3$surface>= lim)])] <- "major"
if(minor.lim == 0)
{
  intro.net$cluster.type.lvl3[which(intro.net$cluster.type.lvl3 == "tiny")] <- "minor"
}
# Excluding individual basins from cluster list
intro.net$cluster.type.lvl3[which(as.character(intro.net$lvl3) == as.character(intro.net$Name))] <- "minor"

plyr::count(intro.net$cluster.type.lvl3)
# Colors from lvl1
intro.net <- colorFromTopLvl(intro.net,
                             toplvl = "lvl1",
                             colortoplvl = "col.intro.lvl1",
                             lvl = "lvl3",
                             clustertype = "cluster.type.lvl3",
                             colorlvl = "col.intro.lvl3")

## EXTINCTIONS ONLY
# Defining major and minor clusters
native.ext.lvl3 <- countsurf(site.native.ext,
                        "lvl3")

native.ext.net$cluster.type.lvl3 <- "tiny"
native.ext.net$cluster.type.lvl3[which(native.ext.net$lvl3 %in%
                                    native.ext.lvl3$x[which(native.ext.lvl3$surface>= minor.lim &
                                                         native.ext.lvl3$surface< lim)])] <- "minor"

native.ext.net$cluster.type.lvl3[which(native.ext.net$lvl3 %in%
                                    native.ext.lvl3$x[which(native.ext.lvl3$surface>= lim)])] <- "major"
if(minor.lim == 0)
{
  native.ext.net$cluster.type.lvl3[which(native.ext.net$cluster.type.lvl3 == "tiny")] <- "minor"
}
# Excluding individual basins from cluster list
native.ext.net$cluster.type.lvl3[which(as.character(native.ext.net$lvl3) == as.character(native.ext.net$Name))] <- "minor"

plyr::count(native.ext.net$cluster.type.lvl3)
# Colors from lvl2
native.ext.net <- colorFromTopLvl(native.ext.net,
                             toplvl = "lvl2",
                             colortoplvl = "col.native.ext.lvl2",
                             lvl = "lvl3",
                             clustertype = "cluster.type.lvl3",
                             colorlvl = "col.native.ext.lvl3")

# ----- Number of sites per cluster -----
# Creating a plot to show the ordered number of sites per cluster
lvls <- c("lvl1",
          "lvl2",
          "lvl3")
datasets <- c("native", "all", "intro", "native.ext")

cutoffs <- data.frame()

nbsitescluster <- data.frame()
for (dataset in datasets)
{
  for(lvl in lvls)
  {
    tmp <- data.frame(get(paste0(dataset, ".", lvl)))

    # INCLUDE OR EXCLUDE individual sites?
    tmp <- tmp[-which(tmp$freq == 1), ]

    # tmp$pos <- rank(tmp$freq,
    #                 ties.method = "first")
    tmp$pos <- rank(tmp$surface,
                    ties.method = "first")
    tmp$lvl <- lvl
    tmp$dataset <- dataset
    tmp$type <- ifelse(tmp$surface >= lim,
                       "major",
                       "minor")

    nbsitescluster <- rbind(nbsitescluster,
                            tmp)


    if(minor.lim == 0)
    {
      cutoffs <- rbind(cutoffs,
                       data.frame(xstart = min(tmp$pos),
                                  xend = max(tmp$pos),
                                  ystart = c(0, lim),
                                  # yend = c(lim, max(tmp$freq)),
                                  yend = c(lim, max(tmp$surface)),
                                  type = c("Minor", "Major"),
                                  lvl = lvl,
                                  dataset = dataset,
                                  number = c(length(which(tmp$type == "minor")),
                                             length(which(tmp$type == "major")))))
    } 

    rm(tmp)
  }
}



# Correcting cutoffs for facets

# General cutoff
cutoffs$xend <- max(cutoffs$xend)
# Adaptive cutoffs
# cutoffs$xend[which(cutoffs$lvl == "lvl3")] <- max(cutoffs$xend[which(cutoffs$lvl == "lvl3")])
# cutoffs$xend[which(cutoffs$lvl == "lvl2")] <- max(cutoffs$xend[which(cutoffs$lvl == "lvl2")])
# cutoffs$xend[which(cutoffs$lvl == "lvl1")] <- max(cutoffs$xend[which(cutoffs$lvl == "lvl1")])
cutoffs$yend[which(cutoffs$dataset == "all" &
                     cutoffs$type == "Major")] <- max(cutoffs$yend[which(cutoffs$dataset == "all")])
cutoffs$yend[which(cutoffs$dataset == "intro" &
                     cutoffs$type == "Major")] <- max(cutoffs$yend[which(cutoffs$dataset == "intro")])
cutoffs$yend[which(cutoffs$dataset == "native" &
                     cutoffs$type == "Major")] <- max(cutoffs$yend[which(cutoffs$dataset == "native")])
cutoffs$yend[which(cutoffs$dataset == "native.ext" &
                     cutoffs$type == "Major")] <- max(cutoffs$yend[which(cutoffs$dataset == "native.ext")])
nbsitescluster$lvl <- as.factor(nbsitescluster$lvl)
cutoffs$lvl <- as.factor(cutoffs$lvl)
nbsitescluster$dataset <- as.factor(nbsitescluster$dataset)
cutoffs$dataset <- as.factor(cutoffs$dataset)

levels(nbsitescluster$lvl) <- levels(cutoffs$lvl) <- c("Level 1",
                                                       "Level 2",
                                                       "Level 3")
levels(nbsitescluster$dataset) <- levels(cutoffs$dataset) <- c("Anthropocene", "Introductions\nonly",
                                                               "Natural",
                                                               "Extinctions\nonly")
nbsitescluster$dataset <- factor(nbsitescluster$dataset,
                                 levels = c("Natural",
                                            "Anthropocene",
                                            "Introductions\nonly",
                                            "Extinctions\nonly"))
cutoffs$dataset <- factor(cutoffs$dataset,
                          levels = c("Natural",
                                     "Anthropocene",
                                     "Introductions\nonly",
                                     "Extinctions\nonly"))

# text position
cutoffs$ytext <- cutoffs$ystart + 1e2
cutoffs$ytext[which(cutoffs$type == "Major")] <- cutoffs$yend[which(cutoffs$type == "Major")] -
  cutoffs$yend[which(cutoffs$type == "Major")]/2


lett <- data.frame(letters = paste0(letters[1:12], "."),
                   lvl = factor(rep(c("Level 1",
                               "Level 2",
                               "Level 3"), 4),
                               levels = c("Level 1",
                                          "Level 2",
                                          "Level 3")),
                   dataset = factor(c(rep("Natural", 3),
                               rep("Anthropocene", 3),
                               rep("Introductions\nonly", 3),
                               rep("Extinctions\nonly", 3)),
                               levels = c("Natural",
                                          "Anthropocene",
                                          "Introductions\nonly",
                                          "Extinctions\nonly")))

pclusters <- ggplot(nbsitescluster) +
  facet_grid(dataset ~ lvl,
             scales = "free_x") +
  geom_rect(data = cutoffs, aes(xmin=xstart,
                                xmax=xend,
                                ymin=ystart,
                                ymax=yend,
                                fill=type), alpha = 0.5) +
  geom_point(size = .5,
             aes(x = pos,
                 y = surface)) +
  geom_line(col = grey(.5),
            aes(x = pos,
                y = surface)) +
  geom_text(data = cutoffs,
            aes(x = max(nbsitescluster$pos),
                y = ytext,
                label = paste0("n = ", number)),
            hjust = 1,
            fontface = "bold") +
  geom_text(data = lett,
            aes(x = 4, y = 1e08,
                label = letters),
            hjust = 0,
            vjust = 0,
            fontface = "bold") +
  scale_y_log10(limits = c(NA, 2e08)) +
  annotation_logticks(sides = "l") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73"),
                    labels = c("Major\n(x \u2265 1,000,000)",
                               "Minor\n(1,000,000 > x \u2265 0)",
                               "Tiny\n(x \u2264 10)"),
                    name = "Cluster category\n(x = Cluster\narea (km²))") +
  theme_bw() +
  xlab("Clusters (ordered by area)") +
  ylab("Cluster area (km²)") +
  theme(text = element_text(size = 16))
pclusters

cairo_pdf("./outputs/nb_sites_per_cluster.pdf",
          height = 10, width = 10)
pclusters
dev.off()
# ggsave("./outputs/nb_sites_per_cluster.png", pclusters,
       # width = 35, height = 30, device = "png", units = "cm")


# Cluster annotation, color changes for maps --------------------------------
# Native regions
native.net$clvl1 <- as.character(native.net$lvl1)
native.net$clvl1[which(native.net$cluster.type.lvl1 == "tiny")] <- "tiny.clusters"
native.net$clvl1[which(native.net$cluster.type.lvl1 == "minor")] <- "minor.clusters"

native.net$clvl2 <- as.character(native.net$lvl2)
native.net$clvl2[which(native.net$cluster.type.lvl2 == "tiny")] <- "tiny.clusters"
native.net$clvl2[which(native.net$cluster.type.lvl2 == "minor")] <- "minor.clusters"

native.net$clvl3 <- as.character(native.net$lvl3)
native.net$clvl3[which(native.net$cluster.type.lvl3 == "tiny")] <- "tiny.clusters"
native.net$clvl3[which(native.net$cluster.type.lvl3 == "minor")] <- "minor.clusters"



# Anthropocene regions
all.net$clvl1 <- as.character(all.net$lvl1)
all.net$clvl1[which(all.net$cluster.type.lvl1 == "tiny")] <- "tiny.clusters"
all.net$clvl1[which(all.net$cluster.type.lvl1 == "minor")] <- "minor.clusters"

all.net$clvl2 <- as.character(all.net$lvl2)
all.net$clvl2[which(all.net$cluster.type.lvl2 == "tiny")] <- "tiny.clusters"
all.net$clvl2[which(all.net$cluster.type.lvl2 == "minor")] <- "minor.clusters"

all.net$clvl3 <- as.character(all.net$lvl3)
all.net$clvl3[which(all.net$cluster.type.lvl3 == "tiny")] <- "tiny.clusters"
all.net$clvl3[which(all.net$cluster.type.lvl3 == "minor")] <- "minor.clusters"


# Network introduced only
intro.net$clvl1 <- as.character(intro.net$lvl1)
intro.net$clvl1[which(intro.net$cluster.type.lvl1 == "tiny")] <- "tiny.clusters"
intro.net$clvl1[which(intro.net$cluster.type.lvl1 == "minor")] <- "minor.clusters"

intro.net$clvl2 <- as.character(intro.net$lvl2)
intro.net$clvl2[which(intro.net$cluster.type.lvl2 == "tiny")] <- "tiny.clusters"
intro.net$clvl2[which(intro.net$cluster.type.lvl2 == "minor")] <- "minor.clusters"

intro.net$clvl3 <- as.character(intro.net$lvl3)
intro.net$clvl3[which(intro.net$cluster.type.lvl3 == "tiny")] <- "tiny.clusters"
intro.net$clvl3[which(intro.net$cluster.type.lvl3 == "minor")] <- "minor.clusters"

# Network extinctions only
native.ext.net$clvl1 <- as.character(native.ext.net$lvl1)
native.ext.net$clvl1[which(native.ext.net$cluster.type.lvl1 == "tiny")] <- "tiny.clusters"
native.ext.net$clvl1[which(native.ext.net$cluster.type.lvl1 == "minor")] <- "minor.clusters"

native.ext.net$clvl2 <- as.character(native.ext.net$lvl2)
native.ext.net$clvl2[which(native.ext.net$cluster.type.lvl2 == "tiny")] <- "tiny.clusters"
native.ext.net$clvl2[which(native.ext.net$cluster.type.lvl2 == "minor")] <- "minor.clusters"

native.ext.net$clvl3 <- as.character(native.ext.net$lvl3)
native.ext.net$clvl3[which(native.ext.net$cluster.type.lvl3 == "tiny")] <- "tiny.clusters"
native.ext.net$clvl3[which(native.ext.net$cluster.type.lvl3 == "minor")] <- "minor.clusters"

# Add levels to maps ------------------------------------------------------
basinshp$native.lvl1 <- native.net$clvl1[match(basinshp$BasinName,
                                               native.net$Name)]
basinshp$native.lvl2 <- native.net$clvl2[match(basinshp$BasinName,
                                               native.net$Name)]
basinshp$native.lvl3 <- native.net$clvl3[match(basinshp$BasinName,
                                               native.net$Name)]

basinshp$all.lvl1 <- all.net$clvl1[match(basinshp$BasinName, all.net$Name)]
basinshp$all.lvl2 <- all.net$clvl2[match(basinshp$BasinName, all.net$Name)]
basinshp$all.lvl3 <- all.net$clvl3[match(basinshp$BasinName, all.net$Name)]

basinshp$intro.lvl1 <- intro.net$clvl1[match(basinshp$BasinName,
                                             intro.net$Name)]
basinshp$intro.lvl2 <- intro.net$clvl2[match(basinshp$BasinName,
                                             intro.net$Name)]
basinshp$intro.lvl3 <- intro.net$clvl3[match(basinshp$BasinName,
                                             intro.net$Name)]

basinshp$native.ext.lvl1 <- native.ext.net$clvl1[match(basinshp$BasinName,
                                                       native.ext.net$Name)]
basinshp$native.ext.lvl2 <- native.ext.net$clvl2[match(basinshp$BasinName,
                                                       native.ext.net$Name)]
basinshp$native.ext.lvl3 <- native.ext.net$clvl3[match(basinshp$BasinName,
                                                       native.ext.net$Name)]



# Add colours to maps ------------------------------------------------------

basinshp$color.native.lvl1 <-
  native.net$col.native.lvl1[match(basinshp$BasinName, native.net$Name)]
basinshp$color.native.lvl2 <-
  native.net$col.native.lvl2[match(basinshp$BasinName, native.net$Name)]
basinshp$color.native.lvl3 <-
  native.net$col.native.lvl3[match(basinshp$BasinName, native.net$Name)]



basinshp$color.all.lvl1 <-
  all.net$col.all.lvl1[match(basinshp$BasinName, all.net$Name)]
basinshp$color.all.lvl2 <-
  all.net$col.all.lvl2[match(basinshp$BasinName, all.net$Name)]
basinshp$color.all.lvl3 <-
  all.net$col.all.lvl3[match(basinshp$BasinName, all.net$Name)]

basinshp$color.intro.lvl1 <-
  intro.net$col.intro.lvl1[match(basinshp$BasinName, intro.net$Name)]
basinshp$color.intro.lvl2 <-
  intro.net$col.intro.lvl2[match(basinshp$BasinName, intro.net$Name)]
basinshp$color.intro.lvl3 <-
  intro.net$col.intro.lvl3[match(basinshp$BasinName, intro.net$Name)]

basinshp$color.native.ext.lvl1 <-
  native.ext.net$col.native.ext.lvl1[match(basinshp$BasinName, native.ext.net$Name)]
basinshp$color.native.ext.lvl2 <-
  native.ext.net$col.native.ext.lvl2[match(basinshp$BasinName, native.ext.net$Name)]
basinshp$color.native.ext.lvl3 <-
  native.ext.net$col.native.ext.lvl3[match(basinshp$BasinName, native.ext.net$Name)]

# Create name tables ------------------------------------------------------
b <- st_as_sf(basinshp)
ggplot() +
  geom_sf(data = b, aes(fill = color.native.lvl1)) +
  scale_fill_identity()
bioregions.lvl1 <- unique(native.net[, c("clvl1", "col.native.lvl1")])
bioregions.lvl1$name <- c("Old World",
                          "New World",
                          "Minor Clusters")
bioregions.lvl1$name[-grep("Minor", bioregions.lvl1$name)] <-
  paste0(bioregions.lvl1$name[-grep("Minor", bioregions.lvl1$name)],
         " (",
         bioregions.lvl1$clvl1[-grep("Minor", bioregions.lvl1$name)],
         ")")

ggplot() +
  geom_sf(data = b, aes(fill = color.native.lvl2))+
  scale_fill_identity()
bioregions.lvl2 <- unique(native.net[, c("clvl2", "col.native.lvl2")])
bioregions.lvl2$name <-  c("Sino-Oriental",
                           "Ethiopian",
                           "Palearctic",
                           "Australian",
                           "Neotropical",
                           "Nearctic",
                           # "CA transition zone",
                           "Minor clusters")


bioregions.lvl2$name[-grep("Minor", bioregions.lvl2$name)] <-
  paste0(bioregions.lvl2$name[-grep("Minor", bioregions.lvl2$name)],
         " (",
         bioregions.lvl2$clvl2[-grep("Minor", bioregions.lvl2$name)],
         ")")

ggplot() +
  geom_sf(data = b, aes(fill = color.native.lvl3))+
  scale_fill_identity()
bioregions.lvl3 <- unique(native.net[, c("clvl3", "col.native.lvl3")])
bioregions.lvl3$name <-  bioregions.lvl3$clvl3

ggplot() +
  geom_sf(data = b, aes(fill = color.all.lvl1)) +
  scale_fill_identity()
anthroregions.lvl1 <- unique(all.net[, c("clvl1", "col.all.lvl1")])
anthroregions.lvl1$name <- c("PAGNEA",
                             "Neotropical",
                             "Neo-Oriental",
                             "Ethiopian",
                             "Minor Clusters")

anthroregions.lvl1$name[-grep("Minor", anthroregions.lvl1$name)] <-
  paste0(anthroregions.lvl1$name[-grep("Minor", anthroregions.lvl1$name)],
         " (",
         anthroregions.lvl1$clvl1[-grep("Minor", anthroregions.lvl1$name)],
         ")")

ggplot() +
  geom_sf(data = b, aes(fill = color.all.lvl2)) +
  scale_fill_identity()
anthroregions.lvl2 <- unique(all.net[, c("clvl2", "col.all.lvl2")])
anthroregions.lvl2$name <- anthroregions.lvl2$clvl2

ggplot() +
  geom_sf(data = b, aes(fill = color.all.lvl3)) +
  scale_fill_identity()
anthroregions.lvl3 <- unique(all.net[, c("clvl3", "col.all.lvl3")])
anthroregions.lvl3$name <- anthroregions.lvl3$clvl3

ggplot() +
  geom_sf(data = b, aes(fill = color.intro.lvl1)) +
  scale_fill_identity()
introregions.lvl1 <- unique(intro.net[, c("clvl1", "col.intro.lvl1")])
introregions.lvl1$name <- c("PAGNEA",
                            "Neotropical",
                            "Neo-Oriental",
                            "Ethiopian",
                            # "Sinean",
                            "Minor Clusters")

introregions.lvl1$name[-grep("Minor", introregions.lvl1$name)] <-
  paste0(introregions.lvl1$name[-grep("Minor", introregions.lvl1$name)],
         " (",
         introregions.lvl1$clvl1[-grep("Minor", introregions.lvl1$name)],
         ")")

ggplot() +
  geom_sf(data = b, aes(fill = color.intro.lvl2)) +
  scale_fill_identity()
introregions.lvl2 <- unique(intro.net[, c("clvl2", "col.intro.lvl2")])
introregions.lvl2$name <- introregions.lvl2$clvl2

ggplot() +
  geom_sf(data = b, aes(fill = color.intro.lvl3)) +
  scale_fill_identity()
introregions.lvl3 <- unique(intro.net[, c("clvl3", "col.intro.lvl3")])
introregions.lvl3$name <- introregions.lvl3$clvl3

ggplot() +
  geom_sf(data = b, aes(fill = color.native.ext.lvl1)) +
  scale_fill_identity()
native.extregions.lvl1 <- unique(native.ext.net[, c("clvl1", "col.native.ext.lvl1")])
native.extregions.lvl1$name <- c("Old World",
                                 "New World",
                                 "Minor Clusters")

native.extregions.lvl1$name[-grep("Minor", native.extregions.lvl1$name)] <-
  paste0(native.extregions.lvl1$name[-grep("Minor", native.extregions.lvl1$name)],
         " (",
         native.extregions.lvl1$clvl1[-grep("Minor", native.extregions.lvl1$name)],
         ")")

ggplot() +
  geom_sf(data = b, aes(fill = color.native.ext.lvl2)) +
  scale_fill_identity()
native.extregions.lvl2 <- unique(native.ext.net[, c("clvl2", "col.native.ext.lvl2")])
native.extregions.lvl2$name <-  c("Sino-Oriental",
                                  "Ethiopian",
                                  "Palearctic",
                                  "Australian",
                                  "Neotropical",
                                  "Nearctic",
                                  "Minor Clusters")

native.extregions.lvl2$name[-grep("Minor", native.extregions.lvl2$name)] <-
  paste0(native.extregions.lvl2$name[-grep("Minor", native.extregions.lvl2$name)],
         " (",
         native.extregions.lvl2$clvl2[-grep("Minor", native.extregions.lvl2$name)],
         ")")

ggplot() +
  geom_sf(data = b, aes(fill = color.native.ext.lvl3)) +
  scale_fill_identity()
native.extregions.lvl3 <- unique(native.ext.net[, c("clvl3", "col.native.ext.lvl3")])
native.extregions.lvl3$name <-  native.extregions.lvl3$clvl3

# Add region names to map -------------------------------------------------
basinshp$Native_level1 <- bioregions.lvl1$name[match(basinshp$native.lvl1,
                                                           bioregions.lvl1$clvl1)]
basinshp$Native_level2 <- bioregions.lvl2$name[match(basinshp$native.lvl2,
                                                      bioregions.lvl2$clvl2)]
basinshp$Native_level3 <- bioregions.lvl3$name[match(basinshp$native.lvl3,
                                                     bioregions.lvl3$clvl3)]

basinshp$Anthropocene_level1 <- anthroregions.lvl1$name[match(basinshp$all.lvl1,
                                                                    anthroregions.lvl1$clvl1)]
basinshp$Anthropocene_level2 <- anthroregions.lvl2$name[match(basinshp$all.lvl2,
                                                               anthroregions.lvl2$clvl2)]
basinshp$Anthropocene_level3 <- anthroregions.lvl3$name[match(basinshp$all.lvl3,
                                                              anthroregions.lvl3$clvl3)]

basinshp$Intro_level1 <- introregions.lvl1$name[match(basinshp$intro.lvl1,
                                                            introregions.lvl1$clvl1)]
basinshp$Intro_level2 <- introregions.lvl2$name[match(basinshp$intro.lvl2,
                                                       introregions.lvl2$clvl2)]
basinshp$Intro_level3 <- introregions.lvl3$name[match(basinshp$intro.lvl3,
                                                      introregions.lvl3$clvl3)]

basinshp$Native.ext_level1 <- native.extregions.lvl1$name[match(basinshp$native.ext.lvl1,
                                                                      native.extregions.lvl1$clvl1)]
basinshp$Native.ext_level2 <- native.extregions.lvl2$name[match(basinshp$native.ext.lvl2,
                                                                 native.extregions.lvl2$clvl2)]
basinshp$Native.ext_level3 <- native.extregions.lvl3$name[match(basinshp$native.ext.lvl3,
                                                                native.extregions.lvl3$clvl3)]


# Add unmodified clusters for map delineation ----------------------------
basinshp$native.ulvl1 <- native.net$lvl1[match(basinshp$BasinName,
                                               native.net$Name)]
basinshp$native.ulvl2 <- native.net$lvl2[match(basinshp$BasinName,
                                               native.net$Name)]
basinshp$native.ulvl3 <- native.net$lvl3[match(basinshp$BasinName,
                                               native.net$Name)]

basinshp$all.ulvl1 <- all.net$lvl1[match(basinshp$BasinName, all.net$Name)]
basinshp$all.ulvl2 <- all.net$lvl2[match(basinshp$BasinName, all.net$Name)]
basinshp$all.ulvl3 <- all.net$lvl3[match(basinshp$BasinName, all.net$Name)]

basinshp$intro.ulvl1 <- intro.net$lvl1[match(basinshp$BasinName,
                                             intro.net$Name)]
basinshp$intro.ulvl2 <- intro.net$lvl2[match(basinshp$BasinName,
                                             intro.net$Name)]
basinshp$intro.ulvl3 <- intro.net$lvl3[match(basinshp$BasinName,
                                             intro.net$Name)]

basinshp$native.ext.ulvl1 <- native.ext.net$lvl1[match(basinshp$BasinName,
                                                       native.ext.net$Name)]
basinshp$native.ext.ulvl2 <- native.ext.net$lvl2[match(basinshp$BasinName,
                                                       native.ext.net$Name)]
basinshp$native.ext.ulvl3 <- native.ext.net$lvl3[match(basinshp$BasinName,
                                                       native.ext.net$Name)]







# Save shapefiles ---------------------------------------------------------

# Function to union region polygons
createShapeRegions <- function(basin.shapefile,
                               cluster.vector,
                               filename = NULL,
                               directory = getwd())
{
  basin.shapefile <- basin.shapefile[cluster.vector]
  
  if(any(is.na(cluster.vector)))
  {
    basin.shapefile <- basin.shapefile[
      -which(is.na(cluster.vector)), ]
    cluster.vector <- cluster.vector[-which(is.na(cluster.vector))]
  }
  
  region.shapefiles <- aggregate(basin.shapefile, 
                                 list(droplevels(as.data.frame(basin.shapefile))[, cluster.vector]), 
                                 head,
                                 n = 1)
  return(region.shapefiles)
}

natregionslvl1 <- createShapeRegions(basinshp,
                                     "native.ulvl1")
natregionslvl2 <- createShapeRegions(basinshp,
                                     "native.ulvl2")
natregionslvl3 <- createShapeRegions(basinshp,
                                     "native.ulvl3")
antregionslvl1 <- createShapeRegions(basinshp,
                                     "all.ulvl1")
antregionslvl2 <- createShapeRegions(basinshp,
                                     "all.ulvl2")
antregionslvl3 <- createShapeRegions(basinshp,
                                     "all.ulvl3")
intregionslvl1 <- createShapeRegions(basinshp,
                                     "intro.ulvl1")
intregionslvl2 <- createShapeRegions(basinshp,
                                     "intro.ulvl2")
intregionslvl3 <- createShapeRegions(basinshp,
                                     "intro.ulvl3")
extregionslvl1 <- createShapeRegions(basinshp,
                                     "native.ext.ulvl1")
extregionslvl2 <- createShapeRegions(basinshp,
                                     "native.ext.ulvl2")
extregionslvl3 <- createShapeRegions(basinshp,
                                     "native.ext.ulvl3")


natregionslvl1 <- st_make_valid(natregionslvl1)
natregionslvl2 <- st_make_valid(natregionslvl2)
natregionslvl3 <- st_make_valid(natregionslvl3)
antregionslvl1 <- st_make_valid(antregionslvl1)
antregionslvl2 <- st_make_valid(antregionslvl2)
antregionslvl3 <- st_make_valid(antregionslvl3)
intregionslvl1 <- st_make_valid(intregionslvl1)
intregionslvl2 <- st_make_valid(intregionslvl2)
intregionslvl3 <- st_make_valid(intregionslvl3)
extregionslvl1 <- st_make_valid(extregionslvl1)
extregionslvl2 <- st_make_valid(extregionslvl2)
extregionslvl3 <- st_make_valid(extregionslvl3)

natregionslvl1$color <-
  bioregions.lvl1$col.native.lvl1[match(natregionslvl1$Group.1,
                                        bioregions.lvl1$clvl1)]
natregionslvl2$color <- bioregions.lvl2$col.native.lvl2[match(natregionslvl2$Group.1,
                                                              bioregions.lvl2$clvl2)]
natregionslvl3$color <- bioregions.lvl3$col.native.lvl3[match(natregionslvl3$Group.1,
                                                              bioregions.lvl3$clvl3)]
antregionslvl1$color <- anthroregions.lvl1$col.all.lvl1[match(antregionslvl1$Group.1,
                                                              anthroregions.lvl1$clvl1)]
antregionslvl2$color <- anthroregions.lvl2$col.all.lvl2[match(antregionslvl2$Group.1,
                                                              anthroregions.lvl2$clvl2)]
antregionslvl3$color <- anthroregions.lvl3$col.all.lvl3[match(antregionslvl3$Group.1,
                                                              anthroregions.lvl3$clvl3)]
intregionslvl1$color <- introregions.lvl1$col.intro.lvl1[match(intregionslvl1$Group.1,
                                                               introregions.lvl1$clvl1)]
intregionslvl2$color <- introregions.lvl2$col.intro.lvl2[match(intregionslvl2$Group.1,
                                                               introregions.lvl2$clvl2)]
intregionslvl3$color <- introregions.lvl3$col.intro.lvl3[match(intregionslvl3$Group.1,
                                                               introregions.lvl3$clvl3)]
extregionslvl1$color <- native.extregions.lvl1$col.native.ext.lvl1[match(extregionslvl1$Group.1,
                                                                  native.extregions.lvl1$clvl1)]
extregionslvl2$color <- native.extregions.lvl2$col.native.ext.lvl2[match(extregionslvl2$Group.1,
                                                                  native.extregions.lvl2$clvl2)]
extregionslvl3$color <- native.extregions.lvl3$col.native.ext.lvl3[match(extregionslvl3$Group.1,
                                                                         native.extregions.lvl3$clvl3)]

# Re-adding minor cluster color because of small cluster delination
natregionslvl1$color[which(is.na(natregionslvl1$color))] <-
  bioregions.lvl1$col.native.lvl1[bioregions.lvl1$clvl1 == "minor.clusters"]
natregionslvl2$color[which(is.na(natregionslvl2$color))] <-
  bioregions.lvl2$col.native.lvl2[bioregions.lvl2$clvl2 == "minor.clusters"]
natregionslvl3$color[which(is.na(natregionslvl3$color))] <-
  bioregions.lvl3$col.native.lvl3[bioregions.lvl3$clvl3 == "minor.clusters"]
antregionslvl1$color[which(is.na(antregionslvl1$color))] <-
  anthroregions.lvl1$col.all.lvl1[anthroregions.lvl1$clvl1 == "minor.clusters"]
antregionslvl2$color[which(is.na(antregionslvl2$color))] <-
  anthroregions.lvl2$col.all.lvl2[anthroregions.lvl2$clvl2 == "minor.clusters"]
antregionslvl3$color[which(is.na(antregionslvl3$color))] <-
  anthroregions.lvl3$col.all.lvl3[anthroregions.lvl3$clvl3 == "minor.clusters"]
intregionslvl1$color[which(is.na(intregionslvl1$color))] <-
  introregions.lvl1$col.intro.lvl1[introregions.lvl1$clvl1 == "minor.clusters"]
intregionslvl2$color[which(is.na(intregionslvl2$color))] <-
  introregions.lvl2$col.intro.lvl2[introregions.lvl2$clvl2 == "minor.clusters"]
intregionslvl3$color[which(is.na(intregionslvl3$color))] <-
  introregions.lvl3$col.intro.lvl3[introregions.lvl3$clvl3 == "minor.clusters"]
extregionslvl1$color[which(is.na(extregionslvl1$color))] <-
  native.extregions.lvl1$col.native.ext.lvl1[native.extregions.lvl1$clvl1 == "minor.clusters"]
extregionslvl2$color[which(is.na(extregionslvl2$color))] <-
  native.extregions.lvl2$col.native.ext.lvl2[native.extregions.lvl2$clvl2 == "minor.clusters"]
extregionslvl3$color[which(is.na(extregionslvl3$color))] <-
  native.extregions.lvl3$col.native.ext.lvl3[native.extregions.lvl3$clvl3 == "minor.clusters"]


# ------------------------- Preparing maps ------------------

natregionslvl1.w <- st_transform_proj(natregionslvl1,
                                      crs = "+proj=wintri +datum=WGS84 +no_defs +over")
natregionslvl2.w <- st_transform_proj(natregionslvl2,
                                      crs = "+proj=wintri +datum=WGS84 +no_defs +over")
natregionslvl3.w <- st_transform_proj(natregionslvl3,
                                      crs = "+proj=wintri +datum=WGS84 +no_defs +over")
antregionslvl1.w <- st_transform_proj(antregionslvl1,
                                      crs = "+proj=wintri +datum=WGS84 +no_defs +over")
antregionslvl2.w <- st_transform_proj(antregionslvl2,
                                      crs = "+proj=wintri +datum=WGS84 +no_defs +over")
antregionslvl3.w <- st_transform_proj(antregionslvl3,
                                      crs = "+proj=wintri +datum=WGS84 +no_defs +over")
intregionslvl1.w <- st_transform_proj(intregionslvl1,
                                      crs = "+proj=wintri +datum=WGS84 +no_defs +over")
intregionslvl2.w <- st_transform_proj(intregionslvl2,
                                      crs = "+proj=wintri +datum=WGS84 +no_defs +over")
intregionslvl3.w <- st_transform_proj(intregionslvl3,
                                      crs = "+proj=wintri +datum=WGS84 +no_defs +over")
extregionslvl1.w <- st_transform_proj(extregionslvl1,
                                      crs = "+proj=wintri +datum=WGS84 +no_defs +over")
extregionslvl2.w <- st_transform_proj(extregionslvl2,
                                      crs = "+proj=wintri +datum=WGS84 +no_defs +over")
extregionslvl3.w <- st_transform_proj(extregionslvl3,
                                      crs = "+proj=wintri +datum=WGS84 +no_defs +over")


removeCollection <- function(x)
{
  if(any(sapply(x$geometry, class)[2, ] != "MULTIPOLYGON"))
  {
    errs <- which(sapply(x$geometry, class)[2, ] != "MULTIPOLYGON")
    onlypoly <- st_collection_extract(x[errs, ], "POLYGON")
    for(i in errs)
    {
      x$geometry[i] <- onlypoly$geometry[which(errs == i)]
    }
  }
  return(x)
}
natregionslvl1.w <- removeCollection(natregionslvl1.w)
natregionslvl2.w <- removeCollection(natregionslvl2.w)
natregionslvl3.w <- removeCollection(natregionslvl3.w)
antregionslvl1.w <- removeCollection(antregionslvl1.w)
antregionslvl2.w <- removeCollection(antregionslvl2.w)
antregionslvl3.w <- removeCollection(antregionslvl3.w)
intregionslvl1.w <- removeCollection(intregionslvl1.w)
intregionslvl2.w <- removeCollection(intregionslvl2.w)
intregionslvl3.w <- removeCollection(intregionslvl3.w)
extregionslvl1.w <- removeCollection(extregionslvl1.w)
extregionslvl2.w <- removeCollection(extregionslvl2.w)
extregionslvl3.w <- removeCollection(extregionslvl3.w)


# Reorder legends ------------------------
bioregions.lvl3 <- bioregions.lvl3[c(1:(grep("minor", bioregions.lvl3$clvl3) - 1),
                                     (grep("minor", bioregions.lvl3$clvl3) + 1):nrow(bioregions.lvl3),
                                     grep("minor", bioregions.lvl3$clvl3)), ]

anthroregions.lvl2 <- anthroregions.lvl2[c(1:(grep("minor", anthroregions.lvl2$clvl2) - 1),
                                        (grep("minor", anthroregions.lvl2$clvl2) + 1):nrow(anthroregions.lvl2),
                                        grep("minor", anthroregions.lvl2$clvl2)), ]
anthroregions.lvl3 <- anthroregions.lvl3[c((grep("minor", anthroregions.lvl3$clvl3) + 1):nrow(anthroregions.lvl3),
                                           grep("minor", anthroregions.lvl3$clvl3)), ]

introregions.lvl2 <- introregions.lvl2[c(1:(grep("minor", introregions.lvl2$clvl2) - 1),
                                           (grep("minor", introregions.lvl2$clvl2) + 1):nrow(introregions.lvl2),
                                           grep("minor", introregions.lvl2$clvl2)), ]

introregions.lvl3 <- introregions.lvl3[c(1:(grep("minor", introregions.lvl3$clvl3) - 1),
                                         (grep("minor", introregions.lvl3$clvl3) + 1):nrow(introregions.lvl3),
                                         grep("minor", introregions.lvl3$clvl3)), ]

native.extregions.lvl3 <- native.extregions.lvl3[c(1:(grep("minor", native.extregions.lvl3$clvl3) - 1),
                                     (grep("minor", native.extregions.lvl3$clvl3) + 1):nrow(native.extregions.lvl3),
                                     grep("minor", native.extregions.lvl3$clvl3)), ]


# Maps native -------------------------------------------------------------

{
thememaps <- theme(legend.key.size = unit(1, "cm"),
                   legend.text = element_text(size = 18),
                   axis.text = element_text(size = 18),
                   axis.title = element_text(size = 18),
                   legend.title = element_text(size = 18),
                   panel.grid = element_blank(),
                   line = element_blank(),
                   rect = element_blank())

# Level 1
pnative1 <- ggplot() +
  geom_sf(data = gr, color = 'grey') +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9)) +
  geom_sf(data = natregionslvl1.w, aes(fill = color),
          col = grey(.2),
          size = .5) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_identity(name = "",
                      labels = as.character(bioregions.lvl1$name),
                      guide = guide_legend(override.aes = list(
                        fill = as.character(bioregions.lvl1$col.native.lvl1)),
                        order = 1)) +
  thememaps


# Native level 2
pnative2 <- ggplot() +
  geom_sf(data = gr, color = 'grey') +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9)) +
  geom_sf(data = natregionslvl2.w, aes(fill = color),
          col = grey(.2),
          size = .5) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_identity(name = "",
                      labels = as.character(bioregions.lvl2$name),
                      guide = guide_legend(override.aes = list(
                        fill = as.character(bioregions.lvl2$col.native.lvl2)),
                        order = 1)) +
  thememaps

# Native level 3
pnative3 <- ggplot() +
  geom_sf(data = gr, color = 'grey') +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9)) +
  geom_sf(data = natregionslvl3.w, aes(fill = color),
          col = grey(.2),
          size = .5) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_identity(name = "",
                      labels = as.character(bioregions.lvl3$name),
                      guide = guide_legend(override.aes = list(
                        fill = as.character(bioregions.lvl3$col.native.lvl3)),
                        order = 1)) +
  thememaps

# Maps Anthropocene -------------------------------------------------------------
# Anthropocene level 1
panthro1 <- ggplot() +
  geom_sf(data = gr, color = 'grey') +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9)) +
  geom_sf(data = antregionslvl1.w, aes(fill = color),
          col = grey(.2),
          size = .5) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_identity(name = "",
                      labels = as.character(anthroregions.lvl1$name),
                      guide = guide_legend(override.aes = list(
                        fill = as.character(anthroregions.lvl1$col.all.lvl1)),
                        order = 1)) +
  thememaps

# Plotting only subregions of Anthropocenian
panthro2 <- ggplot() +
  geom_sf(data = gr, color = 'grey') +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9)) +
  geom_sf(data = antregionslvl2.w,
          aes(fill = color),
          col = grey(.2),
          size = .5) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_identity(name = "",
                      labels = as.character(anthroregions.lvl2$name),
                      guide = guide_legend(override.aes = list(
                        fill = as.character(anthroregions.lvl2$col.all.lvl2)),
                        order = 1,
                        ncol = 2)) +
  thememaps

panthro3 <- ggplot() +
  geom_sf(data = gr, color = 'grey') +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9)) +
  geom_sf(data = antregionslvl3.w,
          aes(fill = color),
          col = grey(.2),
          size = .5) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_identity(name = "",
                      labels = as.character(anthroregions.lvl3$name),
                      guide = guide_legend(override.aes = list(
                        fill = as.character(anthroregions.lvl3$col.all.lvl3)),
                        order = 1)) +
  thememaps

# Maps Introduction only -------------------------------------------------------------
# Level 1
pintro1 <- ggplot() +
  geom_sf(data = gr, color = 'grey') +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9)) +
  geom_sf(data = intregionslvl1.w, aes(fill = color),
          col = grey(.2),
          size = .5) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_identity(name = "",
                      labels = as.character(introregions.lvl1$name),
                      guide = guide_legend(override.aes = list(
                        fill = as.character(introregions.lvl1$col.intro.lvl1)),
                        order = 1)) +
  thememaps

# Level 2
pintro2 <- ggplot() +
  geom_sf(data = gr, color = 'grey') +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9)) +
  geom_sf(data = intregionslvl2.w,
          aes(fill = color),
          col = grey(.2),
          size = .5) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_identity(name = "",
                      labels = as.character(introregions.lvl2$name),
                      guide = guide_legend(override.aes = list(
                        fill = as.character(introregions.lvl2$col.intro.lvl2)),
                        order = 1,
                        ncol = 2)) +
  thememaps


# Level 3
pintro3 <- ggplot() +
  geom_sf(data = gr, color = 'grey') +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9)) +
  geom_sf(data = intregionslvl3.w,
          aes(fill = color),
          col = grey(.2),
          size = .5) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_identity(name = "",
                      labels = as.character(introregions.lvl3$name),
                      guide = guide_legend(override.aes = list(
                        fill = as.character(introregions.lvl3$col.intro.lvl3)),
                        order = 1)) +
  thememaps


# Maps Extinctions only -------------------------------------------------------------
# Level 1
pext1 <- ggplot() +
  geom_sf(data = gr, color = 'grey') +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9)) +
  geom_sf(data = extregionslvl1.w, aes(fill = color),
          col = grey(.2),
          size = .5) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_identity(name = "",
                      labels = as.character(native.extregions.lvl1$name),
                      guide = guide_legend(override.aes = list(
                        fill = as.character(native.extregions.lvl1$col.native.ext.lvl1)),
                        order = 1)) +
  thememaps

# Level 2
pext2 <- ggplot() +
  geom_sf(data = gr, color = 'grey') +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9)) +
  geom_sf(data = extregionslvl2.w,
          aes(fill = color),
          col = grey(.2),
          size = .5) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_identity(name = "",
                      labels = as.character(native.extregions.lvl2$name),
                      guide = guide_legend(override.aes = list(
                        fill = as.character(native.extregions.lvl2$col.native.ext.lvl2)),
                        order = 1)) +
  thememaps

# Level 3
pext3 <- ggplot() +
  geom_sf(data = gr, color = 'grey') +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9)) +
  geom_sf(data = extregionslvl3.w,
          aes(fill = color),
          col = grey(.2),
          size = .5) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_identity(name = "",
                      labels = as.character(native.extregions.lvl3$name),
                      guide = guide_legend(override.aes = list(
                        fill = as.character(native.extregions.lvl3$col.native.ext.lvl3)),
                        order = 1)) +
  thememaps
}

cairo_pdf("./outputs/maps_anthropocene.pdf", width = 32, height = 12,
          pointsize = 6)
egg::ggarrange(pnative1, pnative2, pnative3, panthro1, panthro2, panthro3,
               labels = c("a. Natural regions, level 1 clusters",
                          "b. Natural regions, level 2 clusters",
                          "c. Natural regions, level 3 clusters",
                          "d. Anthropocene regions, level 1 clusters",
                          "e. Anthropocene regions, level 2 clusters",
                          "f. Anthropocene regions, level 3 clusters"),
               newpage = FALSE,
               nrow = 2,
               label.args = list(gp = grid::gpar(font = 4, cex = 3)))
dev.off()

cairo_pdf("./outputs/maps_introonly_nativeext.pdf", width = 32, height = 12,
          pointsize = 6)
egg::ggarrange(pext1, pext2, pext3, pintro1, pintro2, pintro3,
               labels = c("a. Natural regions and extirpations, level 1 clusters",
                          "b. Natural regions and extirpations, level 2 clusters",
                          "c. Natural regions and extirpations, level 3 clusters",
                          "d. Natural regions and introductions, level 1 clusters",
                          "e. Natural regions and introductions, level 2 clusters",
                          "f. Natural regions and introductions, level 3 clusters"),
               newpage = FALSE,
               nrow = 2,
               label.args = list(gp = grid::gpar(font = 4, cex = 3)))
dev.off()


png("./outputs/regions_native_lvl1.png", h = 1300, w = 1600)
pnative1
dev.off()

png("./outputs/regions_native_lvl2.png", h = 1300, w = 1600)
pnative2
dev.off()

png("./outputs/regions_native_lvl3.png", h = 1300, w = 1600)
pnative3
dev.off()


png("./outputs/regions_all_lvl1.png", h = 1300, w = 1600)
panthro1
dev.off()

png("./outputs/regions_all_lvl2.png", h = 1300, w = 1600)
panthro2
dev.off

png("./outputs/regions_all_lvl3.png", h = 1300, w = 1600)
panthro3
dev.off()


png("./outputs/richness_difference.png", h = 1600, w = 1300)
egg::ggarrange(prnative, pranthro)
dev.off()

# Removing problematic nodes that do not belong to lvl2 group
# if(any(is.na(all.net$col.all.lvl2)))
# {
#   all.net$col.all.lvl2[which(is.na(all.net$col.all.lvl2))] <- "#808080"
# }

# Writing networks
writeGDF(fish.native, native.net, site.field = "X1.Basin.Name", species.field = "X6.Fishbase.Valid.Species.Name", color.field = "col.native.lvl2",
         filename = "./data/gephinetworks/fish.native.gdf")

writeGDF(fish.all, all.net, site.field = "X1.Basin.Name", species.field = "X6.Fishbase.Valid.Species.Name", color.field = "col.all.lvl1",
         filename = "./data/gephinetworks/fish.all.gdf")

writeGDF(fish.intro, intro.net, site.field = "X1.Basin.Name", species.field = "X6.Fishbase.Valid.Species.Name", color.field = "col.intro.lvl1",
         filename = "./data/gephinetworks/fish.intro.gdf")

writeGDF(fish.native.ext, native.ext.net, site.field = "X1.Basin.Name", species.field = "X6.Fishbase.Valid.Species.Name", color.field = "col.native.ext.lvl2",
         filename = "./data/gephinetworks/fish.native.ext.gdf")

saveRDS(native.net, "./data/nativenetwork.RDS")
saveRDS(all.net, "./data/allnetwork.RDS")
saveRDS(intro.net, "./data/intronetwork.RDS")
saveRDS(native.ext.net, "./data/native.extnetwork.RDS")
saveRDS(fish.intro, "./data/fish_intro.rds")
saveRDS(basinshp, "./data/shapefile_regions.rds")

saveRDS(natregionslvl1, "./data/region shapefiles/native_regions_lvl1.RDS")
saveRDS(natregionslvl1.w, "./data/region shapefiles/native_regions_lvl1_wintri.RDS")
saveRDS(natregionslvl2, "./data/region shapefiles/native_regions_lvl2.RDS")
saveRDS(natregionslvl2.w, "./data/region shapefiles/native_regions_lvl2_wintri.RDS")
saveRDS(natregionslvl3, "./data/region shapefiles/native_regions_lvl3.RDS")
saveRDS(natregionslvl3.w, "./data/region shapefiles/native_regions_lvl3_wintri.RDS")

saveRDS(antregionslvl1, "./data/region shapefiles/anthropocene_regions_lvl1.RDS")
saveRDS(antregionslvl1.w, "./data/region shapefiles/anthropocene_regions_lvl1_wintri.RDS")
saveRDS(antregionslvl2, "./data/region shapefiles/anthropocene_regions_lvl2.RDS")
saveRDS(antregionslvl2.w, "./data/region shapefiles/anthropocene_regions_lvl2_wintri.RDS")
saveRDS(antregionslvl3, "./data/region shapefiles/anthropocene_regions_lvl3.RDS")
saveRDS(antregionslvl3.w, "./data/region shapefiles/anthropocene_regions_lvl3_wintri.RDS")

saveRDS(intregionslvl1, "./data/region shapefiles/anthropocene_regions_lvl1.RDS")
saveRDS(intregionslvl1.w, "./data/region shapefiles/anthropocene_regions_lvl1_wintri.RDS")
saveRDS(intregionslvl2, "./data/region shapefiles/anthropocene_regions_lvl2.RDS")
saveRDS(intregionslvl2.w, "./data/region shapefiles/anthropocene_regions_lvl2_wintri.RDS")
saveRDS(intregionslvl3, "./data/region shapefiles/anthropocene_regions_lvl3.RDS")
saveRDS(intregionslvl3.w, "./data/region shapefiles/anthropocene_regions_lvl3_wintri.RDS")

saveRDS(extregionslvl1, "./data/region shapefiles/anthropocene_regions_lvl1.RDS")
saveRDS(extregionslvl1.w, "./data/region shapefiles/anthropocene_regions_lvl1_wintri.RDS")
saveRDS(extregionslvl2, "./data/region shapefiles/anthropocene_regions_lvl2.RDS")
saveRDS(extregionslvl2.w, "./data/region shapefiles/anthropocene_regions_lvl2_wintri.RDS")
saveRDS(extregionslvl3, "./data/region shapefiles/anthropocene_regions_lvl3.RDS")
saveRDS(extregionslvl3.w, "./data/region shapefiles/anthropocene_regions_lvl3_wintri.RDS")

saveRDS(bioregions.lvl1, "./data/bioregions_lvl1")
saveRDS(bioregions.lvl2, "./data/bioregions_lvl2")
saveRDS(bioregions.lvl3, "./data/bioregions_lvl3")
saveRDS(anthroregions.lvl1, "./data/anthroregions_lvl1")
saveRDS(anthroregions.lvl2, "./data/anthroregions_lvl2")
saveRDS(anthroregions.lvl3, "./data/anthroregions_lvl3")
saveRDS(introregions.lvl1, "./data/introregions_lvl1")
saveRDS(introregions.lvl2, "./data/introregions_lvl2")
saveRDS(introregions.lvl3, "./data/introregions_lvl3")
saveRDS(native.extregions.lvl1, "./data/native.extregions_lvl1")
saveRDS(native.extregions.lvl2, "./data/native.extregions_lvl2")
saveRDS(native.extregions.lvl3, "./data/native.extregions_lvl3")
