library(sf)
library(lwgeom)

basins <- readRDS("./data/shapefile_regions.rds")

list_anabasins <- read.csv("data/ListBasinsAnabranch.csv", sep = ";")

basins_ana <- basins[which(basins$BasinName %in% list_anabasins$X1.Basin.Name), ]

# plot(basins_ana["Native_level2"])

# Keeping only the important stats
basins_ana <- basins_ana[, c("BasinName",
                             "richness.native",
                             "richness.anthro",
                             "richness.intro",
                             "richness.native.ext",
                             "Native_level2",
                             "Intro_level1")]

# Calculate number of introduced species
basins_ana$n_introduced <- basins_ana$richness.intro - basins_ana$richness.native 

# Remove basins with no introductions
basins_ana <- basins_ana[-which(basins_ana$n_introduced == 0), ]

basins_ana <- st_transform_proj(basins_ana,
                                crs = "+proj=wintri +datum=WGS84 +no_defs +over")


# Get center points of each basin for text boxes
basins_ana$mid_x <- sapply(1:nrow(basins_ana),
                           function(x) {
                             a <- st_bbox(basins_ana[x, ])[c("xmin", "xmax")]
                             diff(a) / 2 + a[1]
                           })

basins_ana$mid_y <- sapply(1:nrow(basins_ana),
                           function(x) {
                             a <- st_bbox(basins_ana[x, ])[c("ymin", "ymax")]
                             diff(a) / 2 + a[1]
                           })

# Small adjustments to make dots more visible
basins_ana$mid_x[basins_ana$BasinName == "Mekong"] <- 8897647
basins_ana$mid_y[basins_ana$BasinName == "Mekong"] <- 2512820

basins_ana$mid_x[basins_ana$BasinName == "Murray.Darling"] <- 12506764


# Filter basins to only keep one per natural region, the one with the largest
# nb of introductions
basins_ana <- basins_ana[order(basins_ana$n_introduced, decreasing = TRUE), ]

# Choose Danube instead of Loire because the native richness is much more 
# important
basins_ana <- basins_ana[-which(basins_ana$BasinName == "Loire"), ]
  
basins_ana <- basins_ana[-which(duplicated(basins_ana$Native_level2)), ]


# Find which species were introduced in that basin
fish.intro <- readRDS("./data/fish_intro.rds")
fish.intro <- fish.intro[fish.intro$X3.Native.Exotic.Status == "exotic", ]

fish.intro <- fish.intro[which(fish.intro$X1.Basin.Name %in% 
                                 basins_ana$BasinName), ]

fish.per.basin <- lapply(basins_ana$BasinName,
                         function(x) fish.intro$X6.Fishbase.Valid.Species.Name[
                           which(fish.intro$X1.Basin.Name %in% x)
                         ])
names(fish.per.basin) <- basins_ana$BasinName

intro.chars <- readRDS("./outputs/introduced_species_characteristics.RDS")

top.fish.per.basin <- lapply(fish.per.basin,
                             function(x) {
                               as.character(
                                 x[which(x %in% intro.chars$species[1:13])])
                             })


# Get bioregion colours
bioregions.lvl2 <- readRDS("./data/bioregions_lvl2")
# Species colours
sp.col <- intro.chars[1:13, c("species", "native.bioregion")]
sp.col$color <- bioregions.lvl2$col.native.lvl2[match(
  sp.col$native.bioregion,
  bioregions.lvl2$name
)]

top.fish.per.basin.vern <- top.fish.per.basin
for(basin in 1:length(top.fish.per.basin))
{
  top.fish.per.basin.vern[[basin]] <- 
    gsub('Carassius auratus', 
       '<span style="color:#66C2A5">**Goldfish**</span>',
       top.fish.per.basin.vern[[basin]])
  
  top.fish.per.basin.vern[[basin]] <- 
    gsub('Ctenopharyngodon idella', 
         '<span style="color:#66C2A5">**Grass carp**</span>',
         top.fish.per.basin.vern[[basin]])
  
  top.fish.per.basin.vern[[basin]] <- 
    gsub('Cyprinus carpio', 
         '<span style="color:#E5C494">**European carp**</span>',
         top.fish.per.basin.vern[[basin]])
  
  top.fish.per.basin.vern[[basin]] <- 
    gsub('Gambusia holbrooki', 
         '<span style="color:#FFD92F">**Eastern mosquitofish**</span>',
         top.fish.per.basin.vern[[basin]])
  
  top.fish.per.basin.vern[[basin]] <- 
    gsub('Hypophthalmichthys molitrix', 
         '<span style="color:#66C2A5">**Silver carp**</span>',
         top.fish.per.basin.vern[[basin]])
  
  top.fish.per.basin.vern[[basin]] <- 
    gsub('Oreochromis mossambicus', 
         '<span style="color:#FC8D62">**Mozambique tilapia**</span>',
         top.fish.per.basin.vern[[basin]])
  
  top.fish.per.basin.vern[[basin]] <- 
    gsub('Oreochromis niloticus', 
         '<span style="color:#FC8D62">**Nile tilapia**</span>',
         top.fish.per.basin.vern[[basin]])
  
  top.fish.per.basin.vern[[basin]] <- 
    gsub('Poecilia reticulata', 
         '<span style="color:#A6D854">**Guppy**</span>',
         top.fish.per.basin.vern[[basin]])
  
  top.fish.per.basin.vern[[basin]] <- 
    gsub('Micropterus salmoides', 
         '<span style="color:#FFD92F">**Largemouth bass**</span>',
         top.fish.per.basin.vern[[basin]])
  
  top.fish.per.basin.vern[[basin]] <- 
    gsub('Gambusia affinis', 
         '<span style="color:#FFD92F">**Mosquitofish**</span>',
         top.fish.per.basin.vern[[basin]])
  
  top.fish.per.basin.vern[[basin]] <- 
    gsub('Lepomis gibbosus', 
         '<span style="color:#FFD92F">**Pumpkinseed**</span>',
         top.fish.per.basin.vern[[basin]])
  
  top.fish.per.basin.vern[[basin]] <- 
    gsub('Perca fluviatilis', 
         '<span style="color:#E5C494">**European perch**</span>',
         top.fish.per.basin.vern[[basin]])
  
}

# Add fish names in basins_ana
basins_ana$introduced.species <- NA
for (basin in names(top.fish.per.basin)) {
  basins_ana$introduced.species[basins_ana$BasinName == basin] <- 
    paste0("<span style='color:#E6E6E6'> - </span> ", 
           paste0(top.fish.per.basin.vern[[basin]], 
                  collapse = "<br><span style='color:#E6E6E6'> - </span> "),
           "")
}

# Add info on endemism
site_metrics <- readRDS("data/metrics_per_site_wide.RDS")
site_metrics <- site_metrics[site_metrics$native.site %in% basins_ana$BasinName, ]
basins_ana$endemism.change <- site_metrics$diff.pc.endemism[
  match(basins_ana$BasinName,
        site_metrics$native.site)]

# Rename Murray Darling
basins_ana$basin_clean <- basins_ana$BasinName
basins_ana$basin_clean[basins_ana$basin_clean == "Murray.Darling"] <-
  "Murray-Darling"

# Create text for text boxes
basins_ana$text <-
  paste0('**<span style="color:#FCFCFC">',
         basins_ana$basin_clean,
         " basin</span>**<br><br><span style=color:#FCFCFC>• ", 
         basins_ana$richness.native,
         " native species <br>• ",
         basins_ana$n_introduced, 
         " introduced species<br>• ",
         " ↓", round(basins_ana$endemism.change * 100, 1),
         "% endemism </span><br>",
         "<br><span style=color:#FCFCFC>Main introduced species:</span><br>",
         basins_ana$introduced.species)

# Danube & Zambezi have 1 extirpation
dz <- which(basins_ana$BasinName %in%
              c("Danube", "Zambezi"))
basins_ana$text[dz] <-
  paste0('**<span style="color:#FCFCFC">',
         basins_ana$basin_clean[dz], 
         " basin</span>**<br><br><span style=color:#FCFCFC>• ", 
         basins_ana$richness.native[dz],
         " native species <br>• ",
         basins_ana$n_introduced[dz], 
         " introduced species<br>• 1 extirpated species<br>• ",
         " ↓", round(basins_ana$endemism.change[dz] * 100, 1),
         "% endemism  </span><br>",
         "<br><span style=color:#FCFCFC>Main introduced species:</span><br>",
         basins_ana$introduced.species[dz])


# Prepare map layers
wm <- st_read("./data/sig data/ne_50m_land.shp")
wm <- st_transform_proj(wm,
                        crs = "+proj=wintri +datum=WGS84 +no_defs +over")

antregionslvl1.w <- readRDS("./data/region shapefiles/anthropocene_regions_lvl1_wintri.RDS")

anthroregions.lvl1 <- readRDS("./data/anthroregions_lvl1")

# Define y pos of text boxes for southern/northern hemisphere basins
# "Mississippi"    "Mekong"         "Parana"         "Danube"        
# "Zambezi"        "Murray.Darling"
basins_ana$y_pos <- c(5e6, 5e6, -7e6, 11.5e6, -7.5e6, -7e6)
basins_ana$x_pos <- c(-14e6, 14.5e6, -12e6, -2e6, 4e6, 18e6)

leg_col <- data.frame(x = 14000000,
                      y = 12000000,
                      text = paste0("<span style=color:#FCFCFC>Text color indicates<br>",
                                    "species region of origin</span> <br><br>",
                                    "<span style=color:#66C2A5> **Sino-Oriental** </span><br>",
                                    "<span style=color:#FC8D62> **Ethiopian** </span><br>",
                                    "<span style=color:#E5C494> **Palearctic** </span><br>",
                                    "<span style=color:#A6D854> **Neotropical** </span><br>",
                                    "<span style=color:#FFD92F> **Nearctic** </span><br>"))

library(ggplot2)
library(ggtext)
cairo_pdf("./outputs/Figure 6.pdf", width = 16, height = 12,
          pointsize = 6)
ggplot() +
  geom_sf(data = wm, col = NA, fill = grey(.9))  +
  geom_sf(data = antregionslvl1.w, aes(fill = color),
          col = NA,
          size = .5) +
  geom_sf(data = basins_ana,
          fill = grey(.8, .6),
          col = grey(.2))  + 
  geom_point(data = basins_ana,
             aes(x = mid_x, y = mid_y)) +
  geom_segment(data = basins_ana,
               aes(x = mid_x, y = mid_y,
                   xend = x_pos, yend = y_pos)) + 
  geom_textbox(data = basins_ana, 
               aes(x = x_pos,
                   y = y_pos,
                   label = text),
               width = NULL,
               fill = "#404040",
               maxwidth = unit(0.1, "in")) +
  geom_textbox(data = leg_col, 
               aes(x = x,
                   y = y,
                   label = text),
               width = NULL,
               fill = "#404040",
               maxwidth = unit(0.1, "in")) +
  coord_sf(clip = "off", datum = NA,
           xlim = c(-15e6, 19e6),
           ylim = c(-8e6, 11e6)) +
  theme_void() +
  theme(legend.position = c(0.7, 1),
        panel.border = element_blank(),
        plot.margin = margin(0.35, 0.5, 0.35, 0.5),
        legend.text = element_text(size=14),
        legend.title = element_text(size = 14),
        legend.key.size = unit(0.8, 'cm'), 
        legend.key.height = unit(0.8, 'cm'), 
        legend.key.width = unit(0.8, 'cm'))  +
  scale_fill_identity(name = "Map colors",
                      labels = as.character(anthroregions.lvl1$name),
                      guide = guide_legend(override.aes = list(
                        fill = as.character(anthroregions.lvl1$col.all.lvl1)),
                        order = 1))
dev.off()
