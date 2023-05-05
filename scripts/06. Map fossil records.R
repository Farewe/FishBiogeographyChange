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

# Add fish names in basins_ana
basins_ana$introduced.species <- NA
for (basin in names(top.fish.per.basin)) {
  basins_ana$introduced.species[basins_ana$BasinName == basin] <- 
    paste0("- *", paste0(top.fish.per.basin[[basin]], collapse = "*<br>- *"),
           "*")
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
  paste0("**", basins_ana$basin_clean, "**<br><br>• ", 
         basins_ana$richness.native,
         " native species <br>• ",
         basins_ana$n_introduced, 
         " introduced species<br>• ",
         " ↓", round(basins_ana$endemism.change * 100, 1),
         "% endemism <br>",
         "<br>Main introduced species:<br>",
         basins_ana$introduced.species)

# Danube & Zambezi have 1 extirpation
dz <- which(basins_ana$BasinName %in%
              c("Danube", "Zambezi"))
basins_ana$text[dz] <-
  paste0("**", basins_ana$basin_clean[dz], "**<br><br>• ", 
         basins_ana$richness.native[dz],
         " native species <br>• ",
         basins_ana$n_introduced[dz], 
         " introduced species<br>• 1 extirpated species<br>• ",
         " ↓", round(basins_ana$endemism.change[dz] * 100, 1),
         "% endemism <br>",
         "<br>Main introduced species:<br>",
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
               fill = "cornsilk",
               maxwidth = unit(0.1, "in")) +
  coord_sf(clip = "off", datum = NA,
           xlim = c(-15e6, 19e6),
           ylim = c(-8e6, 11e6)) +
  theme_void() +
  theme(legend.position = c(0.7, 1),
        panel.border = element_blank(),
        plot.margin = margin(0.35, 0.5, 0.35, 0.5),
        legend.text = element_text(size=15),
        legend.key.size = unit(0.8, 'cm'), 
        legend.key.height = unit(0.8, 'cm'), 
        legend.key.width = unit(0.8, 'cm'))  +
  scale_fill_identity(name = "",
                      labels = as.character(anthroregions.lvl1$name),
                      guide = guide_legend(override.aes = list(
                        fill = as.character(anthroregions.lvl1$col.all.lvl1)),
                        order = 1))
dev.off()
