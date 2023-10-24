library(sf)
library(ggplot2)

natregions <- readRDS("./data/region shapefiles/native_regions_lvl2.RDS")
antregions <- readRDS("./data/region shapefiles/anthropocene_regions_lvl1.RDS")

bioregions.lvl2 <- readRDS("./data/bioregions_lvl2")
anthroregions.lvl1 <- readRDS("./data/anthroregions_lvl1")


natregions <- st_transform(natregions,
                           crs = "+proj=wintri +datum=WGS84 +no_defs +over") 
antregions <- st_transform(antregions,
                           crs = "+proj=wintri +datum=WGS84 +no_defs +over") 

wm <- st_read("./data/sig data/ne_50m_land.shp")
wm <- st_transform(wm, 
                   crs = "+proj=wintri +datum=WGS84 +no_defs +over") 

gr <- sf::st_graticule(lat = c(-89.9,seq(-80,80,20), 89.9))
gr <- st_transform(gr,
                   crs = "+proj=wintri +datum=WGS84 +no_defs +over")


thememaps <- theme(legend.key.size = unit(1, "cm"),
                   legend.text = element_text(size = 18),
                   axis.text = element_text(size = 18),
                   axis.title = element_text(size = 18),
                   legend.title = element_text(size = 18),
                   panel.grid = element_blank(),
                   line = element_blank(),
                   rect = element_blank(),
                   legend.position = "bottom")


bioregions.lvl2$name <- c("Sino-Orientale", "Ethiopienne", "Paléarctique",
                          "Australienne", "Néotropicale", "Néarctique", "Mineures")


pnative2 <- ggplot() +
  geom_sf(data = gr, color = grey(.95)) +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9)) +
  geom_sf(data = natregions[natregions$color != "#6E6E6E", ], aes(fill = color),
          col = grey(.2),
          size = .5) +
  coord_sf(datum = NA,
           ylim = c(-6.5e6, 8.7e6)) +
  theme_minimal() +
  thememaps +
  scale_fill_identity(name = "",
                      labels = as.character(bioregions.lvl2$name)[1:6],
                      guide = guide_legend(override.aes = list(
                        fill = as.character(bioregions.lvl2$col.native.lvl2)[1:6]),
                        order = 1))


cairo_pdf("outputs/press1.pdf", width = 10, height = 6)
pnative2
dev.off()

anthroregions.lvl1$name <- c("PAGNEA", "Néotropicale",
                             "Néo-Orientale", "Ethiopienne",
                             "Mineures")

panthro1 <- ggplot() +
  geom_sf(data = gr, color = grey(.95)) +
  geom_sf(data = wm, col = grey(.6), fill = grey(.9)) +
  geom_sf(data = antregions[antregions$color != "#6E6E6E", ], aes(fill = color),
          col = grey(.2),
          size = .5) +
  coord_sf(datum = NA,
           ylim = c(-6.5e6, 8.7e6)) +
  theme_minimal() +
  thememaps +
  scale_fill_identity(name = "",
                      labels = as.character(anthroregions.lvl1$name)[1:4],
                      guide = guide_legend(override.aes = list(
                        fill = as.character(anthroregions.lvl1$col.native.lvl2)[1:4]),
                        order = 1))

cairo_pdf("outputs/press2.pdf", width = 10, height = 6)
panthro1
dev.off()
