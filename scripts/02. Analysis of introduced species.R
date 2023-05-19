library(tibble)
library(biogeonetworks)
library(ggplot2)
library(sf)
# ---- Chargement des données ----
fish.native <- readRDS("./data/fish_native.rds")
fish.all <- readRDS("./data/fish_anthropocene.rds")
fish.intro <- readRDS("./data/fish_intro.rds")
fish.native.ext <- readRDS("./data/fish_native_extinctions.rds")


# Maps
basinshp <- readRDS("./data/shapefile_regions.rds")
wm <- st_read("./data/sig data/ne_50m_coastline.shp")

# Chargement des réseaux
native.net <- as_tibble(readRDS("./data/nativenetwork.RDS"))
all.net <- as_tibble(readRDS("./data/allnetwork.RDS"))
intro.net <- as_tibble(readRDS("./data/intronetwork.RDS"))
native.ext.net <- as_tibble(readRDS("./data/native.extnetwork.RDS"))

bioregions.lvl1 <- readRDS("./data/bioregions_lvl1")
bioregions.lvl2 <- readRDS("./data/bioregions_lvl2")
bioregions.lvl3 <- readRDS("./data/bioregions_lvl2")

native.net$region <- bioregions.lvl2$name[match(native.net$clvl2,
                                                bioregions.lvl2$clvl2)]

# Attribution des régions natives aux bassins de la base avec exotiques
fish.all$basin.region <- native.net$region[match(fish.all$X1.Basin.Name,
                                                 native.net$Name)]
fish.all$basin.cluster <- native.net$clvl2[match(fish.all$X1.Basin.Name,
                                                 native.net$Name)]

# Attribution des régions natives auxquelles les espèces ont été assignées dans la base avec exotiques
fish.all$species.region <- native.net$region[match(fish.all$X6.Fishbase.Valid.Species.Name,
                                                   native.net$Name)]
# Idem avec les couleurs
fish.all$species.region.colors <- native.net$col.native.lvl2[match(fish.all$X6.Fishbase.Valid.Species.Name,
                                                                   native.net$Name)]
new.basins.intro <- fish.all$X1.Basin.Name[which(is.na(fish.all$basin.region))]



# ---- Basins where there was no native species info ----
# library("ggrepel")
# pdf("./outputs/new_basins_with_introduced.pdf", width = 100, height = 67)
# for(reg in unique(basinshp$Ecoregion[basinshp$BasinName %in% new.basins.intro]))
# {
#   p <- ggplot() +
#     geom_sf(data = wm, fill = NA, color = gray(.5)) +
#     geom_sf(data = basinshp[basinshp$BasinName %in% new.basins.intro &
#                               basinshp$Ecoregion %in% reg, 1],
#             fill = "blue") +
#     geom_text_repel(data = basinshp[basinshp$BasinName %in% new.basins.intro &
#                                       basinshp$Ecoregion %in% reg, ],
#                     aes(x = Med_Longit, y = Med_Latit, label = BasinName),
#                     fontface = "bold") +
#     theme_minimal() +
#     ggtitle(reg)
#   print(p)
# }
# dev.off()

# Correction to basins that were not in the native dataset

# Ethiopian
ethiopian.basins <- new.basins.intro[which(new.basins.intro %in%
                                             basinshp$BasinName[which(basinshp$Ecoregion %in% "Afrotropic")])]
ethiopian.basins <- c(ethiopian.basins,
                      "Chenini", "Tunisia.un.1", "Inner.Harbor", "Wadi.Nashu")
all(ethiopian.basins %in% new.basins.intro)

# Palearctic
palearctic.basins <- c("Taravo", "San.Giovanni", "Asinaro")
all(palearctic.basins %in% new.basins.intro)

# Sino-Oriental
sino.oriental.basins <- c("Tigil", "Kronockaja", "Maekawa") #, "Adako.un", "Azuri") # Older basins
all(sino.oriental.basins %in% new.basins.intro)

# Australian
australian.basins <- new.basins.intro[which(new.basins.intro %in%
                                              basinshp$BasinName[which(basinshp$Ecoregion %in% "Australasia")])]
all(australian.basins %in% new.basins.intro)


# Nearctic
nearctic.basins <- new.basins.intro[which(new.basins.intro %in%
                                            basinshp$BasinName[which(basinshp$Ecoregion %in% "Nearctic")])]
nearctic.basins <- c(nearctic.basins, "San.Jose.del.Cabo")
all(nearctic.basins %in% new.basins.intro)

# Neotropical
neotropical.basins <- c("Anasco", "Capot", "Pilote")
all(neotropical.basins %in% new.basins.intro)

# Others
other.basins <- new.basins.intro[which(new.basins.intro %in%
                                         basinshp$BasinName[which(basinshp$Ecoregion %in% "Oceania")])]


fish.all$basin.region[which(fish.all$X1.Basin.Name %in% ethiopian.basins)] <- "Ethiopian (1.2)"
fish.all$basin.region[which(fish.all$X1.Basin.Name %in% palearctic.basins)] <- "Palearctic (1.3)"
fish.all$basin.region[which(fish.all$X1.Basin.Name %in% sino.oriental.basins)] <- "Sino-Oriental (1.1)"
fish.all$basin.region[which(fish.all$X1.Basin.Name %in% nearctic.basins)] <- "Nearctic (2.2)"
fish.all$basin.region[which(fish.all$X1.Basin.Name %in% neotropical.basins)] <- "Neotropical (2.1)"
fish.all$basin.region[which(fish.all$X1.Basin.Name %in% australian.basins)] <- "Australian (1.4)"


fish.all$basin.cluster[which(fish.all$X1.Basin.Name %in% ethiopian.basins)] <- "1.2"
fish.all$basin.cluster[which(fish.all$X1.Basin.Name %in% palearctic.basins)] <- "1.3"
fish.all$basin.cluster[which(fish.all$X1.Basin.Name %in% sino.oriental.basins)] <- "1.1"
fish.all$basin.cluster[which(fish.all$X1.Basin.Name %in% nearctic.basins)] <- "2.2"
fish.all$basin.cluster[which(fish.all$X1.Basin.Name %in% neotropical.basins)] <- "2.1"
fish.all$basin.cluster[which(fish.all$X1.Basin.Name %in% australian.basins)] <- "1.4"


fish.all$X1.Basin.Name[which(is.na(fish.all$basin.cluster))]

fish.all$basin.region[which(is.na(fish.all$basin.region))] <- "Minor clusters"
fish.all$basin.cluster[which(is.na(fish.all$basin.cluster))] <- "minor.clusters"
fish.all$species.region[which(is.na(fish.all$species.region))] <- "Minor clusters"
# ---- Species without native occurrences ----
# 2 species do not have native occurrences, so we add them here manually in the native
# network
sp.no.native.occ <- NULL
for (sp in levels(fish.all$X6.Fishbase.Valid.Species.Name))
{
  if(all(fish.all$X3.Native.Exotic.Status[fish.all$X6.Fishbase.Valid.Species.Name == sp] == "exotic"))
  {
    sp.no.native.occ <- c(sp.no.native.occ,
                          sp)
  }
}
sp.no.native.occ

native.net <- dplyr::bind_rows(native.net,
                        data.frame(Groups = c("2:1", "2:2"),
                                   Codelength = NA,
                                   Name = c("Gambusia dominicensis", "Relictus solitarius"),
                                   id = NA,
                                   lvl1 = "2",
                                   lvl2 = c("2.1", "2.2"),
                                   lvl3 = c("Gambusia dominicensis", "Relictus solitarius"),
                                   lvl4 = NA,
                                   lvl5 = NA,
                                   lvl6 = NA,
                                   nodetype = "species",
                                   cluster.type.lvl1 = "major",
                                   col.native.lvl1 = bioregions.lvl1$col.native.lvl1[which(bioregions.lvl1$clvl1 == "2")],
                                   cluster.type.lvl2 = "major",
                                   col.native.lvl2 = bioregions.lvl2$col.native.lvl2[match(c("2.1", "2.2"),
                                                                                           bioregions.lvl2$clvl2)],
                                   cluster.type.lvl3 = "minor",
                                   col.native.lvl3 = "#000000",
                                   clvl1 = "2",
                                   clvl2 = c("2.1", "2.2"),
                                   clvl3 = "minor.clusters",
                                   region = c("Neotropical (2.1)", "Nearctic (2.2)")))

# ---- Metrics for all species ----
# Area of each basin
basin.area <- data.frame(name = basinshp$BasinName,
                         area = basinshp$Surf_area)

# Metrics of native species
lvl2metrics.native <- clusterMetrics(db = as.data.frame(fish.native),
                                     network = as.data.frame(native.net),
                                     site.field = "X1.Basin.Name",
                                     species.field = "X6.Fishbase.Valid.Species.Name",
                                     site.area = basin.area,
                                     level = "clvl2")

# Correction to endemism based on

lvl2metrics.native$species.stats <- lvl2metrics.native$species.stats[order(lvl2metrics.native$species.stats$DilVal, decreasing = TRUE), ]
lvl2metrics.native$species.stats[1:25, ]
lvl2metrics.native$species.stats$bioregion <- bioregions.lvl2$name[match(lvl2metrics.native$species.stats$cluster,
                                                                         bioregions.lvl2$clvl2)]
lvl2metrics.native$species.stats$color <- bioregions.lvl2$col.native.lvl2[match(lvl2metrics.native$species.stats$cluster,
                                                                         bioregions.lvl2$clvl2)]

ggplot(lvl2metrics.native$species.stats, aes(x = bioregion, y = IndVal)) +
  geom_boxplot()



# Metrics of natives + exotics: including introduced occurrences
lvl2metrics.all <- clusterMetrics(db = as.data.frame(fish.all),
                                  network = as.data.frame(native.net),
                                  site.field = "X1.Basin.Name",
                                  species.field = "X6.Fishbase.Valid.Species.Name",
                                  site.area = basin.area,
                                  level = "clvl2",
                                  manual.site.correction = data.frame(unique(fish.all[, c("X1.Basin.Name", "basin.cluster")])))


lvl2metrics.all$species.stats <- lvl2metrics.all$species.stats[order(lvl2metrics.all$species.stats$Occ.DilVal, decreasing = TRUE), ]
lvl2metrics.all$species.stats[1:25, ]
lvl2metrics.all$species.stats$bioregion <- bioregions.lvl2$name[match(lvl2metrics.all$species.stats$cluster,
                                                                         bioregions.lvl2$clvl2)]
lvl2metrics.all$species.stats$col.native.lvl2 <- bioregions.lvl2$col.native.lvl2[match(lvl2metrics.all$species.stats$cluster,
                                                                                bioregions.lvl2$clvl2)]

# All species native vs new indval & dilval
species.vals <- data.frame(lvl2metrics.all$species.stats[c("species", "cluster")])
species.vals$native.IndVal <- NA
species.vals$native.DilVal <- NA
species.vals$native.IndVal <- lvl2metrics.native$species.stats$Occ.IndVal[match(species.vals$species,
                                                                                lvl2metrics.native$species.stats$species)]
species.vals$native.DilVal <- lvl2metrics.native$species.stats$Occ.DilVal[match(species.vals$species,
                                                                                lvl2metrics.native$species.stats$species)]
species.vals$all.IndVal <- lvl2metrics.all$species.stats$Occ.IndVal[match(species.vals$species,
                                                                          lvl2metrics.all$species.stats$species)]
species.vals$all.DilVal <- lvl2metrics.all$species.stats$Occ.DilVal[match(species.vals$species,
                                                                          lvl2metrics.all$species.stats$species)]

species.metrics <- lvl2metrics.all$species.stats
species.metrics <- data.frame(anthropocene = species.metrics,
                              native = lvl2metrics.native$species.stats[match(species.metrics$species,
                                                                              lvl2metrics.native$species.stats$species), ])

species.metrics$endemism.status <- "No change"
species.metrics$endemism.status[which(species.metrics$native.Endemism & !species.metrics$anthropocene.Endemism)] <- "No longer endemic"
species.metrics$endemism.status[which(!species.metrics$native.Endemism & species.metrics$anthropocene.Endemism)] <- "Neo-endemic"

plyr::count(species.metrics$endemism.status)

species.metrics[which(species.metrics$endemism.status == "No longer endemic"), ]

# species.metrics[species.metrics$anthropocene.species %in% endemism.change, ]


# ---- Maps ----
# basinshp <- dplyr::bind_cols(basinshp,
#                              lvl2metrics.all$site.stats[match(basinshp$BasinName,
#                                                               lvl2metrics.all$site.stats$site),
#                                                         3:6])
#
# library(tmap)
# wm <- st_read("./data/sig data/ne_50m_land.shp")
#
# png("./outputs/Robustness_native.png", w = 1600, h = 900)
# tm_shape(wm, projection = "+proj=wintri") +
#   tm_fill(col = grey(.75)) +
#   tm_shape(basinshp, projection = "+proj=wintri") +
#   tm_polygons("Occ.Rg")
# dev.off()


# ---- Maps of biogeographical impact ----
# Change in endemism in local basins
site.metrics <- lvl2metrics.all$site.stats
site.metrics <- data.frame(anthropocene = site.metrics,
                           native = lvl2metrics.native$site.stats[match(site.metrics$site,
                                                          lvl2metrics.native$site.stats$site), ])

site.metrics$anthropocene.pc.endemism <- site.metrics$anthropocene.end.richness / site.metrics$anthropocene.richness
site.metrics$native.pc.endemism <- site.metrics$native.end.richness / site.metrics$native.richness

site.metrics$diff.pc.endemism <- site.metrics$native.pc.endemism - site.metrics$anthropocene.pc.endemism


changing.sites <- site.metrics$anthropocene.site[-which(site.metrics$diff.pc.endemism == 0)]

ggsite.metrics <- rbind.data.frame(data.frame(dataset = "Natural",
                                              lvl2metrics.native$site.stats),
                                   data.frame(dataset = "Anthropocene",
                                              lvl2metrics.all$site.stats))
ggsite.metrics$native.cluster <- native.net$region[match(ggsite.metrics$site,
                                                         native.net$Name)]
ggsite.metrics$native.cluster[which(is.na(ggsite.metrics$native.cluster))] <- "No native cluster"
ggsite.metrics$dataset <- factor(ggsite.metrics$dataset,
                                        levels = c("Natural", "Anthropocene"))
ggsite.metrics$native.cluster <- factor(ggsite.metrics$native.cluster,
                                        levels = c(bioregions.lvl2$name,
                                                   "No native cluster"))
ggsite.metrics$native.color <- native.net$col.native.lvl2[match(ggsite.metrics$site,
                                                                native.net$Name)]
ggsite.metrics$native.color[which(is.na(ggsite.metrics$native.color))] <- "#000000"

ggsite.metrics$pc.endemism <- ggsite.metrics$end.richness / ggsite.metrics$richness


saveRDS(ggsite.metrics, "data/metrics_per_site_long.RDS")
saveRDS(site.metrics, "data/metrics_per_site_wide.RDS")

library(lme4)

ggsite.metrics2 <- droplevels(ggsite.metrics[which(!(ggsite.metrics$native.cluster %in% c("Minor clusters", "No native cluster"))), ])

pc.endemism.model <- glmer(pc.endemism ~ -1 + native.cluster + dataset + native.cluster:dataset + (1|site),
                           family = "binomial", data = ggsite.metrics2,
                           weights = richness,
                           control = glmerControl(optimizer="bobyqa",
                                                  optCtrl=list(maxfun=2e5)))

saveRDS(pc.endemism.model, "data/glmm_fish_with_pool_fixed_effect.RDS")

hist(residuals(pc.endemism.model), breaks = 50)
summaryglmm <- summary(pc.endemism.model) 
summaryglmm$coefficients
write.table(summaryglmm$coefficients, "./outputs/glmmsummary.txt", sep = "\t")

qqnorm(residuals(pc.endemism.model))

plot(pc.endemism.model)

ggplot(data.frame(eta=predict(pc.endemism.model,type="link"),pearson=residuals(pc.endemism.model,type="pearson")),
       aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()


fixed_effects <- predict(pc.endemism.model, type = "response", re.form=NA)
fixed_effects <- data.frame(ggsite.metrics2$dataset,
                            ggsite.metrics2$native.cluster,
                            fixed_effects)
fixed_effects <- unique(fixed_effects)
colnames(fixed_effects) <- c("dataset", "native.cluster", "pc.endemism")

fixed_effects$dataset <- factor(fixed_effects$dataset,
                                levels = c("Natural", "Anthropocene"))

fixed_effects$x <- as.numeric(fixed_effects$dataset) - .5
fixed_effects$xend <- as.numeric(fixed_effects$dataset) + .5


margins::margins(pc.endemism.model)
library(emmeans)
ggmod <- data.frame(emmeans(pc.endemism.model,
                            specs = c("native.cluster", "dataset")))
ggmod$emmean <- boot::inv.logit(ggmod$emmean)
ggmod$SE <- boot::inv.logit(ggmod$SE)
ggmod$asymp.LCL <- boot::inv.logit(ggmod$asymp.LCL)
ggmod$asymp.UCL <- boot::inv.logit(ggmod$asymp.UCL)


diffs <- ggmod$emmean[ggmod$dataset == "Natural"] - ggmod$emmean[ggmod$dataset == "Anthropocene"]
names(diffs) <- ggmod$native.cluster[ggmod$dataset == "Natural"]
diffs

ggsite.metrics2$dataset <- factor(ggsite.metrics2$dataset,
                                  levels = c("Anthropocene", "Natural"))
ggmod$dataset <- factor(ggmod$dataset,
                        levels = c("Anthropocene", "Natural"))
ggsite.metrics2$native.cluster <- factor(ggsite.metrics2$native.cluster,
                                         levels = rev(levels(ggsite.metrics2$native.cluster)))
ggmod$native.cluster <- factor(ggmod$native.cluster,
                               levels = rev(levels(ggmod$native.cluster)))

pend <- ggplot() +
  geom_point(data = ggsite.metrics2,
             aes(y = pc.endemism,
                 x = native.cluster,
                 col = dataset),
             position = position_dodge(width = .75),
             alpha = .2) +
  geom_errorbar(data = ggmod, 
                aes(x = native.cluster,
                    ymin = asymp.LCL, ymax = asymp.UCL,
                    colour = dataset), 
                width = 0.5,
                linewidth = 1.5, position = position_dodge(width = .75)) +
  geom_point(data = ggmod, aes(x = native.cluster,
                               y = emmean, 
                               fill = dataset),
             col = "black",
             position = position_dodge(width = .75),
             size = 4,
             shape = 21) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c("#ca0020", "#0571b0"),
                      labels = c("Anthropocene", "Natural"),
                      name = "")  +
  scale_fill_manual(values = c("#ca0020", "#0571b0"),
                    labels = c("Anthropocene", "Natural"),
                    name = "") +
  coord_flip() +
  theme_bw() +
  xlab("Natural biogeographical regions") + 
  ylab("Percentage of endemic species per drainage basin") +
  theme(legend.position = c(.75, .9),
        legend.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1,
                                   size = 13),
        axis.text.y = element_text(size = 13),
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        strip.text.x = element_text(size = 13),
        panel.border = element_blank()) +
  scale_x_discrete(breaks = factor(c(levels(ggmod$native.cluster), "")),
                   limits = c(levels(ggmod$native.cluster), ""))

 library(egg)



bioregions.lvl2 <- readRDS("./data/bioregions_lvl2")
pnative2 <- readRDS("outputs/pnative2.RDS")

levels(ggmod$native.cluster)
df <- data.frame(x = 1:10, y = 1:10)
base <- ggplot(df, aes(x, y)) +
  geom_blank() + theme_void()


  

cairo_pdf("./outputs/Figure 1.pdf", h = 10, w = 10)

pend + 
  annotation_custom(
    ggplotGrob(pnative2 + theme(legend.position = "none",
                                plot.background = element_rect(colour = "grey"))), 
    xmin = 6.3, xmax = 8.1, ymin = -.3, ymax = .25
  ) +
  annotation_custom(
    ggplotGrob(base + theme(plot.background = element_rect(fill = bioregions.lvl2$col.native.lvl2[1]))), 
    xmin = 5.85, xmax = 5.65, ymin = -.15, ymax = -.1
  ) +
  annotation_custom(
    ggplotGrob(base + theme(plot.background = element_rect(fill = bioregions.lvl2$col.native.lvl2[2]))), 
    xmin = 4.85, xmax = 4.65, ymin = -.15, ymax = -.1
  ) +
  annotation_custom(
    ggplotGrob(base + theme(plot.background = element_rect(fill = bioregions.lvl2$col.native.lvl2[3]))), 
    xmin = 3.85, xmax = 3.65, ymin = -.15, ymax = -.1
  ) +
  annotation_custom(
    ggplotGrob(base + theme(plot.background = element_rect(fill = bioregions.lvl2$col.native.lvl2[4]))), 
    xmin = 2.85, xmax = 2.65, ymin = -.15, ymax = -.1
  ) +
  annotation_custom(
    ggplotGrob(base + theme(plot.background = element_rect(fill = bioregions.lvl2$col.native.lvl2[5]))), 
    xmin = 1.85, xmax = 1.65, ymin = -.15, ymax = -.1
  ) +
  annotation_custom(
    ggplotGrob(base + theme(plot.background = element_rect(fill = bioregions.lvl2$col.native.lvl2[6]))), 
    xmin = 0.85, xmax = 0.65, ymin = -.15, ymax = -.1
  ) +
  annotation_custom(
    ggplotGrob(base + theme(plot.background = element_rect(fill = "white",
                                                           colour = "white"))), 
    xmin = 6.9, xmax = 7.8, ymin = .21, ymax = 1.1
  ) +
  ggtitle("\n\n\n")

dev.off()


# basinshp$pc.endemism.change <- ggsite.metrics$pc.endemism

median_endemism <- merge(data.frame(aggregate(ggsite.metrics2$pc.endemism[which(ggsite.metrics2$dataset == "Natural")],
                                        by = list(ggsite.metrics2$native.cluster[which(ggsite.metrics2$dataset == "Natural")]),
                                        FUN = median)),
                         data.frame(aggregate(ggsite.metrics2$pc.endemism[which(ggsite.metrics2$dataset == "Anthropocene")],
                                              by = list(ggsite.metrics2$native.cluster[which(ggsite.metrics2$dataset == "Anthropocene")]),
                                              FUN = median,
                                              na.rm = TRUE)),
                         by = "Group.1")
colnames(median_endemism) <- c("Native_region", "Native_endemism", "Anthropocene_endemism")
median_endemism$Diff_endemism <- median_endemism$Native_endemism - median_endemism$Anthropocene_endemism



# ---- Analyses on introduced species only ----
# Introduced species characteristics
introduced.sp.list <- levels(as.factor(fish.all$X6.Fishbase.Valid.Species.Name[which(fish.all$X3.Native.Exotic.Status == "exotic")]))
intro.chars <- data.frame()
for (sp in introduced.sp.list)
{
  # Occurrences only for this species
  subdb <- as.data.frame(droplevels(fish.all[which(fish.all$X6.Fishbase.Valid.Species.Name %in% sp), ]))


  # Regions where the species occurs natively
  native.regions <- unique(subdb$basin.region[which(subdb$X3.Native.Exotic.Status == "native")])
  if(any(native.regions == "Minor clusters"))
  {
    native.regions <- native.regions[-which(native.regions == "Minor clusters")]
  }
  # Regions where the species has been introduced
  introduced.regions <- unique(subdb$basin.region[which(subdb$X3.Native.Exotic.Status == "exotic")])
  if(any(introduced.regions == "Minor clusters"))
  {
    introduced.regions <- introduced.regions[-which(introduced.regions == "Minor clusters")]
  }
  # Native occurrences
  nat <- subdb[which(subdb$X3.Native.Exotic.Status == "native"), ]
  # Exotic occurrences
  exo <- subdb[which(subdb$X3.Native.Exotic.Status == "exotic"), ]

  intro.chars <- rbind.data.frame(intro.chars,
                                  data.frame(species = sp,
                                             # Region assigned to this species
                                             network.region = unique(subdb$species.region),
                                             # Number of regions where the species occurs natively
                                             nb.native.regions = length(native.regions),
                                             # Regions where the species occurs natively
                                             native.regions = paste(native.regions, collapse = ","),
                                             # Number of regions where the species has been introduced
                                             nb.introduced.regions = length(introduced.regions),
                                             # Regions where the species has been introduced
                                             introduced.regions = paste(introduced.regions, collapse = ","),
                                             # Total occurrence
                                             total.occ = nrow(subdb),
                                             # Native occurrence
                                             occ.native = nrow(nat),
                                             # Exotic occurrence
                                             occ.introduced = nrow(exo),
                                             # Nombre d'occurrences natives dans la région assignée par Map Equation
                                             native.inside.ME.region = length(which(nat$basin.region %in% unique(subdb$species.region))),
                                             # Nombre d'occurrences natives hors de la région assignée par Map Equation
                                             native.outside.ME.region = length(which(!(nat$basin.region %in% unique(subdb$species.region)))),
                                             # Nombre d'occurrences introduites dans la région assignée par Map Equation
                                             intro.inside.ME.region = length(which(exo$basin.region %in% unique(subdb$species.region))),
                                             # Nombre d'occurrences introduites hors la région assignée par Map Equation
                                             intro.outside.ME.region = length(which(!(exo$basin.region %in% unique(subdb$species.region)))),
                                             # Nombre d'occurrences introduites dans les régions natives
                                             intro.inside.native.regions = length(which(exo$basin.region %in% native.regions)),
                                             # Nombre d'occurrences introduites hors des régions natives
                                             intro.outside.native.regions = length(which(!(exo$basin.region %in% native.regions)))))
}

intro.chars <- intro.chars[order(intro.chars$occ.introduced, decreasing = TRUE), ]

# Corrections to species without native occurrences
intro.chars$nb.native.regions[which(intro.chars$nb.native.regions == 0)] <- 1
# Gambusia dominicensis
intro.chars$native.regions[intro.chars$species == "Gambusia dominicensis"] <- "Neotropical (2.1)"
# Relictus solitarius
intro.chars$native.regions[intro.chars$species == "Relictus solitarius"] <- "Nearctic (2.2)"
intro.chars$intro.inside.native.regions[intro.chars$species == "Relictus solitarius"] <- 1
intro.chars$intro.outside.native.regions[intro.chars$species == "Relictus solitarius"] <- 0



intro.chars$col.native.lvl2 <- bioregions.lvl2$col.native.lvl2[match(intro.chars$network.region,
                                                                     bioregions.lvl2$name)]

intro.chars <- data.frame(intro.chars,
                          species.metrics[match(intro.chars$species,
                                                species.metrics$anthropocene.species), ])


intro.chars$exotic.DilVal <- intro.chars$anthropocene.Occ.DilVal - intro.chars$native.Occ.DilVal


# Correcting endemism status for species with no native occurrences
intro.chars$native.Endemism[intro.chars$species == "Gambusia dominicensis"] <- TRUE
intro.chars$native.Endemism[intro.chars$species == "Relictus solitarius"] <- TRUE

intro.chars$anthropocene.Endemism[intro.chars$species == "Gambusia dominicensis"] <- FALSE
intro.chars$native.Endemism[intro.chars$species == "Relictus solitarius"] <- TRUE


# Graphique illustrant les différents types d'introductions
intro.chars$in.out.index <- intro.chars$intro.outside.native.regions /
  (intro.chars$intro.outside.native.regions + intro.chars$intro.inside.native.regions) * 100

intro.chars$endemism.status <-
  ifelse(intro.chars$native.Endemism,
         ifelse(intro.chars$anthropocene.Endemism,
                "Endemic",
                "No longer endemic"),
         ifelse(intro.chars$anthropocene.Endemism,
                "Neo-endemic",
                "Not endemic"))

plyr::count(intro.chars$endemism.status)

cols <- bioregions.lvl2$col.native.lvl2
names(cols) <- bioregions.lvl2$name

intro.chars[which(intro.chars$endemism.status == "Endemic" &
                    intro.chars$in.out.index > 0),]

saveRDS(intro.chars, 
        "./outputs/introduced_species_characteristics.RDS")

library(grid)

intro.chars <- readRDS("./outputs/introduced_species_characteristics.RDS")

intro.chars.summary <- intro.chars[, c("species", "network.region", "native.regions", "introduced.regions", "total.occ", "occ.native", "occ.introduced", "exotic.DilVal", "in.out.index", "endemism.status")]

colnames(intro.chars.summary) <- c("Species", "Cluster", "Native regions", 
                                   "Introduced\nregions", "Total\noccurrence",
                                   "Occurrence in\nnative region", 
                                   "Number of\nintroductions",
                                   "Dilution value",
                                   "Proportion\nof introductions\noutside\nnative region",
                                   "Endemism status")

xlsx::write.xlsx(intro.chars.summary, "outputs/introduced_species.xlsx",
                 row.names = FALSE)


topsp <- intro.chars[intro.chars$occ.introduced >= 100, c("species",
                                                          "in.out.index",
                                                          "occ.introduced")]
topsp$xmod <- topsp$in.out.index
topsp$ymod <- topsp$occ.introduced

# Cyprinus carpio
topsp$xmod[1] <- 67
# Carassius auratus
topsp$xmod[2] <- 48
# Gambusia holbrooki
topsp$xmod[3] <- 100
topsp$ymod[3] <- 320
# Gambusia affinis
topsp$xmod[4] <- 85
topsp$ymod[4] <- 314
# Oreochromis mossambicus
topsp$xmod[5] <- 95
topsp$ymod[5] <- 282
# Micropterus salmoides
topsp$xmod[6] <- 55
# topsp$ymod[6] <- 170
# Poecilia reticulata
# topsp$xmod[7] <- 40
topsp$ymod[7] <- 194
# Ctenopharyngodon idella
topsp$xmod[8] <- 68
topsp$ymod[8] <- 155
# Perca fluviatilis
topsp$xmod[9] <- 62
# topsp$ymod[9] <- 132
# Trichopodus pectoralis
# topsp$xmod[10] <- 33
topsp$ymod[10] <- 142
# Lepomis gibbosus
topsp$xmod[11] <- 91
# topsp$ymod[11] <- 125
# Oreochromis niloticus
# topsp$xmod[12] <- 72
topsp$ymod[12] <- 120
# Hypophthalmichthys molitrix
topsp$xmod[13] <- 69
# topsp$ymod[13] <- 85

topsp$xmod2 <- topsp$xmod
topsp$ymod2 <- topsp$ymod
# # Trichopodus pectoralis
topsp$ymod2[10] <- 132
# # Lepomis gibbosus
topsp$xmod2[11] <- 87.5
# Oreochromis niloticus
# topsp$xmod[12] <- 72
topsp$ymod2[12] <- 115
# # Hypophthalmichthys molitrix
topsp$xmod2[13] <- 70
# topsp$ymod2[13] <- 95


topsp$number <- 1:nrow(topsp)


topsp$label <- paste0(topsp$number, 
                      "** *", topsp$species, "*")


# topsp$label[1] <- paste0(topsp$label[1], (" (European carp)"))
# topsp$label[2] <- paste0(topsp$label[2], (" (Goldfish)"))
# topsp$label[3] <- paste0(topsp$label[3], (" (Eastern <br> mosquitofish)"))

txt <- data.frame(text = 
                    paste0("**", 
                           paste0(topsp$label, 
                                  c(" (European carp)",
                                    " (Goldfish)",
                                    " (Eastern <br> mosquitofish)",
                                    " (Mosquitofish)",
                                    "<br> (Mozambique tilapia)",
                                    "<br> (Largemouth bass)",
                                    " (Guppy)",
                                    "<br> (Grass carp)",
                                    " (European perch)",
                                    "<br> (Snakeskin gourami)",
                                    "<br> (Pumpkinseed)",
                                    " (Nile<br> tilapia)",
                                    "<br> (Silver carp)"),
                                  collapse = "<br>**"),
                           collapse = ""))

cairo_pdf("./outputs/Figure 5.pdf",
          height = 10, width = 10)
ggplot(intro.chars) +
  geom_point(aes(x = in.out.index,
                 y = occ.introduced,
                 size = exotic.DilVal,
                 col = col.native.lvl2,
                 shape = endemism.status),
             alpha = .8) +
  geom_text(data = topsp,
            aes(x = xmod,
                y = ymod,
                label = number),
            size = 5,
            hjust = .75,
            vjust = .25) +
  geom_richtext(data = txt,
            aes(label = text),
            x = 105, 
            y = 0.4,
            hjust = 0,
            size = 4,
            fill = NA, label.color = NA) +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  scale_y_log10() +
  scale_size(range = c(1, 10)) +
  scale_colour_identity(name = "Regions of origin",
                        limits = as.character(bioregions.lvl2$col.native.lvl2),
                        labels = as.character(bioregions.lvl2$name)) +
  guides(colour = guide_legend(override.aes = list(size=5,
                                                   alpha = 1)),
         size = guide_legend(title = "Dilution value",
                             override.aes = list(shape = 15)),
         shape = guide_legend(title = "Endemism status",
                              override.aes = list(size = 5))) +
  xlab("Proportion of introductions outside native region") +
  ylab("Number of introduction basins") +
  geom_segment(data = topsp,
               aes(x = in.out.index, y = occ.introduced,
                   xend = xmod2, yend = ymod2)) +
  theme(text = element_text(size=20),
        legend.position = "right",
        legend.justification = "top") +
  annotation_logticks(sides = "l")
dev.off()



# ---- Other species-based graphs ----
#
# ggplot(intro.chars, aes(x = occ.introduced, y = Occ.DilVal)) +
#   geom_point(alpha = .1) +
#   stat_smooth() +
#   facet_wrap(~network.region)
#
#
# ggplot(intro.chars, aes(x = total.occ)) +
#   stat_bin()
#
# ggplot(intro.chars, aes(x = occ.introduced)) +
#   stat_bin()
#
# ggplot(intro.chars, aes(y = occ.introduced, x = network.region)) +
#   stat_boxplot()
#
# ggplot(intro.chars, aes(x = intro.inside.native.regions,
#                        y = intro.outside.native.regions)) +
#   geom_point() + stat_smooth()
#
#
# boxplot(intro.chars[, c("intro.inside.native.regions", "intro.outside.native.regions")])
# summary(intro.chars)
#
# intro.chars[which(intro.chars$nb.native.regions > 1),]
#
# library(Rarity)
# corPlot(as.data.frame(intro.chars[, 5:ncol(intro.chars)]), method = "pearson")
# summary(intro.chars)
# quantile(intro.chars$occ.introduced, prob = seq(0, 1, by = .1))
# nrow(intro.chars[intro.chars$occ.introduced > 10, ])
# nrow(intro.chars[intro.chars$occ.introduced > 100, ])
#
#
# View(intro.chars[, c("species", "nb.native.regions", "native.regions", "total.occ", "occ.native", "occ.introduced", "intro.inside.native.regions", "intro.outside.native.regions")])



# ---- Réseau d'introductions ----

introduction.network <- data.frame()
# Création d'un réseau directionnel pour les introductions
for (region in unique(fish.all$species.region))
{
  subdb <- fish.all[which(fish.all$species.region == region &
                            fish.all$X3.Native.Exotic.Status == "exotic"), ]
  for (destination in unique(subdb$basin.region))
  {
    destidb <- subdb[which(subdb$basin.region == destination), ]
    introduction.network <- dplyr::bind_rows(introduction.network,
                                             data.frame(Origin = region,
                                                        Destination = destination,
                                                        Nb.introductions = nrow(destidb)))

  }
}

introduction.network$color <- bioregions.lvl2$col.native.lvl2[match(introduction.network$Origin,
                                                                    bioregions.lvl2$name)]

saveRDS(introduction.network,
        "./data/introductionetwork.RDS")


introduction.network <- readRDS("./data/introductionetwork.RDS")

library(dplyr)
library(ggplot2)
library(ggalluvial)
library(ggrepel)


## Preparing the "Origin" part of the alluvial diagram
# Calculate the sum of introductions for each region
origin.sum <- introduction.network %>%
  group_by(Origin) %>%
  summarise(total.egress = sum(Nb.introductions))

# Order by decreasing egress
origin.sum <- origin.sum[order(origin.sum$total.egress, decreasing = TRUE), ]

# Use this to determine the order of boxes in the diagram
introduction.network$Origin <- factor(introduction.network$Origin,
                                      levels = origin.sum$Origin[order(origin.sum$total.egress, decreasing = TRUE)])
introduction.network$Destination <- factor(introduction.network$Destination,
                                      levels = origin.sum$Origin[order(origin.sum$total.egress, decreasing = TRUE)])

origin.sum$Origin <- factor(origin.sum$Origin,
                            levels = levels(introduction.network$Origin))

# Calculate the cumulative sum of introduction to properly position labels
origin.sum$cumsum <- rev(cumsum(rev(origin.sum$total.egress)))
# Labels will be at mid point
origin.sum$midpoint <- sapply(1:nrow(origin.sum),
                              function(x, tabs)
                              {
                                ifelse(is.na(tabs[x + 1]),
                                       0, tabs[x + 1]) +
                                  (tabs[x] - ifelse(is.na(tabs[x + 1]),
                                                  0, tabs[x + 1])) / 2
                              }, tabs = origin.sum$cumsum)

# Correcting the position of labels to avoid overlap, and preparing x/y for segments to link labels to boxes
origin.sum$xpos <- 1 - 1.5/24
origin.sum$xpos2 <- 1 - 1/24 # For end of arrows
origin.sum$midpointcorrected <- origin.sum$midpoint
# Ethiopian
origin.sum$midpointcorrected[4] <- origin.sum$midpoint[4]
# Neotropical
origin.sum$midpointcorrected[5] <- 550
# Minor clusters
origin.sum$midpointcorrected[6] <- 350
# Australian
origin.sum$midpointcorrected[7] <- 125


## Preparing the "Destination part of the alluvial diagram
destination.sum <- introduction.network %>%
  group_by(Destination) %>%
  summarise(total.ingress = sum(Nb.introductions))

# Order identical to origin
destination.sum <- destination.sum[match(origin.sum$Origin,
                                         destination.sum$Destination), ]


destination.sum$cumsum <- rev(cumsum(rev(destination.sum$total.ingress)))

destination.sum$midpoint <- sapply(1:nrow(destination.sum),
                                   function(x, tabs)
                                   {
                                     ifelse(is.na(tabs[x + 1]),
                                            0, tabs[x + 1]) +
                                       (tabs[x] - ifelse(is.na(tabs[x + 1]),
                                                         0, tabs[x + 1])) / 2
                                   }, tabs = destination.sum$cumsum)
destination.sum$xpos <- 2 + 1.5/24
destination.sum$xpos2 <- 2 + 1/24 # For end of arrows
destination.sum$midpointcorrected <- destination.sum$midpoint
# CA transition zone
# destination.sum$midpointcorrected[6] <- 654

# Preparing colors

fillcols <- unique(introduction.network[, c("Origin", "color")])
fillcols <- rep(rev(fillcols$color[match(levels(introduction.network$Origin),
                                            fillcols$Origin)]), 2)
pdf("./outputs/Figure 4.pdf", height = 10, width = 12)
ggplot(data = introduction.network) +
  geom_alluvium(aes(y = Nb.introductions,
                    axis1 = Origin,
                    axis2 = Destination,
                    fill = color), width = 1/12, alpha = .7,
                color = "white") +
  geom_stratum(aes(y = Nb.introductions,
                   axis1 = Origin,
                   axis2 = Destination,
                   fill = color),
               width = 1/12,
               fill = fillcols,
               color = "white") +
  scale_fill_identity() +
  theme_minimal() +
  geom_label(data = origin.sum,
                   aes(x = xpos, y = midpointcorrected,
                       label = Origin),
                   fill = "white",
             hjust = 1) +
  geom_segment(data = origin.sum,
               aes(x = xpos, y = midpointcorrected, xend = xpos2, yend = midpoint)) +
  geom_label(data = destination.sum,
             aes(x = xpos, y = midpointcorrected,
                 label = Destination),
             fill = "white",
             hjust = 0) +
  geom_segment(data = destination.sum,
               aes(x = xpos, y = midpointcorrected, xend = xpos2, yend = midpoint)) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Region of origin", "Region of destination"),
                   limits = c(0.7, 2.3)) +
  xlab("") + ylab("Number of introductions") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))
dev.off()


