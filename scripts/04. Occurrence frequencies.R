library(ggplot2)
library(tidyr)
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

dbnative <- table(fish.native$X6.Fishbase.Valid.Species.Name, fish.native$X1.Basin.Name)

occnative <- rowSums(dbnative)


hist(occnative, breaks = 60)
abline(v = 10, col = "darkred")
length(which(occnative > 10))

intro_chars <- readRDS("./outputs/introduced_species_characteristics.RDS")


extract_occs <- data.frame(species = names(occnative),
                           occurrence = occnative)
extract_occs$introduced <- 0
extract_occs$introduced[which(extract_occs$species %in% 
                                intro_chars$species)] <- 1

xlsx::write.xlsx(extract_occs,
                 file = "data/species_occurrences.xlsx")

occ_comparaison <- data.frame(cutoff = #seq(0, max(occnative), by = 10),
                                c(0, 10, 50, 100, 200))

occ_comparaison$nb_native <- sapply(occ_comparaison$cutoff,
                                    function(x) {
                                      length(which(occnative > x))
                                    })

occ_comparaison$nb_intro <- sapply(occ_comparaison$cutoff,
                                    function(x) {
                                      length(which(intro_chars$occ.native > x))
                                    })


occ_comparaison$pc_intro <- occ_comparaison$nb_intro / occ_comparaison$nb_native

cairo_pdf("./outputs/Figure S5.2.pdf", width = 8, height = 8,
          pointsize = 6)
ggplot(occ_comparaison, aes(x = cutoff, y = pc_intro)) +
  geom_line() +
  geom_point() + xlab("Ocurrence cut-offs") +
  scale_y_continuous(labels = scales::label_percent()) +
  ylab("% of introduced species") +
  theme_bw() +
  theme(text = element_text(size = 18)) 
dev.off()

nrow(intro_chars)
length(occnative)

ggplot(intro_chars) +
  geom_histogram(aes(x = occ.native))

# Defining breaks and parameters of simulation
sim <- 100
breaks <- seq(0, max(occnative), by = 10)
break_points <- breaks[1:(length(breaks) - 1)] + diff(breaks) / 2
break_native <- levels(cut(1:max(occnative),
                           breaks))
res <- data.frame(matrix(nr = length(break_native),
                         nc = sim + 1,
                         dimnames = list(NULL,
                                         c("bins", paste0("sim", 1:sim)))))
res$bins <- break_native

# Simulating random sampling of species
for(i in 1:100){
  sample_occ <- data.frame(occ.native = sample(occnative, nrow(intro_chars)))
  sample_occ$occ_classes <- cut(sample_occ$occ.native,
                                breaks)
  counts <- plyr::count(sample_occ$occ_classes)
  res[match(counts$x, res$bins), i + 1] <- counts$freq
}
res[is.na(res)] <- 0

# Counting the expected number of species in each bin
average_count <- data.frame(bins = res$bins,
                            occ_x = break_points,
                            avg = rowMeans(res[, -1], na.rm = TRUE))


intro_chars$occ.native[which(intro_chars$occ.native == 0)] <- 1

# Counting the observed number of species in each bin
obs_intro_cut <- plyr::count(cut(intro_chars$occ.native,
                                 breaks))

average_count$obs[match(obs_intro_cut$x, average_count$bins)] <-
  obs_intro_cut$freq

average_count$obs[is.na(average_count$obs)] <- 0

average_count <- average_count[rowSums(average_count[, c("avg", "obs")]) > 0, ]

chi2 <- chisq.test(average_count[, c("avg", "obs")],
                   simulate.p.value = TRUE, B = 50000)


average_count$residuals.obs <- chi2$stdres[, "obs"]

average_count$signif <- ifelse(abs(average_count$residuals.obs) > 2,
                               "*", NA)

# average_count$signif_pos <- apply(average_count[, c("avg", "obs")], 1, max) + 20

average_count$signif_pos <- -10



ggaverage_count <- pivot_longer(average_count,
                              cols = c(avg, obs))


ggaverage_count$name <- as.factor(ggaverage_count$name)
levels(ggaverage_count$name) <- c("Expected distribution (if introduced\nspecies were randomly selected)\n",
                                  "Observed distribution\n\n")

cairo_pdf("./outputs/Figure S5.1.pdf", width = 8, height = 8,
          pointsize = 6)
ggplot(ggaverage_count) +
  geom_col(aes(x = occ_x, y = value, fill = name),
           position = "dodge") +
  geom_text(data = average_count, aes(x = occ_x, label = signif,
                                      y = signif_pos),
            size = 10) +
  geom_label(x = 160, y = 100, label = 
               "Chi-squared test = 199.36, p-value = 0.00002\n* = significant differences\n(Pearson standardised residuals > 2)",
             hjust = 0, size = 4.5) +
  theme_bw() +
  theme(text = element_text(size = 18),
        legend.position = c(.7, .7)) +
  guides(fill = guide_legend(title = "Distributions of occurrences\nfor introduced species")) +
  xlab("Native occurrence") +
  ylab("Number of species")

dev.off()



# Adding the total number of species in each bin
tot_occ_breaks <- cut(occnative,
                      breaks)
freq_tot_occ <- plyr::count(tot_occ_breaks)

average_count$tot_occ[match(average_count$bins,
                            freq_tot_occ$x)] <- freq_tot_occ$freq


average_count$pc_introduced <- average_count$obs / average_count$tot_occ
average_count$pc_not_introduced <- 1 - average_count$pc_introduced

ggaverage_count2 <- pivot_longer(average_count,
                                cols = c(pc_introduced, pc_not_introduced))

ggaverage_count2$name <- factor(ggaverage_count2$name)
levels(ggaverage_count2$name) <- c("Introduced",
                                   "Not introduced")

cairo_pdf("./outputs/Figure S5.3.pdf", width = 8, height = 8,
          pointsize = 6)
ggplot(ggaverage_count2, aes(x = occ_x, y = value, fill = name))  + 
  geom_bar(position = "fill", stat = "identity") +
  theme_bw() +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_brewer(palette = "Set2") +
  theme(text = element_text(size = 18)) +
  guides(fill = guide_legend(title = "Species")) +
  xlab("Native occurrence") +
  ylab("% of species for each interval")
dev.off()

write.table(average_count, "./data/count_species.txt", sep = "\t")

a <- lm(pc_introduced ~ occ_x, data = average_count)

plot(predict(a) ~ average_count$occ_x)

ggplot(average_count, aes(x = occ_x, y = pc_introduced)) +
  geom_col(fill = "#de7065") + theme_bw() +
  scale_y_continuous(labels = scales::label_percent()) +
  theme(text = element_text(size = 18),
        legend.position = c(.7, .7)) + 
  stat_smooth(method = "lm") +
  ylim(0, 1) +
  guides(fill = guide_legend(title = "Distributions of occurrences\nfor introduced species")) +
  xlab("Native occurrence") +
  ylab("% of species that were introduced")

# dbanthro <- table(fish.all$X6.Fishbase.Valid.Species.Name, fish.all$X1.Basin.Name)
# occanthro <- rowSums(dbanthro)
# hist(occanthro, breaks = 60)
# 
# b <- data.frame(Occ_native = occnative, 
#                 Occ_introduite = occanthro[match(names(occnative), names(occanthro))])
# bm <- reshape2::melt(b)
# 
# # Histograms of occurrence frequency distribution
# ggplot(bm, aes(x = value)) +
#   geom_histogram() +
#   facet_wrap(vars(variable),
#              scales = "free_x") +
#   theme_bw()
# 
# ggplot(b, aes(x = Occ_native, y = Occ_introduite)) +
#   geom_point(alpha = .2) +
#   # geom_smooth() +
#   geom_abline(slope = 1) + 
#   coord_fixed() +
#   geom_vline(xintercept = 10)
