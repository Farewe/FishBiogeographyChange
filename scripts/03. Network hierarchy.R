# devtools::install_github("https://github.com/YuLab-SMU/ggtree")
library(ggtree)
library(tibble)
library(ggplot2)

# ---------- Native network ------------

native.net <- readRDS("./data/nativenetwork.RDS")

max.lvl <- 6

# Native without extinctions
fish.native <- as.data.frame(readRDS("./data/fish_native.rds"))
fish.native$X1.Basin.Name <- as.factor(fish.native$X1.Basin.Name)
fish.native$X6.Fishbase.Valid.Species.Name <- as.factor(fish.native$X6.Fishbase.Valid.Species.Name)

bioregions.lvl1 <- readRDS("./data/bioregions_lvl1")
bioregions.lvl2 <- readRDS("./data/bioregions_lvl2")

bioregions.lvl1$name <- gsub("\\)", "", gsub("\\(", "", bioregions.lvl1$name))
bioregions.lvl2$name <- gsub("\\)", "", gsub("\\(", "", bioregions.lvl2$name))

fish.net.tree <- native.net
fish.net.tree$Name <- as.character(fish.net.tree$Name)
# Level 1
levels(fish.net.tree$lvl1)[which(!(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1) &
                                   nchar(levels(fish.net.tree$lvl1)) <= 3)] <- 
  paste0("Cluster ", levels(fish.net.tree$lvl1)[which(!(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1) &
                                                        nchar(levels(fish.net.tree$lvl1)) <= 3)])
levels(fish.net.tree$lvl1)[which(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1)] <- 
  na.omit(bioregions.lvl1$name[
    match(bioregions.lvl1$clvl1, levels(fish.net.tree$lvl1)[which(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1)])
  ])

# Level 2
levels(fish.net.tree$lvl2)[which(!(levels(fish.net.tree$lvl2) %in% bioregions.lvl2$clvl2) &
                                   nchar(levels(fish.net.tree$lvl2)) <= 3)] <- 
  paste0("Cluster ", levels(fish.net.tree$lvl2)[which(!(levels(fish.net.tree$lvl2) %in% bioregions.lvl2$clvl2) &
                                                        nchar(levels(fish.net.tree$lvl2)) <= 3)])


levels(fish.net.tree$lvl2)[which(levels(fish.net.tree$lvl2) %in% bioregions.lvl2$clvl2)] <- 
  na.omit(bioregions.lvl2$name[
    match(bioregions.lvl2$clvl2, levels(fish.net.tree$lvl2)[which(levels(fish.net.tree$lvl2) %in% bioregions.lvl2$clvl2)])
  ])

# devtools::install_github("Farewe/biogeonetworks")
fish.sites.tree <-  biogeonetworks::getSiteTable(fish.native, site.field = "X1.Basin.Name", network = fish.net.tree)

lvl0 = "Earth"
network <- fish.sites.tree
# network <- network[-which(network$color.lvl2 %in% "#808080"), ]
levels = paste0("lvl", 1:max.lvl)

# Workaround necessary to avoid solitary leaves on solitary nodes (which will break plotting programs)
netw <- network[, levels]
for (ilvl in 1:(max.lvl - 1)) #NLVL - 1
{
  lvl <- paste0("lvl", ilvl)
  for (s in levels(netw[, lvl]))
  {
    if(nrow(netw[netw[, lvl] %in% s, ]) == 1)
    {
      cur.line <- which(netw[, lvl] %in% s)
      if(any(is.na((netw[cur.line, levels])))) {
        if(ilvl + 2 == min(which(is.na(netw[cur.line, levels]))) |
           (ilvl + 1 == length(levels) & !is.na(netw[cur.line, ilvl + 1])))
        {
          levels(netw[, lvl])[levels(netw[, lvl]) == s] <- as.character(netw[cur.line, 
                                                                             (grep(lvl, colnames(netw)) + 1)])
          netw[cur.line, grep(lvl, colnames(netw)) + 1] <- NA
        }
      }
    }
  }
}
network[, levels] <- netw


if(length(levels(network[, levels[1]])) == 1)
{
  network$pathString <- apply(network[, levels], 1, paste, collapse = "/")
} else
{
  network$pathString <- paste(lvl0, apply(network[, levels], 1, paste, collapse = "/"), sep = "/")
}

network$pathString <- gsub("/NA", "", network$pathString)

network.tree <- data.tree::as.Node(network)

# Converting to newick tree & applying workaround to have branch length of 1
newicktree <- data.tree::ToNewick(network.tree, 
                                  heightAttribute = function(x) 1)
# Previous line creates branch length of 0. Changing everything to 1
newicktree <- gsub(":0", ":1", newicktree)
write(newicktree, "./data/trees/native_newick.tree")


tree <- ape::read.tree(text = newicktree)

treelabels <- c(tree$tip.label, tree$node.label)


tree_order <- as_tibble(tree)
tree_order$color <- NA

# Need to rename nodes, changing spaces to _, to match the tree labels
network2 <- network
network2[, paste0("lvl", 1:max.lvl)] <- data.frame(lapply(network2[, paste0("lvl", 1:max.lvl)],
                                                          as.character))
network2[, paste0("lvl", 1:max.lvl)] <- lapply(network2[, paste0("lvl", 1:max.lvl)], 
                                               function(x) gsub(" ", "_", x))
for(i in 1:nrow(tree_order))
{
  if(any(network2[, c("lvl1", "lvl2")] == tree_order$label[i], na.rm = TRUE))
  {
    tmp <- unique(network2[which(network2[, c("lvl1", "lvl2")] == tree_order$label[i],
                                 arr.ind = TRUE)[, 1], "col.native.lvl1"])
    tree_order$color[i] <- tmp
  } else if(any(network2[, paste0("lvl", 3:max.lvl)] == tree_order$label[i], na.rm = TRUE))
  {
    tmp <- unique(network2[which(network2[, paste0("lvl", 3:max.lvl)] == tree_order$label[i],
                                 arr.ind = TRUE)[, 1], "col.native.lvl2"])
    tree_order$color[i] <- tmp
  } else {
    tree_order$color[i] <- NA
  }
}
tree_order[which(is.na(tree_order$color)), "color"] <- "#6E6E6E"

bioregions.lvl1 <- readRDS("./data/bioregions_lvl1")
bioregions.lvl2 <- readRDS("./data/bioregions_lvl2")

colnames(bioregions.lvl1) <- colnames(bioregions.lvl2) <- c("clvl", "color", "name")
treenat <- tree
treeordernat <- tree_order
natregions <- rbind(bioregions.lvl1[-3, ],
                    bioregions.lvl2)

ptrnat <- ggtree(treenat, layout = "circular",
                 aes(color = treeordernat$color),
                 size = .05) +
  scale_color_identity(name = "",
                      labels = as.character(natregions$name),
                      guide = guide_legend(override.aes = list(
                        col = as.character(natregions$color),
                        size = 2),
                        order = 1)) +
  theme(legend.position = "left")

# ptrnat

# cairo_pdf("outputs/tree.pdf")
# ptrnat
# dev.off()

# -------------- Anthropocene network ---------------

all.net <- readRDS("./data/allnetwork.RDS")

max.lvl <- 5

# Native without extinctions
fish.all <- as.data.frame(readRDS("./data/fish_anthropocene.rds"))
fish.all$X1.Basin.Name <- as.factor(fish.all$X1.Basin.Name)
fish.all$X6.Fishbase.Valid.Species.Name <- as.factor(fish.all$X6.Fishbase.Valid.Species.Name)

bioregions.lvl1 <- readRDS("./data/anthroregions_lvl1")
bioregions.lvl2 <- readRDS("./data/anthroregions_lvl2")

bioregions.lvl1$name <- gsub("\\)", "", gsub("\\(", "", bioregions.lvl1$name))
bioregions.lvl2$name <- gsub("\\)", "", gsub("\\(", "", bioregions.lvl2$name))

fish.net.tree <- all.net
fish.net.tree$Name <- as.character(fish.net.tree$Name)
# Level 1
levels(fish.net.tree$lvl1)[which(!(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1) &
                                   nchar(levels(fish.net.tree$lvl1)) <= 4)] <- 
  paste0("Cluster ", levels(fish.net.tree$lvl1)[which(!(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1) &
                                                        nchar(levels(fish.net.tree$lvl1)) <= 4)])
levels(fish.net.tree$lvl1)[which(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1)] <- 
  na.omit(bioregions.lvl1$name[
    match(bioregions.lvl1$clvl1, levels(fish.net.tree$lvl1)[which(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1)])
  ])

# Level 2
levels(fish.net.tree$lvl2)[which(nchar(levels(fish.net.tree$lvl2)) <= 4)] <- 
  paste0("Cluster ", levels(fish.net.tree$lvl2)[which(nchar(levels(fish.net.tree$lvl2)) <= 4)])

# devtools::install_github("Farewe/biogeonetworks")
fish.sites.tree <-  biogeonetworks::getSiteTable(fish.all, site.field = "X1.Basin.Name", network = fish.net.tree)

lvl0 = "Earth"
network <- fish.sites.tree
# network <- network[-which(network$color.lvl2 %in% "#808080"), ]
levels = paste0("lvl", 1:max.lvl)

# Workaround necessary to avoid solitary leaves on solitary nodes (which will break plotting programs)
netw <- network[, levels]
for (ilvl in 1:(max.lvl - 1)) #NLVL - 1
{
  lvl <- paste0("lvl", ilvl)
  for (s in levels(netw[, lvl]))
  {
    if(nrow(netw[netw[, lvl] %in% s, ]) == 1)
    {
      cur.line <- which(netw[, lvl] %in% s)
      if(any(is.na((netw[cur.line, levels])))) {
        if(ilvl + 2 == min(which(is.na(netw[cur.line, levels]))) |
           (ilvl + 1 == length(levels) & !is.na(netw[cur.line, ilvl + 1])))
        {
          levels(netw[, lvl])[levels(netw[, lvl]) == s] <- as.character(netw[cur.line, 
                                                                             (grep(lvl, colnames(netw)) + 1)])
          netw[cur.line, grep(lvl, colnames(netw)) + 1] <- NA
        }
      }
    }
  }
}
network[, levels] <- netw



if(length(levels(network[, levels[1]])) == 1)
{
  network$pathString <- apply(network[, levels], 1, paste, collapse = "/")
} else
{
  network$pathString <- paste(lvl0, apply(network[, levels], 1, paste, collapse = "/"), sep = "/")
}

network$pathString <- gsub("/NA", "", network$pathString)

network.tree <- data.tree::as.Node(network)

# Converting to newick tree & applying workaround to have branch length of 1
newicktree <- data.tree::ToNewick(network.tree, 
                                  heightAttribute = function(x) 1)
# Previous line creates branch length of 0. Changing everything to 1
newicktree <- gsub(":0", ":1", newicktree)
write(newicktree, "./data/trees/all_newick.tree")


tree <- ape::read.tree(text = newicktree)

treelabels <- c(tree$tip.label, tree$node.label)


tree_order <- as.tibble(tree)
tree_order$color <- NA

# Need to rename nodes, changing spaces to _, to match the tree labels
network2 <- network
network2[, paste0("lvl", 1:max.lvl)] <- data.frame(lapply(network2[, paste0("lvl", 1:max.lvl)],
                                                          as.character))
network2[, paste0("lvl", 1:max.lvl)] <- lapply(network2[, paste0("lvl", 1:max.lvl)], 
                                               function(x) gsub(" ", "_", x))
for(i in 1:nrow(tree_order))
{
  if(any(network2[, c("lvl1", "lvl2")] == tree_order$label[i], na.rm = TRUE))
  {
    tmp <- unique(network2[which(network2[, c("lvl1", "lvl2")] == tree_order$label[i],
                                 arr.ind = TRUE)[, 1], "col.all.lvl1"])
    tree_order$color[i] <- tmp
  } else if(any(network2[, paste0("lvl", 3:max.lvl)] == tree_order$label[i], na.rm = TRUE))
  {
    tmp <- unique(network2[which(network2[, paste0("lvl", 3:max.lvl)] == tree_order$label[i],
                                 arr.ind = TRUE)[, 1], "col.all.lvl1"])
    tree_order$color[i] <- tmp
  } else {
    tree_order$color[i] <- NA
  }
}
tree_order[which(is.na(tree_order$color)), "color"] <- "#6E6E6E"

antregions <- readRDS("./data/anthroregions_lvl1")

colnames(antregions) <- c("clvl", "color", "name")

treeant <- tree
treeorderant <- tree_order
ptrall <- ggtree(treeant, layout = "circular",
                 aes(color = treeorderant$color),
                 size = .05) +
  scale_color_identity(name = "",
                       labels = as.character(antregions$name),
                       guide = guide_legend(override.aes = list(
                         col = as.character(antregions$color),
                         size = 2),
                         order = 1)) +
  theme(legend.position = "right")

# ptrall


# cairo_pdf("outputs/tree.pdf")
# ptrnat
# dev.off()

# ------ Introductions only network -----


intro.net <- readRDS("./data/intronetwork.RDS")

max.lvl <- 4

# Native without extinctions
fish.intro <- as.data.frame(readRDS("./data/fish_intro.rds"))
fish.intro$X1.Basin.Name <- as.factor(fish.intro$X1.Basin.Name)
fish.intro$X6.Fishbase.Valid.Species.Name <- as.factor(fish.intro$X6.Fishbase.Valid.Species.Name)

bioregions.lvl1 <- readRDS("./data/introregions_lvl1")
bioregions.lvl2 <- readRDS("./data/introregions_lvl2")

bioregions.lvl1$name <- gsub("\\)", "", gsub("\\(", "", bioregions.lvl1$name))
bioregions.lvl2$name <- gsub("\\)", "", gsub("\\(", "", bioregions.lvl2$name))

fish.net.tree <- intro.net
fish.net.tree$Name <- as.character(fish.net.tree$Name)
# Level 1
levels(fish.net.tree$lvl1)[which(!(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1) &
                                   nchar(levels(fish.net.tree$lvl1)) <= 4)] <- 
  paste0("Cluster ", levels(fish.net.tree$lvl1)[which(!(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1) &
                                                        nchar(levels(fish.net.tree$lvl1)) <= 4)])
levels(fish.net.tree$lvl1)[which(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1)] <- 
  na.omit(bioregions.lvl1$name[
    match(bioregions.lvl1$clvl1, levels(fish.net.tree$lvl1)[which(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1)])
  ])

# Level 2
levels(fish.net.tree$lvl2)[which(nchar(levels(fish.net.tree$lvl2)) <= 4)] <- 
  paste0("Cluster ", levels(fish.net.tree$lvl2)[which(nchar(levels(fish.net.tree$lvl2)) <= 4)])

# devtools::instintro_github("Farewe/biogeonetworks")
fish.sites.tree <-  biogeonetworks::getSiteTable(fish.intro, site.field = "X1.Basin.Name", network = fish.net.tree)

lvl0 = "Earth"
network <- fish.sites.tree
# network <- network[-which(network$color.lvl2 %in% "#808080"), ]
levels = paste0("lvl", 1:max.lvl)

# Workaround necessary to avoid solitary leaves on solitary nodes (which will break plotting programs)
netw <- network[, levels]
for (ilvl in 1:(max.lvl - 1)) #NLVL - 1
{
  lvl <- paste0("lvl", ilvl)
  for (s in levels(netw[, lvl]))
  {
    if(nrow(netw[netw[, lvl] %in% s, ]) == 1)
    {
      cur.line <- which(netw[, lvl] %in% s)
      if(any(is.na((netw[cur.line, levels])))) {
        if(ilvl + 2 == min(which(is.na(netw[cur.line, levels]))) |
           (ilvl + 1 == length(levels) & !is.na(netw[cur.line, ilvl + 1])))
        {
          levels(netw[, lvl])[levels(netw[, lvl]) == s] <- as.character(netw[cur.line, 
                                                                             (grep(lvl, colnames(netw)) + 1)])
          netw[cur.line, grep(lvl, colnames(netw)) + 1] <- NA
        }
      }
    }
  }
}
network[, levels] <- netw



if(length(levels(network[, levels[1]])) == 1)
{
  network$pathString <- apply(network[, levels], 1, paste, collapse = "/")
} else
{
  network$pathString <- paste(lvl0, apply(network[, levels], 1, paste, collapse = "/"), sep = "/")
}

network$pathString <- gsub("/NA", "", network$pathString)

network.tree <- data.tree::as.Node(network)

# Converting to newick tree & applying workaround to have branch length of 1
newicktree <- data.tree::ToNewick(network.tree, 
                                  heightAttribute = function(x) 1)
# Previous line creates branch length of 0. Changing everything to 1
newicktree <- gsub(":0", ":1", newicktree)
write(newicktree, "./data/trees/intro_newick.tree")


tree <- ape::read.tree(text = newicktree)

treelabels <- c(tree$tip.label, tree$node.label)


tree_order <- as.tibble(tree)
tree_order$color <- NA

# Need to rename nodes, changing spaces to _, to match the tree labels
network2 <- network
network2[, paste0("lvl", 1:max.lvl)] <- data.frame(lapply(network2[, paste0("lvl", 1:max.lvl)],
                                                          as.character))
network2[, paste0("lvl", 1:max.lvl)] <- lapply(network2[, paste0("lvl", 1:max.lvl)], 
                                               function(x) gsub(" ", "_", x))
for(i in 1:nrow(tree_order))
{
  if(any(network2[, c("lvl1", "lvl2")] == tree_order$label[i], na.rm = TRUE))
  {
    tmp <- unique(network2[which(network2[, c("lvl1", "lvl2")] == tree_order$label[i],
                                 arr.ind = TRUE)[, 1], "col.intro.lvl1"])
    tree_order$color[i] <- tmp
  } else if(any(network2[, paste0("lvl", 3:max.lvl)] == tree_order$label[i], na.rm = TRUE))
  {
    tmp <- unique(network2[which(network2[, paste0("lvl", 3:max.lvl)] == tree_order$label[i],
                                 arr.ind = TRUE)[, 1], "col.intro.lvl1"])
    tree_order$color[i] <- tmp
  } else {
    tree_order$color[i] <- NA
  }
}
tree_order[which(is.na(tree_order$color)), "color"] <- "#808080"

intregions <- readRDS("./data/introregions_lvl1")

colnames(intregions) <- c("clvl", "color", "name")

treeint <- tree
treeorderint <- tree_order

ptrint <- ggtree(treeint, layout = "circular",
                 aes(color = treeorderint$color),
                 size = .05) +
  scale_color_identity(name = "",
                       labels = as.character(intregions$name),
                       guide = guide_legend(override.aes = list(
                         col = as.character(intregions$color),
                         size = 2),
                         order = 1)) +
  theme(legend.position = "left")

# ptrint

# cairo_pdf("outputs/tree.pdf")
# ptrnat
# dev.off()

# ------ Extinctions only network -----


native.ext.net <- readRDS("./data/native.extnetwork.RDS")

max.lvl <- 6

# Native without extinctions
fish.native.ext <- as.data.frame(readRDS("./data/fish_native_extinctions.rds"))
fish.native.ext$X1.Basin.Name <- as.factor(fish.native.ext$X1.Basin.Name)
fish.native.ext$X6.Fishbase.Valid.Species.Name <- as.factor(fish.native.ext$X6.Fishbase.Valid.Species.Name)

bioregions.lvl1 <- readRDS("./data/native.extregions_lvl1")
bioregions.lvl2 <- readRDS("./data/native.extregions_lvl2")

bioregions.lvl1$name <- gsub("\\)", "", gsub("\\(", "", bioregions.lvl1$name))
bioregions.lvl2$name <- gsub("\\)", "", gsub("\\(", "", bioregions.lvl2$name))

fish.net.tree <- native.ext.net
fish.net.tree$Name <- as.character(fish.net.tree$Name)
# Level 1
levels(fish.net.tree$lvl1)[which(!(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1) &
                                   nchar(levels(fish.net.tree$lvl1)) <= 4)] <- 
  paste0("Cluster ", levels(fish.net.tree$lvl1)[which(!(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1) &
                                                        nchar(levels(fish.net.tree$lvl1)) <= 4)])
levels(fish.net.tree$lvl1)[which(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1)] <- 
  na.omit(bioregions.lvl1$name[
    match(bioregions.lvl1$clvl1, levels(fish.net.tree$lvl1)[which(levels(fish.net.tree$lvl1) %in% bioregions.lvl1$clvl1)])
  ])

# Level 2
levels(fish.net.tree$lvl2)[which(nchar(levels(fish.net.tree$lvl2)) <= 4)] <- 
  paste0("Cluster ", levels(fish.net.tree$lvl2)[which(nchar(levels(fish.net.tree$lvl2)) <= 4)])

# devtools::instnative.ext_github("Farewe/biogeonetworks")
fish.sites.tree <-  biogeonetworks::getSiteTable(fish.native.ext, site.field = "X1.Basin.Name", network = fish.net.tree)

lvl0 = "Earth"
network <- fish.sites.tree
# network <- network[-which(network$color.lvl2 %in% "#808080"), ]
levels = paste0("lvl", 1:max.lvl)

# Workaround necessary to avoid solitary leaves on solitary nodes (which will break plotting programs)
netw <- network[, levels]
for (ilvl in 1:(max.lvl - 1)) #NLVL - 1
{
  lvl <- paste0("lvl", ilvl)
  for (s in levels(netw[, lvl]))
  {
    if(nrow(netw[netw[, lvl] %in% s, ]) == 1)
    {
      cur.line <- which(netw[, lvl] %in% s)
      if(any(is.na((netw[cur.line, levels])))) {
        if(ilvl + 2 == min(which(is.na(netw[cur.line, levels]))) |
           (ilvl + 1 == length(levels) & !is.na(netw[cur.line, ilvl + 1])))
        {
          levels(netw[, lvl])[levels(netw[, lvl]) == s] <- as.character(netw[cur.line, 
                                                                             (grep(lvl, colnames(netw)) + 1)])
          netw[cur.line, grep(lvl, colnames(netw)) + 1] <- NA
        }
      }
    }
  }
}
network[, levels] <- netw



if(length(levels(network[, levels[1]])) == 1)
{
  network$pathString <- apply(network[, levels], 1, paste, collapse = "/")
} else
{
  network$pathString <- paste(lvl0, apply(network[, levels], 1, paste, collapse = "/"), sep = "/")
}

network$pathString <- gsub("/NA", "", network$pathString)

network.tree <- data.tree::as.Node(network)

# Converting to newick tree & applying workaround to have branch length of 1
newicktree <- data.tree::ToNewick(network.tree, 
                                  heightAttribute = function(x) 1)
# Previous line creates branch length of 0. Changing everything to 1
newicktree <- gsub(":0", ":1", newicktree)
write(newicktree, "./data/trees/native.ext_newick.tree")


tree <- ape::read.tree(text = newicktree)

treelabels <- c(tree$tip.label, tree$node.label)


tree_order <- as.tibble(tree)
tree_order$color <- NA

# Need to rename nodes, changing spaces to _, to match the tree labels
network2 <- network
network2[, paste0("lvl", 1:max.lvl)] <- data.frame(lapply(network2[, paste0("lvl", 1:max.lvl)],
                                                          as.character))
network2[, paste0("lvl", 1:max.lvl)] <- lapply(network2[, paste0("lvl", 1:max.lvl)], 
                                               function(x) gsub(" ", "_", x))
for(i in 1:nrow(tree_order))
{
  if(any(network2[, c("lvl1", "lvl2")] == tree_order$label[i], na.rm = TRUE))
  {
    tmp <- unique(network2[which(network2[, c("lvl1", "lvl2")] == tree_order$label[i],
                                 arr.ind = TRUE)[, 1], "col.native.ext.lvl1"])
    tree_order$color[i] <- tmp
  } else if(any(network2[, paste0("lvl", 3:max.lvl)] == tree_order$label[i], na.rm = TRUE))
  {
    tmp <- unique(network2[which(network2[, paste0("lvl", 3:max.lvl)] == tree_order$label[i],
                                 arr.ind = TRUE)[, 1], "col.native.ext.lvl2"])
    tree_order$color[i] <- tmp
  } else {
    tree_order$color[i] <- NA
  }
}
tree_order[which(is.na(tree_order$color)), "color"] <- "#808080"

bioregions.lvl1 <- readRDS("./data/native.extregions_lvl1")
bioregions.lvl2 <- readRDS("./data/native.extregions_lvl2")

colnames(bioregions.lvl1) <- colnames(bioregions.lvl2) <- c("clvl", "color", "name")
treenat.ext <- tree
treeordernat.ext <- tree_order
nat.extregions <- rbind(bioregions.lvl1[-3, ],
                        bioregions.lvl2)

ptrnat.ext <- ggtree(treenat.ext, layout = "circular",
                 aes(color = treeordernat.ext$color),
                 size = .05) +
  scale_color_identity(name = "",
                       labels = as.character(nat.extregions$name),
                       guide = guide_legend(override.aes = list(
                         col = as.character(nat.extregions$color),
                         size = 2),
                         order = 1)) +
  theme(legend.position = "right")

# ptrnat.ext

# cairo_pdf("outputs/tree.pdf")
# ptrnat
# dev.off()

# Define same scale for all plots
thememaps <- theme(legend.key.size = unit(0.6, "cm"),
                   legend.text = element_text(size = 15),
                   axis.text = element_text(size = 15),
                   axis.title = element_text(size = 15),
                   legend.title = element_text(size = 15),
                   panel.grid = element_blank(),
                   line = element_blank(),
                   rect = element_blank())


ptrall <- ptrall + geom_segment(aes(x=0, xend=6, y=0, yend=1), colour='transparent', lwd=2) +
  thememaps
ptrnat <- ptrnat + geom_segment(aes(x=0, xend=6, y=0, yend=1), colour='transparent', lwd=2) +
  thememaps
ptrint <- ptrint + geom_segment(aes(x=0, xend=6, y=0, yend=1), colour='transparent', lwd=2) +
  thememaps
ptrnat.ext <- ptrnat.ext + geom_segment(aes(x=0, xend=6, y=0, yend=1), colour='transparent', lwd=2) +
  thememaps

cairo_pdf("./outputs/Figure 3.pdf", width = 12, height = 8,
          pointsize = 6)
egg::ggarrange(ptrnat, ptrall, 
               ptrint, ptrnat.ext,
               labels = c("A. Natural regions",
                          "B. Anthropocene regions",
                          "C. Introduction-only regions",
                          "D. Extirpation-only regions"),
               newpage = FALSE,
               nrow = 2,
               label.args = list(gp = grid::gpar(font = 4, cex = 3)))
dev.off()



# ggall <-  ggplot_build(ptrall)
# ggall$layout$panel_params[[1]]$r.range <- c(0, 6)
# ptrall <- ggplot_gtable(ggall)
# 
# ggall <-  ggplot_build(ptrint)
# ggall$layout$panel_params[[1]]$r.range <- c(0, 6)
# ptrint <- ggplot_gtable(ggall)
# plot(ptrint)

# par mfrow sur les lignes dessus ?
