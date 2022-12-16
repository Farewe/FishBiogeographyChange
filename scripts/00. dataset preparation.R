fish.all <- read.csv("data/fish_occurrence_snapshot.csv")

##### Saving datasets #####
# Dataset with introduced fishes only
saveRDS(fish.all, file = "./data/fish_intro_only.rds")

# Dataset anthropocene (extinct + intro)
extinct.fishes <- fish.all[which(fish.all$Status_base1000 != ""), ]
saveRDS(extinct.fishes, file = "./data/extinct_fishes.rds")
fish.anthropocene <- fish.all[-which(fish.all$Status_base1000 != ""), ]
saveRDS(fish.anthropocene, file = "./data/fish_anthropocene.rds")


# Dataset native (no extinct / no intro)
fish.native <- fish.all[-which(fish.all$X3.Native.Exotic.Status == "exotic"), ]
fish.native <- droplevels(fish.native)
saveRDS(fish.native, file = "./data/fish_native.rds")

# Dataset native with extinctions 
fish.native.ext <- droplevels(fish.native[-which(fish.native$Status_base1000 != ""), ])
saveRDS(fish.native.ext, file = "./data/fish_native_extinctions.rds")