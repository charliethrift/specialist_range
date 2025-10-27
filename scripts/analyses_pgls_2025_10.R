# PGLS for bee phylogeny
# 24 october 2025
# charles thrift

setwd("~/specialist_bees/range_size/scripts")

# 0. Libraries
library(tidyverse)
library(ape)
library(phytools)
library(picante)
library(caper)


# 1. Read Data
data <- read_csv("../data/data_2025_09.csv")
genus_data <- data %>% filter(!is.na(diet_genera_rareCount))
data <- data %>% dplyr::select(-diet_genera_rareCount)
data <- data %>% mutate(log_area = log(eoo_size_m2),
                        log_diet = log(diet_families_rareCount))

mytree <- read.tree('../data/bee_matrix_species.nwk') #species level tree


# 2. Root Tree
outgroups_sp <- c("Philanthus_n4m2_Philanthidae")
mytree <- root(mytree,outgroup=outgroups_sp, resolve.root = TRUE)



# 3. Prune Tree
mytree$tip.label <- sub("^([A-Za-z]+)_([a-z]+)_.*",
                        "\\1 \\2", mytree$tip.label) # Extract the species from 
#the tip labels, format is "genus_species_numbers_family"




tree_tips <- mytree$tip.label
bee_species <- c(unique(data$bee_species),outgroups_sp) # bee species plus the outgroup
matching_names <- tree_tips[!tree_tips %in% bee_species]
trimmed_tree <- drop.tip(mytree, matching_names)
trimmed_tree$tip.label #check labels
plot(trimmed_tree)

branch_lengths <- trimmed_tree$edge.length


# trimmed_tree object now contains 520 species from the original 633
lengths <- as.data.frame(trimmed_tree$tip.label)
lengths <- rename(lengths, species = "trimmed_tree$tip.label")

bees1 <- data %>% filter(bee_species %in% lengths$species)
# can grab my species list from bees1$species here...
bees1 <- bees1 %>% rename(species = bee_species)
df <- as.data.frame(left_join(bees1,lengths, by="species"))
# df now has the bee data of interest for the model, only using species in the trimmed tree


# 4. Make Ultrametric
tree_ultra <- chronos(trimmed_tree) # takes ~ 60 seconds to run


# 5. Run PGLS
tree_ultra$node.label <- NULL # drop node labels prior to running pgls

comp_data <- comparative.data(phy = tree_ultra, 
                              data = df, 
                              names.col = "species", 
                              vcv = TRUE)
model <- pgls(log_area ~ log_diet, data = comp_data, lambda = "ML")
summary(model)
plot(model)
#mod.l <- pgls.profile(model,'lambda')
#plot(mod.l)
#summary(model)$param["lambda"]


# read in phylogenetic diet breadth df
phylo_df <- read.csv("../data/phylo_numbers_df.csv")
phylo_df <- phylo_df %>% rename(species = bee_species)

df_phylo <- phylo_df %>% filter(species %in% lengths$species)
tree_ultra$node.label <- NULL # drop node labels prior to running pgls

comp_data2 <- comparative.data(phy = tree_ultra, 
                              data = df_phylo, 
                              names.col = "species", 
                              vcv = TRUE)
model_hill <- pgls(log_area ~ log_hill, data = comp_data2, lambda = "ML")
summary(model_hill)



####################################################
# 2. Supplemental: Repeat analysis using only species for which we have 15 or more occurrence records
####################################################
df_s1 <- df %>% filter(count_in_Dorey >= 15)
comp_data_s1 <- comparative.data(phy = tree_ultra, 
                              data = df_s1, 
                              names.col = "species", 
                              vcv = TRUE)
model_s1 <- pgls(log_area ~ log_diet, data = comp_data_s1, lambda = "ML")
summary(model_s1)

df_s2 <- df_phylo %>% filter(count_in_Dorey >= 15)
comp_data_s2 <- comparative.data(phy = tree_ultra, 
                              data = df_s2, 
                              names.col = "species", 
                              vcv = TRUE)
model_s2 <- pgls(log_area ~ log_hill, data = comp_data_s2, lambda = "ML")
summary(model_s2)

###### Repeat analyses on subset (n = 174) of species with genus-level diet data
genus_data$log_gen_diet <- log(genus_data$diet_genera_rareCount)
genus_data$log_area <- log(genus_data$eoo_size_m2)
genus_data$log_diet <- log(genus_data$diet_families_rareCount)
bee_species_gen <- c(unique(genus_data$bee_species),outgroups_sp) # bee species plus the outgroup
matching_names_gen <- tree_tips[!tree_tips %in% bee_species_gen]
trimmed_tree_gen <- drop.tip(mytree, matching_names_gen)
trimmed_tree_gen$tip.label #check labels
plot(trimmed_tree_gen)
tree_ultra_gen <- chronos(trimmed_tree_gen)
tree_ultra_gen$node.label <- NULL # drop node labels prior to running pgls
genus_data <- as.data.frame(genus_data)
comp_data_gen <- comparative.data(phy = tree_ultra_gen,
                                  data = genus_data,
                                  names.col = "bee_species", 
                                  vcv = TRUE)
gen_mod <- pgls(log_area ~ log_gen_diet, data = comp_data_gen, lambda = "ML")
summary(gen_mod)
# repeat with phylogenetic diet breadth
genus_phy_data <- read_csv("../data/phylo_numbers_df_genus.csv")
genus_phy_data$log_hill_gen <- log(genus_phy_data$genus_hill)
genus_phy_data <- as.data.frame(genus_phy_data)
comp_data_gen_phylo <- comparative.data(phy = tree_ultra_gen,
                                  data = genus_phy_data,
                                  names.col = "bee_species", 
                                  vcv = TRUE)
gen_mod_phylo <- pgls(log_area ~ log_hill_gen, data = comp_data_gen_phylo, lambda = "ML")
summary(gen_mod_phylo)



#### Compare to the regular, family-level diet dataset, subset to those same 174 species
# to do this, just use the same code and data frame, with "log_diet" column

gen_mod_fam <- pgls(log_area ~ log_diet, data = comp_data_gen, lambda = "ML")
summary(gen_mod_fam)

## Compare with family-level phylogenetic diet, subset to the same 174 species
fam_phy_data <- read_csv("../data/phylo_numbers_df_fam_compare.csv")
fam_phy_data <- as.data.frame(fam_phy_data)
comp_data_fam_phylo <- comparative.data(phy = tree_ultra,
                                        data = fam_phy_data,
                                        names.col = "bee_species", 
                                        vcv = TRUE)
fam_mod_phylo <- pgls(log_area ~ log_hill, data = comp_data_fam_phylo, lambda = "ML")
summary(fam_mod_phylo)


