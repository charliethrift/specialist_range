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




