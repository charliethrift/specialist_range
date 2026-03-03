# diet with plant phylogenetic distance
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

tree_family <- read.tree("../data/family_tree_seed_plants.tre")

is.ultrametric(tree_family) # FALSE
Ntip(tree_family) #426 tips
tree_family$tip.label
list <- as.data.frame(tree_family$tip.label)

# get a list of plant families being visited
df <- read.csv("../../pollen_data/wood2023_supp2_data_forfamilyanalysis.csv")
df1 <- df[,c(5:123)] 
df1[df1 <= 2] <- NA
df1[df1 > 2] <- 1
df1[is.na(df1)] <- 0
df_pair <- as.data.frame(df[,3])
df_pair <- rename(df_pair, species = "df[, 3]")
community_matrix_fam <- cbind(df_pair,df1)
# filter community matrix down to just the species with rarefied plant info (633)
data <- data %>% rename(species = bee_species)
community_matrix_fam1 <- left_join(community_matrix_fam,data,by = "species")
community_matrix_fam1 <- community_matrix_fam1 %>%  drop_na(bee_family)
community_matrix_fam <- community_matrix_fam1[,-c(121:128)]

family_list1 <- community_matrix_fam[,-1]
family_list2 <- as.data.frame(colnames(family_list1))
family_list <- rename(family_list2, family = "colnames(family_list1)")
family_list$family[family_list$family == "Hydrangaceae"] <- "Hydrangeaceae"


# Prune Trees
tree_tips <- tree_family$tip.label
tree_tips <- as.data.frame(tree_tips)
tree_tips <- rename(tree_tips, family = "tree_tips")
matching_names_inverse <- anti_join(tree_tips,family_list,by = "family")
tips_to_remove <- as.character(matching_names_inverse$family)
trimmed_tree <- drop.tip(tree_family, tips_to_remove)
trimmed_tree$tip.label #check labels

# Make Ultrametric
tree_fam_ultra <- chronos(trimmed_tree) 

# Make community matrix for family level and genus level data
community_matrix_fam_filtered <- 
  community_matrix_fam[, colnames(community_matrix_fam) %in% trimmed_tree$tip.label]


### Make rarefied family level dataset
pollen_all <- read_csv("../../pollen_data/pollen_data_all.csv")
harmonize_bee <- read_csv("../../pollen_data/harmonize_bee_sp_names.csv")

pollen_all <- rename(pollen_all,sp_wood_og = Species)
pollen_all_fam <- inner_join(pollen_all,harmonize_bee, by = "sp_wood_og")
pollen_all_fam <- pollen_all_fam %>% 
  rename(bee_species = sp_wood_clean)
pollen_all_fam <- pollen_all_fam %>% 
  filter(Family != "Unknown")
pollen_all_fam <- pollen_all_fam %>% 
  filter(Family != "UNKNOWN")
pollen_all_fam1 <- pollen_all_fam[,-c(2,3,4,8)]
pollen_all_fam1 <- subset(pollen_all_fam1, Weight > 2)
# make a new ID column (some IDs are used on multiple specimens)
pollen_all_fam1 <- rename(pollen_all_fam1, ID = "ID#")
# rarefy this dataset!
bee_richness_fam <- pollen_all_fam1 %>%
  distinct(bee_species, ID) %>%
  count(bee_species, name = "n_samples")
bee_rich_5 <- subset(bee_richness_fam,n_samples >= 5)
bee_rich_10 <- subset(bee_richness_fam,n_samples >= 10)
bee_rich_20 <- subset(bee_richness_fam,n_samples >= 20)


pollen_df_fam <- subset(pollen_all_fam1, bee_species %in% bee_rich_10$bee_species)
set.seed(123)
sampled_ids <- pollen_df_fam %>% 
  distinct(bee_species, ID) %>% 
  group_by(bee_species) %>% 
  slice_sample(n = 10) %>% 
  ungroup()
pollen_df_fam_rare <- pollen_df_fam %>% 
  semi_join(sampled_ids, by = c("bee_species","ID"))

pollen_df_fam_rare <- rename(pollen_df_fam_rare, plant_family = Family)

comm_matrix_fam <- pollen_df_fam_rare %>%
  distinct(bee_species, plant_family) %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = plant_family,
    values_from = value,
    values_fill = list(value = 0))
comm_matrix_fam_filtered <- 
  comm_matrix_fam[, colnames(comm_matrix_fam) %in% trimmed_tree$tip.label]
comm_matrix_fam_filtered <- as.data.frame(comm_matrix_fam_filtered)



## Calculate phylogenetic diversity (as a diet breadth metric) through Hill Numbers
library(hillR)
family_list_3 <- as.data.frame(colnames(community_matrix_fam_filtered))
family_list_3 <- rename(family_list_3, family = "colnames(community_matrix_fam_filtered)")
tree_tips_fam_3 <- as.data.frame(trimmed_tree$tip.label)
tree_tips_fam_3 <- rename(tree_tips_fam_3, family = "trimmed_tree$tip.label")
matching_names_fam_inverse_3 <- anti_join(tree_tips_fam_3,family_list_3,by="family")
tips_to_remove_fam_3 <- as.character(matching_names_fam_inverse_3$family)
trimmed_tree_fam_3 <- drop.tip(trimmed_tree, tips_to_remove_fam_3)
trimmed_tree_fam_3$tip.label # 104 families
#trimmed_tree_fam_3_ultra <- chronos(trimmed_tree_fam_3)
#is.ultrametric(trimmed_tree_fam_3)

# read a diff community matrix that retains percent of each fam
cm <- read.csv("../../pollen_data/comm_matrix_raw.csv")
cm <- cm[,-c(1,2,4)]
cm[is.na(cm)] <- 0
cmf <- as.data.frame(cm[,colnames(cm) %in% trimmed_tree$tip.label])
cmfsp <- as.data.frame(cm$species)

trimmed_tree_chronos <- chronos(trimmed_tree)

fam_hill_2 <- hill_phylo(comm = cmf,
                         tree = trimmed_tree_chronos,
                         q = 2, # for inverse simpson
                         rel_then_pool = FALSE)
fam_hill_2_s_chr <- cbind(cmfsp,fam_hill_2)
fam_hill_2_s_chr <- rename(fam_hill_2_s_chr, bee_species = "cm$species")
data <- data %>% rename(bee_species = species)
fam_hill_2_s_chr_df <- inner_join(fam_hill_2_s_chr,data,by="bee_species")
fam_hill_2_s_chr_df$log_hill <- log(fam_hill_2_s_chr_df$fam_hill_2)
model_2_log <- lm(log_area ~ log_hill*bee_family, data = fam_hill_2_s_chr_df)
summary(model_2_log)
emtrends(model_2_log, var = "log_hill", specs = "bee_family")

# write csv, for use in PGLS in separate script
write.csv(fam_hill_2_s_chr_df, "../data/phylo_numbers_df.csv")


####################################################
# 2. Supplemental: Repeat analysis using only species for which we have 15 or more occurrence records
####################################################
sdfp <- fam_hill_2_s_chr_df %>% filter(count_in_Dorey >= 15)
mod_sdfp <- lm(log_area ~ log_hill*bee_family, data = sdfp)
summary(mod_sdfp)
emtrends(mod_sdfp, var = "log_hill", specs = "bee_family")

####################################################
# 3. Supplemental: Repeat analysis for genus-level diets
####################################################

# 1. Read Data
data <- read_csv("../data/data_2025_09.csv")
genus_data <- data %>% filter(!is.na(diet_genera_rareCount))
genus_data <- genus_data %>% 
  mutate(log_area = log(eoo_size_m2),
         log_diet_fam = log(diet_families_rareCount),
         log_diet_gen = log(diet_genera_rareCount))

tree_genus <- read.tree("../data/genus_tree_seed_plants.tre")

is.ultrametric(tree_genus) # FALSE
Ntip(tree_genus) #15498 tips
tree_genus$tip.label

# get a list of plant genera being visited
genus_list <- read_csv("../../pollen_data/genus_list_plants.csv") # 336 genera

# Prune Trees
tree_tips_genus <- as.data.frame(tree_genus$tip.label)
tree_tips_genus <- rename(tree_tips_genus, genus = "tree_genus$tip.label")
matching_names_genus_inverse <- anti_join(tree_tips_genus,genus_list,by="genus")
tips_to_remove_genus <- as.character(matching_names_genus_inverse$genus)
trimmed_tree_genus <- drop.tip(tree_genus, tips_to_remove_genus)
trimmed_tree_genus$tip.label # 298 genera 


# Make Ultrametric
tree_gen_ultra <- chronos(trimmed_tree_genus)

# Make community matrix for genus level data
pollen_all <- read_csv("../../pollen_data/pollen_data_all.csv")
harmonize_bee <- read_csv("../../pollen_data/harmonize_bee_sp_names.csv")
harmonize_pollen <- read_csv("../../pollen_data/harmonize_pollen_genera.csv")

pollen_all <- rename(pollen_all,sp_wood_og = Species)
pollen_all_match <- inner_join(pollen_all,harmonize_bee, by = "sp_wood_og")
pollen_all_match1 <- inner_join(pollen_all_match,harmonize_pollen, by = "Plant pollen 1")
pollen_all_match1 <- pollen_all_match1 %>% 
  rename(bee_species = sp_wood_clean,
         plant_genus = drop_fam_unk)
pollen_all_match_filtered <- pollen_all_match1[,-c(2,4,6,8,10,12:18)]
pollen_all_match_filtered <- subset(pollen_all_match_filtered, Weight > 2)
# make a new ID column (some IDs are used on multiple specimens)
pollen_all_match_filtered <- rename(pollen_all_match_filtered, ID = "ID#")
pollen_all_match_filtered$newID <- pollen_all_match_filtered
pollen_all_match_filtered <- pollen_all_match_filtered %>% 
  mutate(newID = paste(bee_species,ID,sep = "_"))
# rarefy this dataset!
bee_richness <- pollen_all_match_filtered %>%
  distinct(bee_species, newID) %>%
  count(bee_species, name = "n_samples")
bee_rich_5 <- subset(bee_richness,n_samples >= 5)
bee_rich_10 <- subset(bee_richness,n_samples >= 10)
bee_rich_20 <- subset(bee_richness,n_samples >= 20)

pollen_df <- subset(pollen_all_match_filtered, bee_species %in% bee_rich_10$bee_species)
set.seed(123)
sampled_ids <- pollen_df %>% 
  distinct(bee_species, ID) %>% 
  group_by(bee_species) %>% 
  slice_sample(n = 10) %>% 
  ungroup()
pollen_df_rare <- pollen_df %>% 
  semi_join(sampled_ids, by = c("bee_species","ID"))

comm_matrix_gen <- pollen_df_rare %>%
  distinct(bee_species, plant_genus) %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = plant_genus,
    values_from = value,
    values_fill = list(value = 0))
comm_matrix_gen_filtered <- 
  comm_matrix_gen[, colnames(comm_matrix_gen) %in% trimmed_tree_genus$tip.label]
comm_matrix_gen_filtered <- as.data.frame(comm_matrix_gen_filtered)

library(hillR)

genus_list1 <- as.data.frame(colnames(comm_matrix_gen_filtered))
genus_list1 <- rename(genus_list1, genus = "colnames(comm_matrix_gen_filtered)")
tree_tips_genus_1 <- as.data.frame(tree_gen_ultra$tip.label)
tree_tips_genus_1 <- rename(tree_tips_genus_1, genus = "tree_gen_ultra$tip.label")
matching_names_genus_inverse_1 <- anti_join(tree_tips_genus_1,genus_list1,by="genus")
tips_to_remove_genus_1 <- as.character(matching_names_genus_inverse_1$genus)
trimmed_tree_genus_1 <- drop.tip(tree_gen_ultra, tips_to_remove_genus_1)
trimmed_tree_genus_1$tip.label # 217 genera
trimmed_tree_genus_1_ultra <- chronos(trimmed_tree_genus_1)

genus_hill <- hill_phylo(comm = comm_matrix_gen_filtered,
                         tree = trimmed_tree_genus_1_ultra,
                         q = 2, # to get inverse simpson
                         rel_then_pool = FALSE)
genus_hill_scores <- cbind(comm_matrix_gen[1],genus_hill)
genus_data_scores_2 <- left_join(genus_data, genus_hill_scores, by = "bee_species")
# write this as a csv, for use in PGLS
write.csv(genus_data_scores_2, "../data/phylo_numbers_df_genus.csv")


# now, analyze
# phylogenetic linear regression
hist(genus_data_scores_2$genus_hill)
genus_data_scores_2$log_hill_gen <- log(genus_data_scores_2$genus_hill)
mod_gen_phy <- lm(log_area ~ log_hill_gen*bee_family, data = genus_data_scores_2)
summary(mod_gen_phy)

# compare with family-level data within these 174 species
fam_filtered <- fam_hill_2_s_chr_df %>% 
  filter(bee_species %in% genus_data_scores_2$bee_species)
mod_fam_phy <- lm(log_area ~ log_hill*bee_family, data = fam_filtered)
summary(mod_fam_phy)
write.csv(fam_filtered, "../data/phylo_numbers_df_fam_compare.csv")


