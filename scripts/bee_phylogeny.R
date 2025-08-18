# incorporate bee phylogeny
# 15 August 2025
# charles thrift

setwd("~/specialist_bees/range_size/scripts")

# 0. Libraries
library(tidyverse)
library(ape)
library(phytools)
library(picante)
library(caper)


# 1. Read Data
bees <- read_csv('../data/data_clean_dietFamily.csv') 
rare_data <- read_csv("../../pollen_data/family_rarified.csv")
rare_data <- rare_data[,c(4,5)]
data <- left_join(bees,rare_data,by="species")
data <- data %>% drop_na(Rare10)
bee_species <- unique(data$species)
data$log_area <- log(data$area)
data$log_diet <- log(data$Rare10)

mytree <- read.tree('../data/bee_matrix_species.nwk') #species level tree



# 2. Root Tree
outgroups_sp <- c("Philanthus_n4m2_Philanthidae",
                  "Pulverro_n4m0_Ammoplanidae",
                  "Bembix_n2m0_Bembicidae",
                  "Tachysphex_n4m0_Crabronidae",
                  "Cerceris_n3m1_Philanthidae")
outgroups_sp <- c("Philanthus_n4m2_Philanthidae")
mytree <- root(mytree,outgroup=outgroups_sp, resolve.root = TRUE)



# 3. Prune Tree

mytree$tip.label <- sub("^([A-Za-z]+)_([a-z]+)_.*", "\\1 \\2", mytree$tip.label) # Extract the species from the tip labels, format is "genus_species_numbers_family"


# Keep only the tips which are species in my species list, drop others
tree_tips <- mytree$tip.label
bee_species <- c(unique(data$species),outgroups_sp) # bee species plus the outgroup
matching_names <- tree_tips[!tree_tips %in% bee_species]
trimmed_tree <- drop.tip(mytree, matching_names)
trimmed_tree$tip.label #check labels

plot(trimmed_tree)
trimmed_tree
plot.phylo(trimmed_tree, cex = .1, main="species level tree")

#check branch lengths
branch_lengths <- trimmed_tree$edge.length

keep <- c("Osmia conjuncta","Osmia atriventris",
          "Osmia bicolor","Osmia bucephala",
          "Osmia conjuncta","Osmia pumila", 
          "Panurginus albopilosus","Eucera interrupta", 
          'Bombus vagans',"Andrena w-scripta",
          'Megachile maritima','Halictus rubicundus',
          "Lasioglossum foxii")
new_bee_tree <- trimmed_tree
all_species <- new_bee_tree$tip.label
rm_species1 <- trimmed_tree$tip.label[!trimmed_tree$tip.label %in% keep]
very_pruned1 <- drop.tip(trimmed_tree,rm_species1)
keep2 <- c("Andrena aeneiventris",
           "Andrena aerinifrons",
           "Andrena afrensis",
           "Andrena afzeliella",
           "Andrena agilissima",
           "Andrena albopunctata",
           "Andrena alfkenella",
           "Andrena alleghaniensis",
           "Andrena allosa")
rm_species2 <- trimmed_tree$tip.label[!trimmed_tree$tip.label %in% keep2]
very_pruned2 <- drop.tip(trimmed_tree,rm_species2)

#plot and write new tree
plot.phylo(very_pruned1, cex = .7, main="Test, Various Species")
plot.phylo(very_pruned2, cex = .7, main="Test, Andrena Species")


# trimmed_tree object now contains 520 species from my original 633
lengths <- as.data.frame(trimmed_tree$tip.label)
lengths <- rename(lengths, species = "trimmed_tree$tip.label")

bees1 <- data %>% filter(species %in% lengths$species)
# can grab my species list from bees1$species here...
df <- as.data.frame(left_join(bees1,lengths, by="species"))
# df now has the bee data of interest for the model, only using species in the trimmed tree


# 4. Make Ultrametric
tree_ultra <- chronos(trimmed_tree)


# 5. Run PGLS
bees1 <- data %>% filter(species %in% lengths$species)
# can grab my species list from bees1$species here...
df <- as.data.frame(left_join(bees1,lengths, by="species"))
tree_ultra$node.label <- NULL # drop node labels prior to running pgls

comp_data <- comparative.data(phy = tree_ultra, 
                              data = df, 
                              names.col = "species", 
                              vcv = TRUE)
model <- pgls(log_area ~ log_diet, data = comp_data)
summary(model)
family_colors <- c("Andrenidae" = "#DDCC77",
                   "Apidae" = "#88CCEE",
                   "Colletidae" = "#332288",
                   "Halictidae" = "#44AA99",
                   "Megachilidae" = "#AA4499",
                   "Melittidae" = "#fc5603")
par(mfrow=c(2,2)) # Arrange plots in a 2x2 grid
plot(model)
par(mfrow=c(1,1))
# Extract fitted values and residuals
fitted_values <- fitted(model)
residuals <- residuals(model)

# Create a scatter plot
plot(comp_data$data$log_diet, comp_data$data$log_area,
     xlab = "Log Diet", ylab = "Log EOO",
     main = "PGLS Regression Plot")

# Add the phylogenetically corrected regression line
lines(comp_data$data$log_diet, fitted_values, col = "blue", lwd = 2)
x = seq(0,3, by = 0.01)
y = 27.05488 + 1.04175*x
lines(x, y, col = "red", lwd = 1)



ggplot(data, aes(log_diet,log_area))+
  geom_point(aes(color = beeFamily))+
  geom_abline(intercept = 27.05488,slope = 1.04175, color = "black")+
  scale_color_manual(values = family_colors)+
  theme_bw()+
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white"),
        text = element_text(size = 16))+
  labs(x = "Diet",
       y = "Log Extent of Occurrence")



