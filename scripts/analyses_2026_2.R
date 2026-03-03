# analyses
# 3 December 2025
# Charles Thrift, An Bui

####################################################
# 0. Load Packages
####################################################
library(tidyverse) # general use
library(ggeffects) # get model predictions
library(emmeans) # marginal means
library(glmmTMB) # model GLM
library(sf) # spatial
library(rnaturalearth) # spatial
library(DHARMa) # check residuals
library(ggtext)

setwd("~/specialist_bees/range_size/scripts")

####################################################
# 1. Read Data, Log-Transform, Set Colors
####################################################
data <- read_csv("../data/data_2025_09.csv")
genus_data <- data %>% dplyr::filter(!is.na(diet_genera_rareCount))
data <- data %>% dplyr::select(-diet_genera_rareCount)
data <- data %>% dplyr::rename(diet_numeric = diet_families_rareCount) %>% 
  mutate(bee_family = as.factor(bee_family),
         diet_binary = as.factor(diet_binary),
         eoo_size_km2 = eoo_size_m2/(1e6))
spatial <- read_sf("../data/clipped_hulls_dietFamily_july25/clipped_hulls_dietFamily_july25.shp")

family_colors <- c("Andrenidae" = "#DDCC77",
                   "Apidae" = "#88CCEE",
                   "Colletidae" = "#332288",
                   "Halictidae" = "#44AA99",
                   "Megachilidae" = "#AA4499",
                   "Melittidae" = "#fc5603")
diet_colors <- c("Specialist" = "#88CCEE", 
                 "Generalist" = "#DDCC77")

####################################################
# 2. Correlation Test
####################################################
cor.test(data$eoo_size_km2, data$diet_numeric, method = "spearman")

####################################################
# 3. Generalized Linear Models
####################################################
mod <- glmmTMB(eoo_size_km2 ~ diet_numeric + bee_family, 
               data = data, family = Gamma(link = "log"))
mod_int <- glmmTMB(eoo_size_km2 ~ diet_numeric * bee_family, 
                   data = data, family = Gamma(link = "log"))
AIC(mod, mod_int) %>%  
  dplyr::arrange(AIC) # mod1: 2881.73, mod2: 2882.12
AIC(mod)-AIC(mod_int) # delta AIC = 2.55

plot(simulateResiduals(mod))
plot(simulateResiduals(mod_int))


summary(mod)
#tidy_mod_add <- tidy(mod, effects = "fixed", conf.int = TRUE)
#ft_add <- as_flextable(tidy_mod_add,random = TRUE)
#ft_add
#tidy_mod_int <- tidy(mod_int, effects = "fixed", conf.int = TRUE)
#ft_int <- as_flextable(tidy_mod_int,random = TRUE)
#ft_int
#summary(mod_int)

emm_df <- emmeans(mod, specs = "bee_family", regrid = "response") %>% 
  as_tibble()

ggplot(emm_df, aes(x = bee_family, y = response, color = bee_family))+
  geom_pointrange(aes(ymin = asymp.LCL,
                      ymax = asymp.UCL))+
  labs(x = "Bee family",
       y = "Estimated mean range size (km<sup>2</sup>)")+
  scale_color_manual(values = family_colors)+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.y = element_markdown())

ggsave(width = 16, 
       height = 12,
       units = "cm",
       dpi = 300,
       bg = "white",
       "../figures/glm_family_means.png")

df_predictions_fams <- ggeffects::ggpredict(
  mod,terms = c("diet_numeric", "bee_family")) %>% 
  rename(bee_family = group)
df_predictions <- ggeffects::ggpredict(
  mod,terms = c("diet_numeric")) %>% 
  rename(bee_family = group)

glm1a <- ggplot(data = data, aes(x = diet_numeric, y = eoo_size_km2)) +
  geom_point(aes(color = bee_family)) +
  scale_color_manual(values = family_colors)+
  geom_ribbon(data = df_predictions,
              aes(x = x,y = predicted,ymin = conf.low,ymax = conf.high),
              alpha = 0.3) +
  geom_line(data = df_predictions,
            aes(x = x,y = predicted)) +
  theme_bw()+
  labs(color = "Bee Family",
       x = "Diet breadth",
       y = "Range size (km<sup>2</sup>)")+
  theme(text = element_text(size = 10),
        axis.title.y = element_markdown(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(width = 16, 
       height = 8,
       units = "cm",
       dpi = 300,
       "../figures/glm_1_notSciNotation.png")

new_labels <- c(
  "Andrenidae" = "Andrenidae (n = 188)",
  "Apidae" = "Apidae (n = 108)",
  "Colletidae" = "Colletidae (n = 83)",
  "Halictidae" = "Halictidae (n = 85)",
  "Megachilidae" = "Megachilidae (n = 150)",
  "Melittidae" = "Melittidae (n = 19)")

glm1b <- ggplot(data = data, aes(x = diet_numeric, y = eoo_size_km2)) +
  geom_point(aes(color = bee_family)) +
  scale_color_manual(values = family_colors)+
  scale_fill_manual(values = family_colors)+
  geom_ribbon(data = df_predictions_fams,
              aes(x = x,y = predicted,
                  ymin = conf.low,ymax = conf.high,
                  fill = bee_family),
              alpha = 0.6) +
  geom_line(data = df_predictions_fams,
            aes(x = x,y = predicted),color = "black") +
  theme_bw()+
  facet_wrap(~ bee_family,labeller = as_labeller(new_labels))+
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white"),
        text = element_text(size = 10),
        axis.title.y = element_markdown(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Diet breadth",
       y = "Range size (km<sup>2</sup>)")

cowplot::plot_grid(glm1a, glm1b, ncol = 1, nrow = 2)
ggsave(width = 16, 
       height = 12,
       units = "cm",
       dpi = 300,
       "../figures/glm_family.png")

####################################################
# 4. Categorical Diet Analysis
####################################################
gamma_binary <- glmmTMB::glmmTMB(
  eoo_size_km2 ~ diet_binary, data = data,
  family = Gamma(link = "log"))

summary(gamma_binary)
#tidy_mod_bin <- tidy(gamma_binary, effects = "fixed", conf.int = TRUE)
#ft_bin <- as_flextable(tidy_mod_bin,random = TRUE)
#ft_bin
exp(confint(gamma_binary))

df_predictions_cat <- ggeffects::ggpredict(
  gamma_binary,terms = c("diet_binary"))

mean_spec <- df_predictions_cat$predicted[2]
mean_gen <- df_predictions_cat$predicted[1]
df_predictions_cat$predicted[1]/df_predictions_cat$predicted[2] # mean ratio

ggplot(data = data,
       aes(x = eoo_size_km2, fill = diet_binary))+
  geom_density(alpha = 0.7, color = "black")+
  geom_rug(aes(color = diet_binary))+
  scale_fill_manual(values = diet_colors)+
  scale_color_manual(values = diet_colors)+
  theme_bw()+
  labs(x = "Range size (km<sup>2</sup>)",
       y = "Density",
       fill = "Diet Breadth")+
   theme(legend.position = "inside",
         legend.position.inside = c(.85,.8),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.title.x = element_markdown())+
  guides(color = "none")+
  geom_vline(aes(xintercept = mean_spec),color = "#88CCEE",linetype = "dashed")+ # mean
  geom_vline(aes(xintercept = mean_gen),color = "#DDCC77",linetype = "dashed") # mean

ggsave(width = 16, 
       height = 8,
       units = "cm",
       dpi = 300,
       "../figures/density_glm.png")

emm_df_binary <- emmeans(gamma_binary, specs = "diet_binary", 
                         regrid = "response") %>% 
  as_tibble()

ggplot(emm_df_binary, aes(x = diet_binary, 
                          y = response))+
  geom_point(data = data,
             aes(x = diet_binary, 
                 y = eoo_size_km2, 
                 color = diet_binary),
             alpha = 0.8,
             shape = 21,
             position = position_jitter(width = 0.2, height = 0, seed = 666)) +
  geom_pointrange(aes(ymin = asymp.LCL,
                      ymax = asymp.UCL),
                      color = "black")+
  labs(x = "Diet breadth",
       y = "Estimated mean range size (km<sup>2</sup>)")+
  scale_color_manual(values = diet_colors)+
  scale_y_continuous(labels = scales::label_number())+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.y = element_markdown())
ggsave(width = 8, 
       units = "cm",
       dpi = 300,
       bg = "white",
       "../figures/binary_glm.png")

ggplot(emm_df_binary, aes(x = diet_binary, 
                          y = response))+
  geom_pointrange(aes(ymin = asymp.LCL,
                      ymax = asymp.UCL),
                  color = "black")+
  labs(x = "Diet breadth",
       y = "Estimated mean range size (km<sup>2</sup>)")+
  scale_color_manual(values = diet_colors)+
  scale_y_continuous(labels = scales::label_number())+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.y = element_markdown())

ggsave(width = 8, 
       units = "cm",
       dpi = 300,
       bg = "white",
       "../figures/binary_glm_noraw.png")

####################################################
# 6. Spatial Data Maps 
####################################################
species_combined <- spatial %>%
  group_by(species) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf()
spatial_sp <- species_combined #%>% filter(species %in% data$bee_species)
world_map <- ne_countries(scale = "medium", returnclass = "sf")
world_map <- world_map[,c("admin","geometry")]
france <- subset(world_map,admin == "France") #edit french guiana to not be equal to france for the choropleth map
boundingbox <- st_as_sfc(st_bbox(c(xmin = -15, xmax = 15, ymin = 34, ymax = 60),
                                 crs = st_crs(4326)))
st_crs(france)==st_crs(boundingbox)
france_filtered <- st_intersection(france,boundingbox)
world_map_new <- rbind(world_map,france_filtered)
world_map_new <- world_map_new[-161,]

ggplot()+
  geom_sf(data = spatial_sp, fill = "darkblue",color = NA,alpha = 0.01) +
  theme_minimal()+
  labs(x = "Longitude", 
       y = "Latitude")+
  coord_sf(expand = FALSE) +
  theme(text = element_text(size = 18))+
  scale_y_continuous(breaks = c(-40,0,40),
                     labels = c("-40\u00b0","0\u00b0","40\u00b0"))+
  scale_x_continuous(breaks = c(-120,-60,0,60,120),
                     labels = c("-120\u00b0","-60\u00b0",
                                "0\u00b0","60\u00b0","120\u00b0"))


world_map_new <- st_transform(world_map_new, 3857)
spatial_sp <- st_transform(spatial_sp, 3857)
st_crs(world_map_new) == st_crs(spatial_sp)
intersections <- st_intersection(spatial_sp,world_map_new)
overlap_counts <- intersections %>% 
  st_drop_geometry() %>% 
  group_by(admin) %>% 
  summarise(unique_species_n = n_distinct(species),
            list_species = toString(unique(species)))
world_map_counts <- world_map_new %>% 
  left_join(overlap_counts,by = "admin")
world_map_counts <- st_transform(world_map_counts,4326)



####################################################
# 7. Phylogenetic Distance as Diet Metrics
####################################################
# Different Script

####################################################
# 8. Account for Bee Phylogeny with Phylogenetic Generalized Least Squares
####################################################
# Different Script

####################################################
# 9. Supplemental: Repeat analysis using only species for which we have 15 or more occurrence records
####################################################
sdf <- data %>% dplyr::filter(count_in_Dorey >= 15)
cor.test(sdf$eoo_size_km2, sdf$diet_numeric, method = "spearman") #0.42, p < 0.001
gamma_binary_sdf <- glmmTMB::glmmTMB(
  eoo_size_km2 ~ diet_binary, data = sdf,
  family = Gamma(link = "log"))
summary(gamma_binary_sdf) 
df_predictions_cat_sdf <- ggeffects::ggpredict(
  gamma_binary_sdf,terms = c("diet_binary"))
df_predictions_cat_sdf$predicted[1]/df_predictions_cat_sdf$predicted[2] # mean ratio
smod <- glmmTMB(eoo_size_km2 ~ diet_numeric + bee_family, 
               data = sdf, family = Gamma(link = "log"))
summary(smod)
tidy_mod_bin_sdf <- tidy(gamma_binary_sdf, effects = "fixed", conf.int = TRUE)
ft_bin_sdf <- as_flextable(tidy_mod_bin_sdf,random = TRUE)
ft_bin_sdf
tidy_smod <- tidy(smod, effects = "fixed", conf.int = TRUE)
ft_tidy_smod <- as_flextable(tidy_smod,random = TRUE)
ft_tidy_smod

###############################
# Compare to genus-level diet data
###############################
genus_data <- genus_data %>% 
  mutate(diet_numeric_families = diet_families_rareCount,
         diet_numeric_genera = diet_genera_rareCount,
         eoo_size_km2 = eoo_size_m2/1e6,
         diet_binary = as.factor(diet_binary))
# to compare with fam data, use genus_data$diet_numeric_families
cor.test(genus_data$eoo_size_km2, 
         genus_data$diet_numeric_families, 
         method = "spearman") #0.60
cor.test(genus_data$eoo_size_km2, 
         genus_data$diet_numeric_genera, 
         method = "spearman") #0.61

# the gamma glm binary predictor tests are identical; have identical results
gamma_binary_genus <- glmmTMB::glmmTMB(
  eoo_size_km2 ~ diet_binary, data = genus_data,
  family = Gamma(link = "log"))
summary(gamma_binary_genus) 
gamma_binary_genus <- glmmTMB::glmmTMB(
  eoo_size_km2 ~ diet_binary, data = genus_data,
  family = Gamma(link = "log"))
summary(gamma_binary_genus) 
tidy_mod_bin_genus <- tidy(gamma_binary_genus, effects = "fixed", conf.int = TRUE)
ft_bin_genus <- as_flextable(tidy_mod_bin_genus,random = TRUE)
ft_bin_genus

df_predictions_cat_genus <- ggeffects::ggpredict(
  gamma_binary_genus,terms = c("diet_binary"))
df_predictions_cat_genus$predicted[1]/df_predictions_cat_genus$predicted[2] # mean ratio


smodgen <- glmmTMB(eoo_size_km2 ~ diet_numeric_genera + bee_family, 
                data = genus_data, family = Gamma(link = "log"))
DHARMa::simulateResiduals(smodgen, plot = TRUE)
summary(smodgen)
smodfam <- glmmTMB(eoo_size_km2 ~ diet_numeric_families + bee_family, 
                   data = genus_data, family = Gamma(link = "log"))
DHARMa::simulateResiduals(smodfam, plot = TRUE)
summary(smodfam)
tidy_mod_genus <- tidy(smodgen, effects = "fixed", conf.int = TRUE)
ft_bin_genus_num <- as_flextable(tidy_mod_genus,random = TRUE)
ft_bin_genus_num
tidy_mod_genus_fam <- tidy(smodfam, effects = "fixed", conf.int = TRUE)
ft_bin_genus_num_fam <- as_flextable(tidy_mod_genus_fam,random = TRUE)
ft_bin_genus_num_fam


