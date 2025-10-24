# analyses
# 24 October 2025
# Charles Thrift, An Bui

####################################################
# 0. Load Packages
####################################################
library(tidyverse)
library(ggeffects)
library(emmeans)
library(sf)
library(rnaturalearth)


####################################################
# 1. Read Data, Log-Transform, Set Colors
####################################################
data <- read_csv("../data/data_2025_09.csv")
data <- data %>% dplyr::select(-diet_genera_rareCount)
data <- data %>% mutate(log_area = log(eoo_size_m2),
                        log_diet = log(diet_families_rareCount))
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
# 2. Correlation Tests
####################################################
cor.test(data$log_area, data$log_diet, method = "pearson")

####################################################
# 3. Linear Models
####################################################
mod <- lm(log_area~log_diet * bee_family, data=data) # family and diet interactive effect

DHARMa::simulateResiduals(mod, plot = TRUE)

summary(mod)

emtrends(mod, var = "log_diet", specs = "bee_family")

df_predictions <- ggeffects::ggpredict(mod,terms = c("log_diet", "bee_family")) %>% 
  rename(bee_family = group)
df_predictions_1 <- ggeffects::ggpredict(mod,terms = c("log_diet")) %>% 
  rename(bee_family = group)


lm1a <- ggplot(data = data, aes(x = log_diet, y = log_area)) +
  geom_point(aes(color = bee_family)) +
  scale_color_manual(values = family_colors)+
  geom_ribbon(data = df_predictions_1,
              aes(x = x,y = predicted,ymin = conf.low,ymax = conf.high),
              alpha = 0.3) +
  geom_line(data = df_predictions_1,
            aes(x = x,y = predicted)) +
  theme_bw()+
  labs(color = "Bee Family",
       x = "Log Diet Breadth",
       y = "Log Range Size")+
  theme(text = element_text(size = 10))

new_labels <- c(
  "Andrenidae" = "Andrenidae (n = 188)",
  "Apidae" = "Apidae (n = 108)",
  "Colletidae" = "Colletidae (n = 83)",
  "Halictidae" = "Halictidae (n = 85)",
  "Megachilidae" = "Megachilidae (n = 150)",
  "Melittidae" = "Melittidae (n = 19)")

lm1b <- ggplot(data = data, aes(x = log_diet, y = log_area)) +
  geom_point(aes(color = bee_family)) +
  scale_color_manual(values = family_colors)+
  scale_fill_manual(values = family_colors)+
  geom_ribbon(data = df_predictions,
              aes(x = x,y = predicted,ymin = conf.low,ymax = conf.high,fill = bee_family),
              alpha = 0.6) +
  geom_line(data = df_predictions,
            aes(x = x,y = predicted),color = "black") +
  theme_bw()+
  facet_wrap(~ bee_family,labeller = as_labeller(new_labels))+
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white"),
        text = element_text(size = 10))+
  labs(x = "Log Diet Breadth",
       y = "Log Range Size")

cowplot::plot_grid(lm1a, lm1b, ncol = 1, nrow = 2)
ggsave(width = 16, 
       height = 12,
       units = "cm",
       dpi = 300,
       "../figures/lm_family.png")

####################################################
# 4. Extract Slopes from Linear Models
####################################################
family_slopes <- emtrends(mod, var = "log_diet", specs = "bee_family") |> 
  as.data.frame() |> 
  mutate(type = "family",
         sig = case_when(
           lower.CL > 0 ~ "yes",
           lower.CL <= 0 ~ "no"),
         sig = fct_relevel(as.factor(sig), "yes", "no"))

sig_shape <- c("yes" = 16,
               "no" = 21)

ggplot(data = family_slopes,
       aes(x = bee_family,
           y = log_diet.trend,
           color = bee_family)) +
  geom_hline(yintercept = 0, 
             linetype = 3,
             color = "darkgrey") +
  geom_pointrange(aes(shape = sig,
                      ymin = lower.CL,
                      ymax = upper.CL,
                      lty = sig),
                  position = position_dodge(width = 0.5),
                  size = 1,
                  linewidth = 1,
                  fill = "#FFFFFF") +
  scale_color_manual(values = family_colors,
                     guide = "none") +
  scale_shape_manual(values = sig_shape) +
  scale_linetype_discrete(guide = "none") +
  scale_y_continuous(limits = c(-3.5, 2.5)) +
  labs(x = "Bee Family",
       y = "Slope estimate and 95% CI",
       shape = "sig") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        axis.text.x = element_text(size = 11))

ggsave(width = 16, 
       height = 10,
       units = "cm",
       dpi = 300,
       "../figures/slopes_fam.png")


####################################################
# 5. Categorical Diet Analysis
####################################################
t.test(log_area ~ diet_binary, data = data) # t test
rstatix::cohens_d(log_area~diet_binary,data=data,var.equal = F) # cohen's d
sd <- data %>%  # get standard deviation and summary stats
  group_by(diet_binary) %>% 
  summarise(mean = mean(log_area), sd = sd(log_area), n = n(),
            median = median(log_area))

ggplot(data = data,
       aes(x = log_area, fill = diet_binary))+
  geom_density(alpha = 0.8, color = "black")+
  geom_rug(aes(color = diet_binary))+
  scale_fill_manual(values = diet_colors)+
  scale_color_manual(values = diet_colors)+
  theme_bw()+
  labs(x = "Log Range Size",
       y = "Frequency",
       fill = "Diet Breadth")+
  # theme(text = element_text(size = 12))+
  guides(color = "none")+
  #    geom_vline(aes(xintercept = 27.72187),color = "#88CCEE",linetype = "dashed")+ # mean
  #    geom_vline(aes(xintercept = 29.31176),color = "#DDCC77",linetype = "dashed")+ # mean
  scale_x_continuous(limits = c(12,35))

ggsave(width = 16, 
       height = 8,
       units = "cm",
       dpi = 300,
       "../figures/Fig4_density.png")

ggplot(data = data,
       aes(x = eoo_size_m2, fill = diet_binary))+
  geom_density(alpha = 0.8, color = "black")+
  geom_rug(aes(color = diet_binary))+
  scale_fill_manual(values = diet_colors)+
  scale_color_manual(values = diet_colors)+
  theme_bw()+
  labs(x = "Range Size",
       y = "Frequency",
       fill = "Diet Breadth")+
  # theme(text = element_text(size = 18))+
  guides(color = "none")+
  scale_x_continuous(limits= c(0,1e14))

ggsave(width = 16, 
       height = 8,
       units = "cm",
       dpi = 300,
       "../figures/FigS1_density_nolog.png")


####################################################
# 6. Spatial Data Maps. 
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
sdf <- data %>% filter(count_in_Dorey >= 15)
cor.test(sdf$log_area, sdf$log_diet, method = "pearson") #0.38
t.test(log_area ~ diet_binary, data = sdf) # t test 
# p < 0.001 ; meanGeneralist = 29.5 ; meanSpecialist = 28.3
rstatix::cohens_d(log_area~diet_binary,data=sdf,var.equal = F) # cohen's d
# d = .63 ; effect size = moderate
smod <- lm(log_area~log_diet * bee_family, data=sdf) # family and diet interactive effect
DHARMa::simulateResiduals(smod, plot = TRUE)
summary(smod)
emtrends(smod, var = "log_diet", specs = "bee_family")

family_slopes_s <- emtrends(smod, var = "log_diet", specs = "bee_family") |> 
  as.data.frame() |> 
  mutate(type = "family",
         sig = case_when(
           lower.CL > 0 ~ "yes",
           lower.CL <= 0 ~ "no"),
         sig = fct_relevel(as.factor(sig), "yes", "no"))
# same slopes, where AND, API, COL, MEG = significant; HAL and MEL = NS