# TITLE
# 27 June 2025
# Charles Thrift

# 0. Libraries
library(tidyverse)
library(sf)
library(rnaturalearth)

# 1. Read Data
data <- read_csv("../data/dataframe_species_range_diet.csv")
spatial <- read_sf("../data/clipped_hulls/clipped_hulls_jun25.shp")

# 2. Log transform area and diet
data$log_area <- log(data$range_size_m2)
data$log_diet <- log(data$pollen_genera_count)
data$log_family_diet <- log(data$pollen_families_count)

# 3. Linear Models
mod_gen <- lm(log_area~log_diet,data=data)
mod_fam <- lm(log_area~log_family_diet,data=data)
mod_gen1 <- lm(log_area~log_diet + bee_family,data=data)
mod_fam1 <- lm(log_area~log_family_diet + bee_family,data=data)

summary(mod_gen) # area by diet (genus) (*Fig 1*)
summary(mod_fam) # area by diet (family) (*Supp Fig 1*)
summary(mod_gen1) # area by diet (genus) with bee families (*Fig 2*)
summary(mod_fam1) # area by diet (family) with bee families

fam_and_df <- subset(data,bee_family == "Andrenidae")
fam_api_df <- subset(data,bee_family == "Apidae")
fam_col_df <- subset(data,bee_family == "Colletidae")
fam_hal_df <- subset(data,bee_family == "Halictidae")
fam_meg_df <- subset(data,bee_family == "Megachilidae")

fam_and <- lm(log_area~log_diet,data=fam_and_df)
fam_api <- lm(log_area~log_diet,data=fam_api_df)
fam_col <- lm(log_area~log_diet,data=fam_col_df)
fam_hal <- lm(log_area~log_diet,data=fam_hal_df)
fam_meg <- lm(log_area~log_diet,data=fam_meg_df)

summary(fam_and) # (*Supp Table 3*)
summary(fam_api) # (*Supp Table 3*)
summary(fam_col) # (*Supp Table 3*)
summary(fam_hal) # (*Supp Table 3*)
summary(fam_meg) # (*Supp Table 3*)

# 4. Scatterplots
family_colors <- c("Andrenidae" = "#DDCC77",
                   "Apidae" = "#88CCEE",
                   "Colletidae" = "#332288",
                   "Halictidae" = "#44AA99",
                   "Megachilidae" = "#AA4499")
diet_colors <- c("genus specialist" = "#88CCEE", 
                 "generalist" = "#DDCC77")

ggplot(data)+ ### (*Fig 1.*)
  geom_jitter(aes(x = log_diet, y = log_area,color = bee_family), 
              alpha = 1, size = 3,width=0.1)+ 
  scale_color_manual(values = family_colors)+
  geom_smooth(aes(x = log_diet, y = log_area), 
              method = "lm",color = "black")+
  theme_bw()+
  labs(x = "Diet Breadth (Log Plant Genera)",
       y = "Range Size",
       "color" = "Bee Family")+
  theme(text = element_text(size = 18))

ggplot(data)+ ### (*Fig 2.*)
  geom_jitter(aes(x = log_diet, y = log_area,color = bee_family), 
              alpha = 1, size = 3,width=0.1)+ 
  scale_color_manual(values = family_colors)+
  geom_smooth(aes(x = log_diet, y = log_area), 
              method = "lm",color = "black")+
  theme_bw()+
  labs(x = "Diet Breadth (Log Plant Genera)",
       y = "Range Size",
       "color" = "Bee Family")+
  theme(text = element_text(size = 18))+
  facet_wrap(~bee_family_n,
             scales = "free")

ggplot(data)+ ### (*Supp Fig 1.*)
  geom_jitter(aes(x = log_family_diet, y = log_area,color = bee_family), 
              alpha = 1, size = 3,width=0.1)+ 
  scale_color_manual(values = family_colors)+
  geom_smooth(aes(x = log_family_diet, y = log_area), 
              method = "lm",color = "black")+
  theme_bw()+
  labs(x = "Diet Breadth (Log Plant Families)",
       y = "Range Size",
       "color" = "Bee Family")+
  theme(text = element_text(size = 18))

ggplot(data)+ ### (*Supp Fig 2.*)
  geom_jitter(aes(x = log_diet, y = log_area,color = bee_family), 
              alpha = 1, size = 3,width=0.1)+ 
  scale_color_manual(values = family_colors)+
  geom_smooth(aes(x = log_diet, y = log_area), 
              method = "lm",color = "black")+
  theme_bw()+
  labs(x = "Diet Breadth (Log Plant Genera)",
       y = "Range Size",
       "color" = "Bee Family")+
  theme(text = element_text(size = 18))+
  facet_wrap(~bee_family_n)


# 5. Density Plot Specialist/Generalist
ggplot(data = data, ### (*Fig 3*)
       aes(x = log_area, fill = diet_binary_genus))+
  geom_density(alpha = 0.8, color = "black")+
  scale_fill_manual(values = diet_colors)+
  theme_minimal()+
  labs(x = "Range Size",
       y = "Frequency",
       fill = "Diet Breadth")+
  theme(text = element_text(size = 18))+
  scale_x_continuous(limits = c(17,34))

t.test(log_area ~ diet_binary_genus, data = data) # t test
rstatix::cohens_d(log_area~dietg,data=data_dist,var.equal = F) # cohen's d
sd <- data_dist %>%  # get standard deviation and summary stats
  group_by(dietg) %>% 
  summarise(mean = mean(log_area), sd = sd(log_area), n = n())

# 6. Choropleth Map
species_combined <- spatial %>%
  group_by(species) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf()
spatial_sp <- species_combined %>% filter(species %in% data$bee_species)
world_map <- ne_countries(scale = "medium", returnclass = "sf")
world_map <- world_map[,c("admin","geometry")]
france <- subset(world_map,admin == "France") #edit french guiana to not be equal to france for the choropleth map
boundingbox <- st_as_sfc(st_bbox(c(xmin = -10, xmax = 100, ymin = 34, ymax = 60),
                         crs = st_crs(4326)))
st_crs(france)==st_crs(boundingbox)
france_filtered <- st_intersection(france,boundingbox)
world_map_new <- rbind(world_map,france_filtered)
world_map_new <- world_map_new[-161,]
ggplot()+
  geom_sf(data = world_map, fill = "lightgray",color = "black") + 
  geom_sf(data = spatial_sp, fill = "blue", alpha = 0.1) +
  theme_minimal()
world_map_new <- st_transform(world_map_new, 3857)
spatial_sp <- st_transform(spatial_sp, 3857)
st_crs(world_map_new) == st_crs(spatial_sp)
intersections <- st_intersection(spatial_sp,world_map_new)
overlap_counts <- intersections %>% 
  st_drop_geometry() %>% 
  group_by(admin) %>% 
  summarise(unique_species_n = n_distinct(species))
world_map_counts <- world_map_new %>% 
  left_join(overlap_counts,by = "admin")
world_map_counts <- st_transform(world_map_counts,4326)
ggplot(data = world_map_counts) +
  geom_sf(aes(fill = unique_species_n)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value = "grey90")+
  theme_minimal()+
  labs(fill = "Number of bee species present")

