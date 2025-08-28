# analyses
# 15 August 2025
# Charles Thrift

# 0. Libraries
library(tidyverse)
library(ggeffects)
library(emmeans)
library(sf)
library(rnaturalearth)

# 1. Read Data
data <- read_csv("../data/data_clean_dietFamily.csv")
genusdata <- data
rare_data <- read_csv("../../pollen_data/family_rarified.csv")
rare_data <- rare_data[,c(4,5)]
data <- left_join(data,rare_data,by="species")
data <- data %>% drop_na(Rare10)
rare_genusdata <- read_csv("../../pollen_data/genus_rarified.csv")
rare_genusdata <- rare_genusdata[,c(1,3)]
genusdata <- left_join(genusdata,rare_genusdata,by="species")
genusdata <- genusdata %>% drop_na(Rare10)
genusdata <- genusdata %>% filter(beeFamily != "Megachilidae")
spatial <- read_sf("../data/clipped_hulls_dietFamily_july25/clipped_hulls_dietFamily_july25.shp")
data$beeFamily <- as.factor(data$beeFamily)
genusdata$beeFamily <- as.factor(genusdata$beeFamily)
data$FamilyCount <- NA
data$FamilyCount[data$beeFamily == "Andrenidae"] <- "Andrenidae (n = 168)"
data$FamilyCount[data$beeFamily == "Apidae"] <- "Apidae (n = 108)"
data$FamilyCount[data$beeFamily == "Colletidae"] <- "Colletidae (n = 83)"
data$FamilyCount[data$beeFamily == "Halictidae"] <- "Halictidae (n = 86)"
data$FamilyCount[data$beeFamily == "Megachilidae"] <- "Megachilidae (n = 150)"
data$FamilyCount[data$beeFamily == "Melittidae"] <- "Melittidae (n = 18)"
data$FamilyCount <- as.factor(data$FamilyCount)
genusdata$FamilyCount <- NA



# 2. Log transform area and diet variables
data$log_area <- log(data$area)
data$log_diet <- log(data$Rare10)
genusdata$log_area <- log(genusdata$area)
genusdata$log_diet <- log(genusdata$Rare10)

# 3. Linear Models
mod <- lm(log_area~log_diet * beeFamily, data=data) # family and diet interactive effect
genus_mod <- lm(log_area~log_diet * beeFamily, data=genusdata)

DHARMa::simulateResiduals(mod, plot = TRUE)
DHARMa::simulateResiduals(genus_mod, plot = TRUE)

summary(mod)
summary(genus_mod)

family_colors <- c("Andrenidae" = "#DDCC77",
                   "Apidae" = "#88CCEE",
                   "Colletidae" = "#332288",
                   "Halictidae" = "#44AA99",
                   "Megachilidae" = "#AA4499",
                   "Melittidae" = "#fc5603")
genus_family_colors <- c("Andrenidae" = "#DDCC77",
                   "Apidae" = "#88CCEE",
                   "Halictidae" = "#44AA99")

ggpredict(mod,terms = c("log_diet", "beeFamily")) %>% 
  plot(show_data = TRUE, alpha = 0.5, color = family_colors) +
  facet_wrap(~group)

ggpredict(genus_mod,terms = c("log_diet", "beeFamily")) %>% 
  plot(show_data = TRUE, color = genus_family_colors) +
  facet_wrap(~group)

emtrends(mod, var = "log_diet", specs = "beeFamily")
emtrends(genus_mod, var = "log_diet", specs = "beeFamily")


### 3b. Linear models with subset of data: only those with larger sample size
test <- data
test25 <- test %>% filter(count_in_Dorey >25)
test100 <- test %>% filter(count_in_Dorey > 99)
test_mod25 <- lm(log_area~log_diet * beeFamily, data = test25)
test_mod100 <- lm(log_area~log_diet * beeFamily, data = test100)
summary(test_mod25)
summary(test_mod100)
ggpredict(test_mod25,terms = c("log_diet", "beeFamily")) %>% 
  plot(show_data = TRUE) +
  facet_wrap(~group)
ggpredict(test_mod100,terms = c("log_diet", "beeFamily")) %>% 
  plot(show_data = TRUE) +
  facet_wrap(~group)
emtrends(test_mod25, var = "log_diet", specs = "beeFamily")
emtrends(test_mod100, var = "log_diet", specs = "beeFamily")

df_predictions <- ggeffects::ggpredict(mod,
                terms = c("log_diet", "beeFamily")) %>% 
                rename(beeFamily = group)
df_predictions_1 <- ggeffects::ggpredict(mod,
                terms = c("log_diet")) %>% 
                rename(beeFamily = group)
df_predictions_genus <- ggeffects::ggpredict(genus_mod,
                terms = c("log_diet", "beeFamily")) %>% 
                rename(beeFamily = group)
df_predictions_genus_1 <- ggeffects::ggpredict(genus_mod,
                terms = c("log_diet")) %>% 
                rename(beeFamily = group)


ggplot(data = data, aes(x = log_diet, y = log_area)) +
  geom_point(aes(color = beeFamily)) +
  scale_color_manual(values = family_colors)+
   scale_fill_manual(values = family_colors)+
  geom_ribbon(data = df_predictions,
              aes(x = x,
                  y = predicted,
                  ymin = conf.low,
                  ymax = conf.high,
                  fill = beeFamily),
              alpha = 0.1) +
  geom_line(data = df_predictions,
            aes(x = x,
                y = predicted,
                color = beeFamily)) +
  theme_minimal()+
  facet_wrap(~beeFamily)

fig1a <- ggplot(data = data, aes(x = log_diet, y = log_area)) +
  geom_point(aes(color = beeFamily)) +
  scale_color_manual(values = family_colors)+
  geom_ribbon(data = df_predictions_1,
              aes(x = x,
                  y = predicted,
                  ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.3) +
  geom_line(data = df_predictions_1,
            aes(x = x,
                y = predicted)) +
  theme_bw()+
  labs(color = "Bee Family",
       x = "Diet",
       y = "Log Extent of Occurrence")+
  theme(text = element_text(size = 16))

new_labels <- c(
  "Andrenidae" = "Andrenidae (n = 188)",
  "Apidae" = "Apidae (n = 108)",
  "Colletidae" = "Colletidae (n = 83)",
  "Halictidae" = "Halictidae (n = 86)",
  "Megachilidae" = "Megachilidae (n = 150)",
  "Melittidae" = "Melittidae (n = 18)"
)

fig1b <- ggplot(data = data, aes(x = log_diet, y = log_area)) +
  geom_point(aes(color = beeFamily)) +
  scale_color_manual(values = family_colors)+
  scale_fill_manual(values = family_colors)+
  geom_ribbon(data = df_predictions,
              aes(x = x,
                  y = predicted,
                  ymin = conf.low,
                  ymax = conf.high,
                  fill = beeFamily),
              alpha = 0.3) +
  geom_line(data = df_predictions,
            aes(x = x,
                y = predicted,
                color = beeFamily)) +
  theme_bw()+
  facet_wrap(~ beeFamily,
             labeller = as_labeller(new_labels))+
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white"),
        text = element_text(size = 16))+
  labs(x = "Diet",
       y = "Log Extent of Occurrence")

cowplot::plot_grid(fig1a, fig1b, ncol = 1, nrow = 2)


fig2a <- ggplot(data = genusdata, aes(x = log_diet, y = log_area)) +
  geom_point(aes(color = beeFamily)) +
  scale_color_manual(values = genus_family_colors)+
  geom_ribbon(data = df_predictions_genus_1,
              aes(x = x,
                  y = predicted,
                  ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.3) +
  geom_line(data = df_predictions_genus_1,
            aes(x = x,
                y = predicted)) +
  theme_bw()+
  labs(color = "Bee Family",
       x = "Diet",
       y = "Log Extent of Occurrence")+
  theme(text = element_text(size = 16))

new_labels_genus <- c(
  "Andrenidae" = "Andrenidae (n = 91)",
  "Apidae" = "Apidae (n = 51)",
  "Halictidae" = "Halictidae (n = 32)")

fig2b <- ggplot(data = genusdata, aes(x = log_diet, y = log_area)) +
  geom_point(aes(color = beeFamily)) +
  scale_color_manual(values = family_colors)+
  scale_fill_manual(values = family_colors)+
  geom_ribbon(data = df_predictions_genus,
              aes(x = x,
                  y = predicted,
                  ymin = conf.low,
                  ymax = conf.high,
                  fill = beeFamily),
              alpha = 0.3) +
  geom_line(data = df_predictions_genus,
            aes(x = x,
                y = predicted,
                color = beeFamily)) +
  theme_bw()+
  facet_wrap(~ beeFamily,
             labeller = as_labeller(new_labels_genus))+
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white"),
        text = element_text(size = 16))+
  labs(x = "Diet",
       y = "Log Extent of Occurrence")

cowplot::plot_grid(fig2a, fig2b, ncol = 1, nrow = 2)




# 4. Correlation tests
cor.test(data$log_area, data$log_diet, method = "pearson")
cor.test(genusdata$log_area, genusdata$log_diet, method = "pearson")

correlations <- data %>%
  group_by(beeFamily) %>%
  group_map(~ {
    result <- cor.test(.x$log_area, .x$log_diet) %>% broom::tidy()
    result$beeFamily <- .y$beeFamily
    result
  }) %>%
  bind_rows()
genuscorrelations <- genusdata %>%
  group_by(beeFamily) %>%
  group_map(~ {
    result <- cor.test(.x$log_area, .x$log_diet) %>% broom::tidy()
    result$beeFamily <- .y$beeFamily
    result
  }) %>%
  bind_rows()

ggplot(correlations, aes(x = beeFamily, y = estimate, fill = beeFamily)) +
  geom_bar(stat = "identity") +
  labs(title = "Correlation Coefficients Across Bee Families",
       y = "Correlation Coefficient") +
  theme_minimal()
ggplot(genuscorrelations, aes(x = beeFamily, y = estimate, fill = beeFamily)) +
  geom_bar(stat = "identity") +
  labs(title = "Correlation Coefficients Across Bee Families",
       y = "Correlation Coefficient") +
  theme_minimal()



# 5. Scatterplots
family_colors <- c("Andrenidae" = "#DDCC77",
                   "Apidae" = "#88CCEE",
                   "Colletidae" = "#332288",
                   "Halictidae" = "#44AA99",
                   "Megachilidae" = "#AA4499",
                   "Melittidae" = "#fc5603")

ggplot(data = data, # contains individual lines per family
       aes(x = log_diet, y = log_area)) +
  geom_point(aes(color = beeFamily),size = 2) +
  geom_ribbon(data = df_predictions,
              aes(x = x,
                  y = predicted,
                  ymin = conf.low,
                  ymax = conf.high,
                  fill = beeFamily),
              alpha = 0.1) +
  geom_line(data = df_predictions,
            aes(x = x,
                y = predicted,
                color = beeFamily)) +
  scale_color_manual(values = family_colors)+
  scale_fill_manual(values = family_colors)+
  theme_bw()+
  labs(x = "Diet Breadth",
       y = "Range Size",
       "color" = "Bee Family")+
  theme(text = element_text(size = 18))

ggplot(data = data, # one model line for the whole thing?
       aes(x = log_diet, y = log_area)) +
  geom_point(aes(color = beeFamily),size = 2) +
  geom_ribbon(data = df_predictions1,
              aes(x = x,
                  y = predicted,
                  ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.35) +
  geom_line(data = df_predictions1,
            aes(x = x,
                y = predicted)) +
  scale_color_manual(values = family_colors)+
  theme_bw()+
  labs(x = "Diet Breadth",
       y = "Range Size",
       "color" = "Bee Family")+
  theme(text = element_text(size = 18))

plot_and <- ggplot(data = fam_and_df, aes(x = log_diet, y = log_area)) +
  geom_point(aes(color = beeFamily),size = 2) +
  geom_ribbon(data = df_predictions_and,
              aes(x = x,y = predicted,ymin = conf.low,ymax = conf.high),
              alpha = 0.35) +
  geom_line(data = df_predictions_and,aes(x = x,y = predicted)) +
  scale_color_manual(values = family_colors)+
  theme_bw()+
  labs(x = "Diet Breadth",
       y = "Range Size")+
  theme(legend.position = "none")

plot_api <- ggplot(data = fam_api_df, aes(x = log_diet, y = log_area)) +
  geom_point(aes(color = beeFamily),size = 2) +
  geom_ribbon(data = df_predictions_api,
              aes(x = x,y = predicted,ymin = conf.low,ymax = conf.high),
              alpha = 0.35) +
  geom_line(data = df_predictions_api,aes(x = x,y = predicted)) +
  scale_color_manual(values = family_colors)+
  theme_bw()+
  labs(x = "Diet Breadth",
       y = "Range Size")+
  theme(legend.position = "none")

plot_col <- ggplot(data = fam_col_df, aes(x = log_diet, y = log_area)) +
  geom_point(aes(color = beeFamily),size = 2) +
  geom_ribbon(data = df_predictions_col,
              aes(x = x,y = predicted,ymin = conf.low,ymax = conf.high),
              alpha = 0.35) +
  geom_line(data = df_predictions_col,aes(x = x,y = predicted)) +
  scale_color_manual(values = family_colors)+
  theme_bw()+
  labs(x = "Diet Breadth",
       y = "Range Size")+
  theme(legend.position = "none")

plot_hal <- ggplot(data = fam_hal_df, aes(x = log_diet, y = log_area)) +
  geom_point(aes(color = beeFamily),size = 2) +
  geom_ribbon(data = df_predictions_hal,
              aes(x = x,y = predicted,ymin = conf.low,ymax = conf.high),
              alpha = 0.35) +
  geom_line(data = df_predictions_hal,aes(x = x,y = predicted)) +
  scale_color_manual(values = family_colors)+
  theme_bw()+
  labs(x = "Diet Breadth",
       y = "Range Size")+
  theme(legend.position = "none")

plot_meg <- ggplot(data = fam_meg_df, aes(x = log_diet, y = log_area)) +
  geom_point(aes(color = beeFamily),size = 2) +
  geom_ribbon(data = df_predictions_meg,
              aes(x = x,y = predicted,ymin = conf.low,ymax = conf.high),
              alpha = 0.35) +
  geom_line(data = df_predictions_meg,aes(x = x,y = predicted)) +
  scale_color_manual(values = family_colors)+
  theme_bw()+
  labs(x = "Diet Breadth",
       y = "Range Size")+
  theme(legend.position = "none")

plot_mel <- ggplot(data = fam_mel_df, aes(x = log_diet, y = log_area)) +
  geom_point(aes(color = beeFamily),size = 2) +
  geom_ribbon(data = df_predictions_mel,
              aes(x = x,y = predicted,ymin = conf.low,ymax = conf.high),
              alpha = 0.35) +
  geom_line(data = df_predictions_mel,aes(x = x,y = predicted)) +
  scale_color_manual(values = family_colors)+
  theme_bw()+
  labs(x = "Diet Breadth",
       y = "Range Size")+
  theme(legend.position = "none")

plot_and
plot_api
plot_col
plot_hal
plot_meg
plot_mel

figure <- cowplot::plot_grid(plot_and, plot_api, plot_col,
                             plot_hal, plot_meg, plot_mel,
                             #  labels = c("Andrenidae", "Apidae", "Colletidae",
                             #            "Halictidae","Megachilidae","Melittidae"),
                             ncol = 3, nrow = 2)
figure

diet_colors <- c("Specialist" = "#88CCEE", 
                 "Generalist" = "#DDCC77")
data$bee_family_n <- NA
data$bee_family_n[data$beeFamily == "Andrenidae"] <- "Andrenidae (n = 188)"
data$bee_family_n[data$beeFamily == "Apidae"] <- "Apidae (n = 108)"
data$bee_family_n[data$beeFamily == "Colletidae"] <- "Colletidae (n = 83)" #CHANGE FROM 84
data$bee_family_n[data$beeFamily == "Halictidae"] <- "Halictidae (n = 86)"
data$bee_family_n[data$beeFamily == "Megachilidae"] <- "Megachilidae (n = 150)"
data$bee_family_n[data$beeFamily == "Melittidae"] <- "Melittidae (n = 18)"
data$diet_binary_family <- NA
data$diet_binary_family[data$Rare10 == 1] <- "Family Specialist"
data$diet_binary_family[data$Rare10 > 1] <- "Family Generalist"
df <- data
data$diet_binary_family_simple <- NA
data$diet_binary_family_simple[data$Rare10 == 1]<- "Specialist"
data$diet_binary_family_simple[data$Rare10 > 1]<- "Generalist"


ggplot(data)+
  geom_point(aes(x = log_diet, y = log_area,color = beeFamily),
             alpha = 1, size = 3)+
  # geom_jitter(aes(x = log_diet, y = log_area,color = beeFamily),
  #             alpha = 1, size = 3,width=0.1)+
  scale_color_manual(values = family_colors)+
  geom_smooth(aes(x = log_diet, y = log_area), 
              method = "lm",color = "black")+
  theme_bw()+
  labs(x = "Diet Breadth",
       y = "Range Size",
       "color" = "Bee Family")+
  theme(text = element_text(size = 18))

ggplot(data)+ ### (*Fig 2.*)
  geom_jitter(aes(x = log_diet, y = log_area,color = beeFamily), 
              alpha = 1, size = 2,width=0.1)+ 
  scale_color_manual(values = family_colors)+
  geom_smooth(aes(x = log_diet, y = log_area), 
              method = "lm",color = "black")+
  theme_bw()+
  labs(x = "Diet Breadth",
       y = "Range Size",
       "color" = "Bee Family")+
  theme(text = element_text(size = 18))+
  facet_wrap(~bee_family_n,
             scales = "free_y",
             ncol = 3)

ggplot(data)+ ### (*Supp Fig 2.*)
  geom_jitter(aes(x = log_diet, y = log_area,color = beeFamily), 
              alpha = 1, size = 3,width=0.1)+ 
  scale_color_manual(values = family_colors)+
  geom_smooth(aes(x = log_diet, y = log_area), 
              method = "lm",color = "black")+
  theme_bw()+
  labs(x = "Diet Breadth (Log Plant Families)",
       y = "Range Size",
       "color" = "Bee Family")+
  theme(text = element_text(size = 18),
        strip.background = element_rect(fill = "darkblue"),
        strip.text = element_text(color = "white"))+
  facet_wrap(~bee_family_n, ncol = 3)


# 5. Density Plot Specialist/Generalist
# Note for this fig: add a version that shows area (non log transformed)
# and, add bar plot coding at the base to show occurrences, and add median
# lines vertical (dashed), direct label special/general
ggplot(data = data, ### (*Fig 3*)
       aes(x = log_area, fill = diet_binary_family_simple))+
  geom_density(alpha = 0.8, color = "black")+
  geom_rug(aes(color = diet_binary_family_simple))+
  scale_fill_manual(values = diet_colors)+
  scale_color_manual(values = diet_colors)+
  theme_minimal()+
  labs(x = "Range Size",
       y = "Frequency",
       fill = "Diet Breadth")+
  theme(text = element_text(size = 18))+
  guides(color = "none")+
  #  geom_vline(aes(xintercept = 27.72187),color = "#88CCEE",linetype = "dashed")+ # mean
  #  geom_vline(aes(xintercept = 29.31176),color = "#DDCC77",linetype = "dashed")+ # mean
  scale_x_continuous(limits = c(12,35))

ggplot(data = data, ### (*Supp Fig 3*) ???
       aes(x = area, fill = diet_binary_family_simple))+
  geom_density(alpha = 0.8, color = "black")+
  geom_rug(aes(color = diet_binary_family_simple))+
  scale_fill_manual(values = diet_colors)+
  scale_color_manual(values = diet_colors)+
  theme_minimal()+
  labs(x = "Range Size",
       y = "Frequency",
       fill = "Diet Breadth")+
  theme(text = element_text(size = 18))+
  guides(color = "none")+
  scale_x_continuous(limits= c(0,1e14))

ggplot(data = data, aes(diet_binary_family_simple, y = area))+
  geom_boxplot()

# non-parametric comparison of two medians
wilcox.test(area ~ diet_binary_family_simple, data = data)

t.test(log_area ~ diet_binary_family, data = data) # t test
rstatix::cohens_d(log_area~diet_binary_family,data=data,var.equal = F) # cohen's d
sd <- data %>%  # get standard deviation and summary stats
  group_by(diet_binary_family) %>% 
  summarise(mean = mean(log_area), sd = sd(log_area), n = n(),
            median = median(log_area))



# 6. Choropleth Map
species_combined <- spatial %>%
  group_by(species) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf()
spatial_sp <- species_combined #%>% filter(species %in% data$bee_species)
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
  geom_sf(data = spatial_sp, fill = "darkblue",color = NA,alpha = 0.01) +
  theme_void()+
  labs(x = "Longitude", 
       y = "Latitude")+
  theme(text = element_text(size = 18),
        axis.text.y = element_text())+
  scale_y_continuous(breaks = c(-50,0,50),
                     labels = c("A","B","C"))+
  scale_x_continuous(breaks = c(-120,-60,0,60,120))

ggplot()+
  geom_sf(data = spatial_sp, fill = "darkblue",color = NA,alpha = 0.1) +
  theme_void()+
  labs(x = "Longitude", 
       y = "Latitude")+
  theme(text = element_text(size = 18))



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


ggplot(data = world_map_counts) +
  geom_sf(aes(fill = unique_species_n)) +
  scale_fill_viridis_c(option = "plasma",trans = "sqrt", na.value = "grey90",
                       breaks = c(1,25,100,200,350))+
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) + 
  theme_minimal() +
  labs(x = "Longitude", 
       y = "Latitude", 
       fill = "Bee species") +
  theme(text = element_text(size = 18))

hist(country_count_list$unique_species_n)

country_count_list <- intersections %>% 
  st_drop_geometry() %>% 
  group_by(admin) %>% 
  summarise(unique_species_n = n_distinct(species),
            list_species = toString(unique(species)))
colnames(country_count_list)[1] <- "Country"
