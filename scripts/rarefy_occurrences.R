# rarefy the occurrence dataset
# 2025-11-19
# charles thrift

library(vroom)
library(data.table)
library(dplyr)
data <- vroom("ct_dorey_bees.csv")

df <- data[,c(2:8,22:23,28,44:46)]
rm(data) #remove dfs from environment
rm(df)


data <- read.csv("suppTable2.csv")
data$species <- data$bee_species



# match with just the 633 target species in the paper
list <- inner_join(df,data, by = "species")

vroom_write(list, "filtered_occurrence_data.csv", delim = ",")

summary <- list %>% 
  group_by(species) %>% 
  summarise(count = n())

mean(summary$count)
median(summary$count)
max(summary$count)
min(summary$count)
hist(summary$count)

summary15 <- subset(summary,count >= 15) # retains 560 species

mean(summary15$count)
median(summary15$count)
max(summary15$count)
min(summary15$count)
hist(summary15$count)

excluded <- anti_join(summary,summary15,by = "species") # list of 73 species excluded bc they had fewer than 15 records



# take a subset of points with just a random 15 per bee species
df_15 <- subset(summary, count >= 15)
list_15 <- inner_join(list,df_15,by="species")

# loop to build 10 different dataframes with random grabs (because of different seeds)
for (i in 1:10) {
  set.seed(200 + i)
  
  assign(
    paste0("df_small", i),
    list_15 %>%
      group_by(species) %>%
      slice_sample(n = 15) %>%
      ungroup() %>% 
      mutate("grp" = paste0("df_grp_", i))
  )
}


df_total <- rbind(df_small1,df_small2,df_small3,df_small4,df_small5,df_small6,df_small7,df_small8,df_small9,df_small10)


library(rnaturalearth)
land <- ne_download(scale = "small", type = "land", category = "physical", returnclass = "sf")
library(sf)
target_sp_all <- df_total[,c("species","decimalLatitude","decimalLongitude", "grp")]
target.sf <- target_sp_all %>% st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

target_hulls <- target.sf %>% 
  group_by(species,grp) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  st_convex_hull()

polygons <- target_hulls[st_geometry_type(target_hulls)=="POLYGON",]

polygon_sp_crs <- st_transform(polygons, 3857)
land_crs <- st_transform(land, 3857)
st_crs(land_crs) == st_crs(polygon_sp_crs)


clipped_polygons_sp <- st_intersection(polygon_sp_crs, land_crs)

clipped_polygons_sp$area <- st_area(clipped_polygons_sp)
clipped_polygons_sp$area <- as.numeric(clipped_polygons_sp$area)
sum_clipped_area <- clipped_polygons_sp %>% 
  group_by(species,grp) %>% 
  summarise(total_area = sum(area))

library(ggplot2)
ggplot(data = sum_clipped_area, aes(grp, total_area, color = species))+
  geom_point()+
  theme(legend.position = "none")


# model the relationship between range and diet (by adding in the diet information)
area_grps <- st_drop_geometry(sum_clipped_area)
df_test <- left_join(area_grps,data,by = "species")
df_test$log_area <- log(df_test$total_area)
df_test$log_diet <- log(df_test$diet_families_rareCount)

ggplot(data = df_test, aes(grp, log_area, color = species))+
  geom_point()+
  theme(legend.position = "none")

#cor.test(dft1$log_area, dft1$log_diet, method = "pearson")

dft1 <- subset(df_test, grp == "df_grp_1")
dft2 <- subset(df_test, grp == "df_grp_2")
dft3 <- subset(df_test, grp == "df_grp_3")
dft4 <- subset(df_test, grp == "df_grp_4")
dft5 <- subset(df_test, grp == "df_grp_5")
dft6 <- subset(df_test, grp == "df_grp_6")
dft7 <- subset(df_test, grp == "df_grp_7")
dft8 <- subset(df_test, grp == "df_grp_8")
dft9 <- subset(df_test, grp == "df_grp_9")
dft10 <- subset(df_test, grp == "df_grp_10")

cor.test(dft1$log_area, dft1$log_diet, method = "pearson")
cor.test(dft2$log_area, dft2$log_diet, method = "pearson")
cor.test(dft3$log_area, dft3$log_diet, method = "pearson")
cor.test(dft4$log_area, dft4$log_diet, method = "pearson")
cor.test(dft5$log_area, dft5$log_diet, method = "pearson")
cor.test(dft6$log_area, dft6$log_diet, method = "pearson")
cor.test(dft7$log_area, dft7$log_diet, method = "pearson")
cor.test(dft8$log_area, dft8$log_diet, method = "pearson")
cor.test(dft9$log_area, dft9$log_diet, method = "pearson")
cor.test(dft10$log_area, dft10$log_diet, method = "pearson")

cor.test(dft1$total_area, dft1$diet_families_rareCount, method = "pearson")
cor.test(dft2$total_area, dft2$diet_families_rareCount, method = "pearson")
cor.test(dft3$total_area, dft3$diet_families_rareCount, method = "pearson")
cor.test(dft4$total_area, dft4$diet_families_rareCount, method = "pearson")
cor.test(dft5$total_area, dft5$diet_families_rareCount, method = "pearson")
cor.test(dft6$total_area, dft6$diet_families_rareCount, method = "pearson")
cor.test(dft7$total_area, dft7$diet_families_rareCount, method = "pearson")
cor.test(dft8$total_area, dft8$diet_families_rareCount, method = "pearson")
cor.test(dft9$total_area, dft9$diet_families_rareCount, method = "pearson")
cor.test(dft10$total_area, dft10$diet_families_rareCount, method = "pearson")

summary(lm(log_area~log_diet+bee_family,data = dft1))
summary(lm(log_area~log_diet+bee_family,data = dft2))
summary(lm(log_area~log_diet+bee_family,data = dft3))
summary(lm(log_area~log_diet+bee_family,data = dft4))
summary(lm(log_area~log_diet+bee_family,data = dft5))
summary(lm(log_area~log_diet+bee_family,data = dft6))
summary(lm(log_area~log_diet+bee_family,data = dft7))
summary(lm(log_area~log_diet+bee_family,data = dft8))
summary(lm(log_area~log_diet+bee_family,data = dft9))
summary(lm(log_area~log_diet+bee_family,data = dft10))

ggplot(data = df_test, aes(log_diet,log_area))+
  geom_point()+
  facet_wrap(~grp)
ggplot(data = df_test, aes(total_area,eoo_size_m2))+
  geom_point()+
  facet_wrap(~grp)
#glm
install.packages("glmmTMB")
library(glmmTMB)
mod4 <- glmmTMB(total_area ~ diet_families_rareCount + bee_family, data = dft1, family = Gamma(link = "log"))
summary(mod4)

# plot hulls
ggplot()+
  geom_sf(data = clipped_polygons_sp, fill = "blue", alpha = 0.01) +
  theme_minimal()+
  facet_wrap(~grp)