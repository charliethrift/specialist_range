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

write.csv(summary, "counts_for633_species.csv")
mean(summary$count)
median(summary$count)
max(summary$count)
min(summary$count)
hist(summary$count)


## rounding lat/long for plotting Fig 1
unq_pts <- list %>% distinct(family,scientificName,decimalLatitude,decimalLongitude)
unq_pts_round <- unq_pts %>% mutate(across(c(decimalLatitude,decimalLongitude), round, digits = 2))
unq_pts_round <- unq_pts_round %>% distinct(family,scientificName,decimalLatitude,decimalLongitude)
rando <- unq_pts_round %>% slice_sample(n = 100000)
# if not preserving family or species
unq_pts_nosp <- target %>% distinct(decimalLatitude,decimalLongitude)


library(ggplot2)

world <- map_data("world")

ggplot(unq_pts_round, aes(decimalLongitude,decimalLatitude))+
  geom_polygon(data = world, aes(x=long,y=lat,group=group),fill="gray",alpha=0.3)+
  geom_hex(bins=100)+
  scale_fill_viridis_b(trans="log",
                       breaks=c(10,100,1000,10000))+
  facet_wrap(~family)

ggplot(unq_pts_round, aes(decimalLongitude,decimalLatitude))+
  geom_polygon(data = world, aes(x=long,y=lat,group=group),fill="gray",alpha=0.5)+
  geom_hex(bins=100)+
  scale_fill_viridis_b(option="plasma",
                       breaks=c(10,100,1000,10000,100000))+
  scale_y_continuous(breaks = c(-40,0,40),
                     labels = c("-40\u00b0","0\u00b0","40\u00b0"))+
  scale_x_continuous(breaks = c(-120,-60,0,60,120),
                   labels = c("-120\u00b0","-60\u00b0","0\u00b0","60\u00b0","120\u00b0"))+
  labs(x="Longitude",y="Latitude",fill="Count")+
  theme_bw()

ggsave(width = 16,
       height = 10,
       units = "cm",
       dpi = 300,
       "figures/occurrence_pts.png")





















# make spatial object
library(sf)
target_sp_all <- target[,c("species","decimalLatitude","decimalLongitude")]
target.sf <- target_sp_all %>% st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

target_hulls <- target.sf %>% 
  group_by(species) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  st_convex_hull()

polygons <- target_hulls[st_geometry_type(target_hulls)=="POLYGON",] # results in 634 species

st_write(polygons, "hulls_july25.shp")
polygons <- read_sf("hulls_july25.shp")




# remove oceans
library(rnaturalearth)
land <- ne_download(scale = "small", type = "land", category = "physical", returnclass = "sf")

polygon_sp_crs <- st_transform(polygons, 3857)
land_crs <- st_transform(land, 3857)
st_crs(land_crs) == st_crs(polygon_sp_crs)


clipped_polygons_sp <- st_intersection(polygon_sp_crs, land_crs)

clipped_polygons_sp$area <- st_area(clipped_polygons_sp)
clipped_polygons_sp$area <- as.numeric(clipped_polygons_sp$area)
sum_clipped_area <- clipped_polygons_sp %>% 
  group_by(species) %>% 
  summarise(total_area = sum(area))

sum_clipped_area_dropGeom <- st_drop_geometry(sum_clipped_area)
df_dietFamily <- left_join(sum_clipped_area_dropGeom,summary,by="species")

write.csv(df_dietFamily, "chulls_area_clipped_dietFamily.csv")
st_write(clipped_polygons_sp, "clipped_hulls_dietFamily_july25.shp")

ggplot()+
  geom_sf(data = clipped_polygons_sp, fill = "blue", alpha = 0.01) +
  theme_minimal()
