# Create Hulls
# 27 January 2025
# Charles Thrift

# 0. Libraries
library(vroom) # read Dorey data
library(dplyr) # organize
library(sf) # spatial data
library(rnaturalearth) # make land cover spatial object


# 1. Read data in
data <- vroom("../data/ct_dorey_bees.csv") # read all Dorey data (6.7 M records)
df <- data[,c(2:8,22:23,28,44:46)] # filter columns
target_sp <- as.data.frame(read.csv("../data/species_list_353.csv"))

# 2. Match data together, make spatial object
list <- inner_join(df,target_sp,by="species")
summary <- list %>% 
  group_by(species) %>% 
  summarise(count=n())
mean(summary$count)
median(summary$count)
max(summary$count)
min(summary$count)
target_sp_all <- target[,c("species","decimalLatitude","decimalLongitude")]
target.sf <- target_sp_all %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# 3. Create convex hulls by species
hulls <- data.sf %>% 
  group_by(species) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  st_convex_hull()
polygons <- target_hulls[st_geometry_type(target_hulls)=="POLYGON",]


# 4. Exclude oceans from hulls
land <- ne_download(scale = "small",  # create a "land" object
                    type = "land", 
                    category = "physical", 
                    returnclass = "sf")
polygon_sp_crs <- st_transform(polygon_sp, 3857) # set crs
land_crs <- st_transform(land, 3857) # set crs
st_crs(land_crs) == st_crs(polygon_sp_crs) # double check == true
clipped_polygons_sp <- st_intersection(polygon_sp_crs, land_crs) #only keep intersection


# 5. Extract area for each species
clipped_polygons$area <- st_area(clipped_polygons)
clipped_polygons$area <- as.numeric(clipped_polygons$area)
sum_clipped_area <- clipped_polygons %>% 
  group_by(species) %>% 
  summarise(total_area = sum(area))

sum_clipped_area_dropGeom <- st_drop_geometry(sum_clipped_area)


# 6. Write data (list of area, and spatial object with hulls)
write.csv(sum_clipped_area_dropGeom, "chulls_area_clipped.csv")
st_write(clipped_polygons, "clipped_hulls_jun25.shp")



