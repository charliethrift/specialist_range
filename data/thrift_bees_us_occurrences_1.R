library(vroom)
library(data.table)
library(dplyr)
data <- vroom("ct_dorey_bees.csv")

df <- data[,c(2:8,22:23,28,44:46)]
rm(data) #remove dfs from environment
rm(df)


# match with just the 353 target species in the paper
target_sp353 <- read.csv("species_list_19jun25.csv")
target_sp353 <- as.data.frame(target_sp353[,c(3)])
colnames(target_sp353)[1]<-"species"

list <- inner_join(df,target_sp353, by = "species")

summary <- list %>% 
  group_by(species) %>% 
  summarise(count = n())

write.csv(summary, "summary_stats_for353_target_sp.csv")
mean(summary$count)
median(summary$count)
max(summary$count)
hist(summary$count)

summary2 <- summary %>% 
  group_by(count) %>% 
  summarise(freq = n())















#write full dataset as different families
and <- subset(df, family == "Andrenidae")
api <- subset(df, family == "Apidae")
col <- subset(df, family == "Colletidae")
hal <- subset(df, family == "Halictidae")
meg <- subset(df, family == "Megachilidae")
mel <- subset(df, family == "Melittidae")

rm(df) #remove dfs from environment

write.csv(and,"all_andrenidae.csv")
write.csv(api,"all_apidae.csv")
write.csv(col,"all_colletidae.csv")
write.csv(hal,"all_halictidae.csv")
write.csv(meg,"all_megichilidae.csv")
write.csv(mel,"all_melittidae.csv")

target_sp_new <- read.csv("target_wood_sp_list.csv")
target_sp_new <- as.data.frame(target_sp_new[,3])
colnames(target_sp_new)[1]<-"species"

# write the unique Dorey species list
full_dorey_sp_list <- as.data.frame(unique(df$species))
colnames(full_dorey_sp_list)[1]<-"species"
write.csv(full_dorey_sp_list, "species_list_from_Dorey.csv")

# try to match the whole dorey dataset with our targeted wood species
target <- inner_join(df,target_sp_new, by = "species")

try_match <- inner_join(full_dorey_sp_list,target_sp_new,by = "species")
try_missing <- anti_join(target_sp_new,full_dorey_sp_list, by = "species")
write.csv(try_missing, "species_in_wood_not_dorey.csv")

# match the target species with the occurrences to extract those species

and_target <- inner_join(and, target_sp_new, by = "species")
api_target <- inner_join(api, target_sp_new, by = "species")
col_target <- inner_join(col, target_sp_new, by = "species")
hal_target <- inner_join(hal, target_sp_new, by = "species")
meg_target <- inner_join(meg, target_sp_new, by = "species")
mel_target <- inner_join(mel, target_sp_new, by = "species")

# drop unnecessary columns to ease computation demand
and_target <- and_target[,c(2,7:13)]
api_target <- api_target[,c(2,7:13)]
col_target <- col_target[,c(2,7:13)]
hal_target <- hal_target[,c(2,7:13)]
meg_target <- meg_target[,c(2,7:13)]
mel_target <- mel_target[,c(2,7:13)]

rm(and,api,col,hal,meg,mel) #remove dfs from environment

# create spatial objects out of these data frames
library(sf)

and_target_sp <- and_target[,c("species","decimalLatitude","decimalLongitude")] #just save the lat, long, and species
rm(and_target) # remove this large file just to save a little memory space
and.sf <- and_target_sp %>% st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) #convert to spatial

api_target_sp <- api_target[,c("species","decimalLatitude","decimalLongitude")]
rm(api_target)
api.sf <- api_target_sp %>% st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

col_target_sp <- col_target[,c("species","decimalLatitude","decimalLongitude")]
rm(col_target)
col.sf <- col_target_sp %>% st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

hal_target_sp <- hal_target[,c("species","decimalLatitude","decimalLongitude")] #just save the lat, long, and species
rm(hal_target)
hal.sf <- hal_target_sp %>% st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

meg_target_sp <- meg_target[,c("species","decimalLatitude","decimalLongitude")] #just save the lat, long, and species
rm(meg_target)
meg.sf <- meg_target_sp %>% st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

mel_target_sp <- mel_target[,c("species","decimalLatitude","decimalLongitude")] #just save the lat, long, and species
rm(mel_target)
mel.sf <- mel_target_sp %>% st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)


target_sp_all <- target[,c("species","decimalLatitude","decimalLongitude")]
target.sf <- target_sp_all %>% st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)


# remove other dfs from environment
rm(and_target_sp,api_target_sp,col_target_sp,hal_target_sp,meg_target_sp,mel_target_sp)

# build convex hulls around each point by species
#draw convex hulls around all occurrence points for each species
and_hulls <- and.sf %>% group_by(species) %>% 
  summarise(geometry = st_combine(geometry)) %>% st_convex_hull()
#rm(and.sf)

api_hulls <- api.sf %>%
  group_by(species) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull()
#rm(api.sf)

col_hulls <- col.sf %>%
  group_by(species) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull()
#rm(col.sf)

hal_hulls <- hal.sf %>%
  group_by(species) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull()
#rm(hal.sf)

meg_hulls <- meg.sf %>%
  group_by(species) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull()
#rm(meg.sf)

mel_hulls <- mel.sf %>%
  group_by(species) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull()
#rm(mel.sf)

target_hulls <- target.sf %>% 
  group_by(species) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  st_convex_hull()

polygons <- target_hulls[st_geometry_type(target_hulls)=="POLYGON",]
xylocopa <- subset(polygons, species == "Xylocopa virginica")

st_write(polygons, "hulls_jun25.shp")
polygons <- read_sf("hulls_jun25.shp")

head_target_sf <- target.sf[c(1:10000),]

# plot the hulls object

library(rnaturalearth)

world_map <- ne_countries(scale = "medium", returnclass = "sf")
afroeurasia <- ne_countries(continent = c("Europe","Asia","Africa"), scale = "medium", returnclass = "sf")
coasts <- ne_coastline(scale = "medium", returnclass = "sf")
coast_polygons <- st_union(st_make_valid(coasts)) %>% st_polygonize()
land <- ne_download(scale = "small", type = "land", category = "physical", returnclass = "sf")


head_clipped_polygons <- clipped_polygons[c(1:100),]
  
library(ggplot2)
ggplot()+
  geom_sf(data = land_crs, fill = "lightgray",color = "black") + 
  geom_sf(data = clipped_polygons, fill = "blue", alpha = 0.1) +
  theme_minimal()

ggplot()+
  geom_sf(data = coast_polygons, fill = "lightgray",color = "black") + 
  geom_sf(data = xylocopa, fill = "blue", alpha = 0.5) +
  theme_minimal()



st_crs(land) == st_crs(polygons)
polygons_crs <- st_transform(polygons, 3857)
land_crs <- st_transform(land, 3857)
st_crs(land_crs) == st_crs(polygons_crs)


clipped_polygons <- st_intersection(polygons_crs, land_crs)

xylocopa <- subset(clipped_polygons, species == "Xylocopa virginica")


#sf::sf_use_s2(FALSE)

clipped_polygons$area <- st_area(clipped_polygons)
clipped_polygons$area <- as.numeric(clipped_polygons$area)
sum_clipped_area <- clipped_polygons %>% 
  group_by(species) %>% 
  summarise(total_area = sum(area))

sum_clipped_area_dropGeom <- st_drop_geometry(sum_clipped_area)

write.csv(sum_clipped_area_dropGeom, "chulls_area_clipped.csv")
st_write(clipped_polygons, "clipped_hulls_jun25.shp")






########################
# Make a choropleth figure
#########################

polygons <- st_read("hulls_jun25.shp")

#subset the data to only retain the 353 bee species being analyzed
sp_list <- read.csv("species_list_19jun25.csv")

polygon_sp <- polygons %>% filter(species %in% sp_list$species)

# clip these polygons to exclude water
polygon_sp_crs <- st_transform(polygon_sp, 3857)
land_crs <- st_transform(land, 3857)
st_crs(land_crs) == st_crs(polygon_sp_crs)


clipped_polygons_sp <- st_intersection(polygon_sp_crs, land_crs)

ggplot()+
  geom_sf(data = clipped_polygons_sp, aes(fill = geometry))+
  scale_fill_viridis_c()

world_map <- ne_countries(scale = "medium", returnclass = "sf")
ggplot()+
  geom_sf(data = world_map, fill = "lightgray",color = "black") + 
  geom_sf(data = clipped_polygons_sp, fill = "blue", alpha = 0.1) +
  theme_minimal()

st_crs(world_map)
st_crs(clipped_polygons_sp)

world_map <- st_transform(world_map, 3857)
clipped_polygons_sp <- st_transform(clipped_polygons_sp, 4326)
clipped_polygons_sp <- st_transform(clipped_polygons_sp, 3857)


st_crs(world_map) == st_crs(clipped_polygons_sp)
st_crs(world_map) == st_crs(polygon_sp_crs)


## perform spatial intersection to count overlaps, using geometry = 3857

overlap_counts <- st_intersects(world_map, clipped_polygons_sp) #NOTE: MAY WANT TO DO THIS WITH THE UNCLIPPED ONES
overlap_counts <- st_intersects(world_map, polygon_sp_crs) # with unclipped
world_map$overlap_count <- lengths(overlap_counts)

ggplot(data = world_map) +
  geom_sf(aes(fill = overlap_count)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value = "grey90")+
  theme_minimal()+
  labs(fill = "TITLE 1")

world_map_transform <- st_transform(world_map, 4326)

map_clipped <- ggplot(data = world_map_transform) +
  geom_sf(aes(fill = overlap_count)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value = "grey90")+
  theme_minimal()+
  labs(fill = "Number of bee species present")
map_unclipped <- ggplot(data = world_map_transform) +
  geom_sf(aes(fill = overlap_count)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value = "grey90")+
  theme_minimal()+
  labs(fill = "Number of bee species present")

world_map_transform <- world_map_transform %>%  relocate(overlap_count, .after = sovereignt)



## plotting
map_clipped
map_unclipped 

## Trying to unify the different clipped hulls by species
unify_sp_poly <- clipped_polygons_sp %>% group_by(species) %>% summarize()


world_map <- st_transform(world_map, 3857)
unify_sp_poly <- st_transform(unify_sp_poly, 3857)
overlap_counts <- st_intersects(world_map,unify_sp_poly)
world_map$overlap_count <- lengths(overlap_counts)

world_map <- st_transform(world_map, 4326)
world_map <- world_map %>%  relocate(overlap_count, .after = sovereignt)


ggplot(data = world_map)+
  geom_sf(aes(fill=overlap_count))+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value = "grey90")+
  theme_minimal()+
  labs(fill = "Number of bee species present")


# try to look at just country count and geometry
simple_world <- world_map[,c("admin", "overlap_count","geometry")]
french_guiana <- subset(simple_world,admin == "France")
french_guiana <- french_guiana %>% filter(geometry[c(6:10)])
france <- simple_world[[3]][[161]]
france_not_colonizing <- france[c(1:5)]
france_not_colonizing_simple <- data.frame(admin = "France",
                                           overlap_count=999,
                                           geometry = france_not_colonizing)
simple_world_editFrance <- rbind(simple_world_noFrance,france_not_colonizing_simple)
ggplot(data = simple_world)+
  geom_sf(aes(fill=overlap_count))+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value = "grey90")+
  theme_minimal()+
  labs(fill = "Number of bee species present")+
  geom_sf(data = french_guiana, aes(fill="red"))



world_map <- st_transform(world_map, 3857)
unify_sp_poly <- st_transform(unify_sp_poly, 3857)
intersections <- st_intersection(world_map,unify_sp_poly)
species_counts <- intersections %>% st_drop_geometry() %>% group_by(geounit) %>% summarise(unique_species = n_distinct(species))
world_geounits <- world_map %>% group_by(geounit) %>% summarise(geometry = st_union(geometry), .groups = "drop")
world_mapped <- left_join(world_geounits, species_counts, by = "geounit")
world_mapped <- st_transform(world_mapped, 4326)
ggplot(world_mapped)+
  geom_sf(aes(fill=unique_species))+
  scale_fill_viridis_c(option = "plasma",  na.value = "grey90")+
  theme_minimal()+
  labs(fill = "Number of bee species present")












## Try building alpha convex hulls instead
install.packages("spatstat.data")
library(alphahull)
mel_ahulls <- mel.sf %>% group_by(species) %>% 
  summarise(geometry = st_combine(geometry)) %>% alphahull::ahull()


#check species count by summing counts across all families
220+111+94+88+188+28 # equals 729
## this means that of the 860 species in Wood et al., only 729 were present in Dorey et al. 2023 data.
## need to figure out which species are not in Dorey. make a csv of this


# make one combined spatial object with hulls for each species
all_hulls <- rbind(and_hulls,api_hulls,col_hulls,hal_hulls,meg_hulls,mel_hulls)

species_present <- as.data.frame(all_hulls$species)
species_present$species <- species_present$`all_hulls$species`
species_absent <- anti_join(target_sp_new,species_present, by = "species")
species_absent <- species_absent[,c(1:2)]
write.csv(species_absent, "species_in_woodgen_not_dorey.csv")
write.csv(species_present, "species_list_present_new.csv")


summary(all_hulls$geometry) #summarize the hulls
## 674 polygons, 26 lines, and 29 points
## we will end up wanting to drop points and lines, because that means the hulls were constructed with less than 3 points
# write a csv of species with either lines or points instead of hulls
species_not_hull <- all_hulls
species_not_hull$geomType <- class(all_hulls$geometry)
species_not_hull$type_column <- sapply(species_not_hull$geometry, function(x) typeof(x))
species_not_hull <- subset(species_not_hull, type_column == "double")
species_not_hull <- species_not_hull[,1]
write.csv(species_not_hull, "species_too_few_records.csv")

# compute area based on the convex hulls
invalid_geometries <- st_is_valid(all_hulls)
all_hulls_invalid <- all_hulls
invalid_geom <- as.data.frame(invalid_geometries)
all_hulls_invalid <- cbind(all_hulls_invalid,invalid_geom)
all_hulls_invalid_only <- subset(all_hulls_invalid, invalid_geom == "FALSE")
all_hulls_valid_only <- subset(all_hulls_invalid, invalid_geom == "TRUE")

library(ggplot2)
world <- map_data("world")
world.sf <- world %>% st_as_sf(coords = c("long", "lat"), crs = 4326) #convert to spatial


ggplot() + 
  geom_sf(data = all_hulls_invalid_only,aes(fill=species))+
  geom_sf(data = world.sf, fill = "lightgray", color = "black")



all_hulls_valid_only$area <- st_area(all_hulls_valid_only)
all_hulls_valid_only$area <- as.numeric(all_hulls_valid_only$area)
all_hulls_valid_only <- all_hulls_valid_only[,c(1,4)]
all_hulls_valid_only <- st_drop_geometry(all_hulls_valid_only)
write.csv(all_hulls_valid_only, "area_species_list_june2025.csv")

## Now, try to plot (diet) by (area) for this subset of species

target_sp_valid <- target_sp

all_hulls_valid_only_df <- st_drop_geometry(all_hulls_valid_only)
target_sp_valid_join <- left_join(target_sp_valid,all_hulls_valid_only_df,by="species")

df <- target_sp_valid_join
df$area <- as.numeric(df$area)

ggplot(data = df)+
  geom_point(aes(x=area,y=n_families))

model <- lm(n_families ~ area, data = df)
summary(model)
cor(df$area, df$n_families)

# drop the areas with 0
df1 <- subset(df, area > 0)

ggplot(data = df1)+
  geom_smooth(aes(x=area,y=n_families))+
  geom_point(aes(x=area,y=n_families,color=Family))

ggplot(data = df1)+
  geom_point(aes(x=area,y=n_families))
ggplot(data = df1)+
  geom_smooth(aes(x=n_families,y=area))+
  geom_point(aes(x=n_families,y=area,color=Family))+
  theme_minimal()
ggplot(data = df1)+
  geom_point(aes(x=n_families,y=area))

# add a log scale
df1$log_area <- log(df1$area)
df1$log_pollen <- log(df1$n_families)

ggplot(data = df1)+
  geom_smooth(aes(x=log_pollen,y=log_area))+
  geom_jitter(aes(x=log_pollen,y=log_area,color=Family,alpha = 0.2,size = 1))+
  theme_minimal()


model <- lm(n_families ~ area, data = df1)
summary(model)
cor(df1$area, df1$n_families)

model <- lm(log_area ~ log_pollen, data = df1)
summary(model)
cor(df1$log_area, df1$log_pollen)


write.csv(df1, "diet_by_area_prelim_26march2025.csv")






data$country <- as.factor(data$country)
summary(data$country, maxsum = 1000000)
country <- as.data.frame(summary(data$country,maxsum = 1000000))
# "US" 
# "United States of America" 
# "United States" 
# "USA" 
# "UNITED STATES"
# "U.S.A" 
# "UNited States"
# "United StatesA"
# "United states"
# "CA"
# "canada"
# "Canada"
# "CANADA"
# "Mexico"
# "MEXICO"
# "México"
# "MX"

# subset from the total data to only united states bees
usa1 <- subset(data, country == "US")
usa2 <- subset(data, country == "United States of America")
usa3 <- subset(data, country == "United States")
usa4 <- subset(data, country == "USA")
usa5 <- subset(data, country == "UNITED STATES")
usa6 <- subset(data, country == "U.S.A.")
usa7 <- subset(data, country == "UNited States")
usa8 <- subset(data, country == "United StatesA")
usa9 <- subset(data, country == "United states")
can1 <- subset(data, country == "CA")
can2 <- subset(data, country == "canada")
can3 <- subset(data, country == "Canada")
can4 <- subset(data, country == "CANADA")
mex1 <- subset(data, country == "Mexico")
mex2 <- subset(data, country == "MEXICO")
mex3 <- subset(data, country == "México")
mex4 <- subset(data, country == "MX")


# combine the 9 different USAs into one df
usa <- rbind(usa1,usa2,usa3,usa4,usa5,usa6,usa7,usa8,usa9)
#combine the 4 different candadas into one df
can <- rbind(can1,can2,can3,can4)
#repeat with mexico
mex <- rbind(mex1,mex2,mex3,mex4)

#write a csv with these USA-only (or Canada or Mexico) data
write.csv(usa, "ct_dorey_usa.csv")
write.csv(can, "ct_dorey_can.csv")
write.csv(mex, "ct_dorey_mex.csv")

#combine all into one North America df
namerica <- rbind(can,usa,mex)

#write a csv with all North America data (2.82 M records)
write.csv(namerica, "ct_dorey_namerica.csv")
namerica <- vroom("ct_dorey_namerica.csv")
#plot(namerica$decimalLongitude,namerica$decimalLatitude)

# compute some summary statistics 
namerica$family <- as.factor(namerica$family)
summary(namerica$family)
and <- subset(namerica, family == "Andrenidae")
api <- subset(namerica, family == "Apidae")
col <- subset(namerica, family == "Colletidae")
hal <- subset(namerica, family == "Halictidae")
meg <- subset(namerica, family == "Megachilidae")
mel <- subset(namerica, family == "Melittidae")

# build species lists by family
and_sp <- and[,c(3:9)]
and_sp <- and_sp %>% distinct(species, .keep_all = T)
api_sp <- api[,c(3:9)]
api_sp <- api_sp %>% distinct(species, .keep_all = T)
col_sp <- col[,c(3:9)]
col_sp <- col_sp %>% distinct(species, .keep_all = T)
hal_sp <- hal[,c(3:9)]
hal_sp <- hal_sp %>% distinct(species, .keep_all = T)
meg_sp <- meg[,c(3:9)]
meg_sp <- meg_sp %>% distinct(species, .keep_all = T)
mel_sp <- mel[,c(3:9)]
mel_sp <- mel_sp %>% distinct(species, .keep_all = T)

# build species lists by family with SCIENTIFICNAME COLUMN INSTEAD
and_sp <- and[,c(3:9)]
and_sp <- and_sp %>% distinct(scientificName, .keep_all = T)
api_sp <- api[,c(3:9)]
api_sp <- api_sp %>% distinct(scientificName, .keep_all = T)
col_sp <- col[,c(3:9)]
col_sp <- col_sp %>% distinct(scientificName, .keep_all = T)
hal_sp <- hal[,c(3:9)]
hal_sp <- hal_sp %>% distinct(scientificName, .keep_all = T)
meg_sp <- meg[,c(3:9)]
meg_sp <- meg_sp %>% distinct(scientificName, .keep_all = T)
mel_sp <- mel[,c(3:9)]
mel_sp <- mel_sp %>% distinct(scientificName, .keep_all = T)

# write a csv with species list
sp_list <- rbind(and_sp,api_sp,col_sp,hal_sp,meg_sp,mel_sp)
write.csv(sp_list,"ct_sp_list_namerica.csv")
