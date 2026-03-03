# model rarefied occurrences
# 3 December 2025
# Charles Thrift

####################################################
# 0. Load Packages
####################################################
library(tidyverse)
library(ggeffects)
library(emmeans)

####################################################
# 1. Load Data
####################################################
df_eoo <- read_csv("../data/eoo_rarefied_100.csv") # should have 100 diff. versions of EOO per bee species, with same diet

data <- read_csv("../data/data_2025_09.csv")
data <- data %>% dplyr::select(diet_families_rareCount,bee_species,bee_family)
data <- data %>% mutate(log_diet = log(diet_families_rareCount),
                        species = bee_species)

# join the diet info with the newly modeled range predictions
df <- left_join(df_eoo, data, by = "species")
df <- df %>% mutate(log_area = log(total_area),
                    eoo_size_km2 = total_area/1e6,
                    diet_numeric = diet_families_rareCount)

####################################################
# 2. Run Models and Plot
####################################################
# 1 split data
df_list <- split(df, df$grp)

# 2 fit models
model_list <- lapply(df_list, function(d) {
  glmmTMB(
    eoo_size_km2 ~ diet_numeric + bee_family,
    data = d,
    family = Gamma(link = "log")
  )
})

# 3 extract predictions (convert to data frame!)
pred_list <- lapply(seq_along(model_list), function(i) {
  
  pr <- ggpredict(model_list[[i]], terms = "diet_numeric")
  
  pr_df <- as.data.frame(pr) 
  pr_df$grp <- names(df_list)[i]
  
  pr_df
})

# 4 combine into one dataframe
df_predictions <- do.call(rbind, pred_list)

# 5 plot
ggplot(df_predictions,
       aes(x = x,
           y = predicted,
           group = grp)) +
  geom_line(alpha = 0.15, colour = "black") +
  theme_minimal()+
  labs(x = "Diet breadth",
       y = "Range size (km<sup>2</sup>)")+
  theme(legend.position = "none",
        axis.title.y = element_markdown())
# 6 save plot
ggsave(width = 16, 
       height = 12,
       units = "cm",
       dpi = 300,
       bg = "white",
       "../figures/supp_mod100.png")
