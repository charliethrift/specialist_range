##########################################################################-
##### Genus-level and family-level comparisons
##### 2025-09-10
##########################################################################-

# This script requires you to run the cleaning/analysis steps in the
# `analyses_2025_august.R` script at least down to generating the models.

# a. wrangling ------------------------------------------------------------

# creating a new data frame to only include species that appear in genusdata
subset_data <- data |> 
  filter(species %in% c(unique(genusdata$species)))

# double checking
pull(genusdata, species) == pull(subset_data, species)
# all TRUE means that the species in each dataset match up

# b. fitting models -------------------------------------------------------

# original model with genus-level data
genus_mod <- lm(log_area ~ log_diet * beeFamily, data = genusdata)

# new model with subset family-level data
subset_mod <- lm(log_area ~ log_diet * beeFamily, data = subset_data)

# model diagnostics
DHARMa::simulateResiduals(subset_mod, plot = TRUE)
# residuals look fine

summary(subset_mod)
# "adjusted" R2 = 0.34
# adjusted R2 for genus_mod = 0.35 (which is pretty similar)


# c. plot predictions -----------------------------------------------------

# predictions look very similar
ggeffect(subset_mod,
         terms = c("log_diet", "beeFamily")) |> 
  plot(show_data = TRUE) +
  facet_wrap(~group) +
  scale_x_continuous(limits = c(0, 3.5)) +
  theme(legend.position = "none") +
  labs(title = "Family-level subset")

ggeffect(genus_mod,
         terms = c("log_diet", "beeFamily")) |> 
  plot(show_data = TRUE) +
  facet_wrap(~group) +
  scale_x_continuous(limits = c(0, 3.5)) +
  theme(legend.position = "none") +
  labs(title = "Genus-level")

# slopes look very similar
emtrends(genus_mod, var = "log_diet", specs = "beeFamily")
emtrends(subset_mod, var = "log_diet", specs = "beeFamily")
