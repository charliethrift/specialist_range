##########################################################################-
##### Some examples of correlation plots
##### 2025-08-28
##########################################################################-

# This script requires you to run the cleaning/analysis steps in the
# `analyses_2025_august.R` script at least down to generating the correlations.
# I've tried to annotate new things as clearly as I can, but let me know if you
# have questions!!

# 1. wrangling ------------------------------------------------------------

correlations <- data %>%
  group_by(beeFamily) %>%
  group_map(~ {
    result <- cor.test(.x$log_area, .x$log_diet) %>% broom::tidy()
    result$beeFamily <- .y$beeFamily
    result
  }) %>%
  bind_rows() |> 
  # adding in a column called "type" to distinguish between family and
  # genus correlation
  mutate(type = "family") |> 
  # adding a new column with categorical p-value (significant or not)
  mutate(sig = case_when(
    p.value < 0.05 ~ "yes",
    TRUE ~ "no"
  ))

genuscorrelations <- genusdata %>%
  group_by(beeFamily) %>%
  group_map(~ {
    result <- cor.test(.x$log_area, .x$log_diet) %>% broom::tidy()
    result$beeFamily <- .y$beeFamily
    result
  }) %>%
  bind_rows() |> 
  # adding a new column with categorical p-value (significant or not)
  mutate(sig = case_when(
    p.value < 0.05 ~ "yes",
    TRUE ~ "no"
  )) |> 
  # adding in a column called "type" to distinguish between family and
  # genus correlation
  mutate(type = "genus") |> 
  # combine the data frame with the correlations df
  bind_rows(correlations) |> 
  # complete the different combinations of beeFamily and type
  # for example, there is now a new row for Colletidae with genus correlation,
  # and it is full of NAs
  # this is important for plotting down the line!
  complete(beeFamily, type) |> 
  # creating a new column with significance and type and setting factor order
  mutate(sig_type = paste0(sig, "_", type),
         sig_type = fct_relevel(as.factor(sig_type),
                                "yes_family", "no_family","yes_genus", "no_genus", "NA_genus")) |> 
  # making "yes" come first in factor order in the sig column
  mutate(sig = fct_relevel(as.factor(sig), "yes", "no"))


# 2. square tiles ---------------------------------------------------------

# one option for a correlation plot
ggplot(data = correlations,
       aes(x = method,
           y = beeFamily,
           fill = estimate)) +
  # tiles to represent correlations
  geom_tile(color = "#FFFFFF",
            linewidth = 2) +
  # text labels for correlations
  geom_text(aes(label = round(estimate, digits = 2))) +
  scale_fill_gradient2(
    low = "#F21A00",
    mid = "#FFFFFF",
    high = "#3B9AB2"
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  # to make everything appear in squares
  coord_fixed(ratio = 1) +
  labs(title = "Pearson correlation") +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.title.position = "plot")


# 3. dot and whisker plot for family correlations only --------------------

# just had to move this up to use the colors
family_colors <- c("Andrenidae" = "#DDCC77",
                   "Apidae" = "#88CCEE",
                   "Colletidae" = "#332288",
                   "Halictidae" = "#44AA99",
                   "Megachilidae" = "#AA4499",
                   "Melittidae" = "#fc5603")

# new correlation plot
ggplot(data = correlations, 
       aes(x = beeFamily,
           y = estimate,
           color = beeFamily
       )) +
  # horizontal dashed line
  geom_hline(yintercept = 0, 
             linetype = 3,
             color = "darkgrey") +
  # dot and whisker
  geom_pointrange(aes(ymin = conf.low,
                      ymax = conf.high,
                      lty = sig),
                  size = 1) +
  scale_color_manual(values = family_colors) +
  scale_linetype_manual(values = c("yes" = 1,
                                   "no" = 2)) +
  labs(x = "Bee family",
       y = "Pearson correlation and 95% CI") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank())


# 4. dot and whisker plot with family and genus correlations --------------

## a. creating a legend ---------------------------------------------------

legend <- tribble(
  ~x,       ~y,    ~sig_type,
  "legend",  5, "yes_family",
  "legend",  3,  "yes_genus",
  "legend",  4,  "no_family",
  "legend",  2,   "no_genus" 
) |> 
  ggplot(aes(x = x,
             y = y,
             ymin = y - 0.3,
             ymax = y + 0.3,
             shape = sig_type)) +
  geom_pointrange(fill = "#FFFFFF") +
  scale_shape_manual(values = c("yes_family" = 16, # closed circles
                                "yes_genus" = 15, # closed squares
                                "no_family" = 21, # open circles
                                "no_genus" = 22), # open squares
                     # new labels
                     # to be honest, I don't like the way the legend looks here
                     # I would probably do something different (and happy to help clean this up)
                     # but for now leaving the way it is to get your feedback
                     label = c("yes_family" = "Family",
                               "yes_genus" = "Genus",
                               "no_family" = "",
                               "no_genus" = "")) +
  annotate(geom = "label",
           x = 1.3, y = 4.5,
           label = "Plant families",
           size = 2.5) +
  annotate(geom = "label",
           x = 1.3, y = 2.5,
           label = "Plant genera",
           size = 2.5) +
  annotate(geom = "text",
           x = 1.15, y = 5.8,
           label = "Diet metric",
           size = 3) +
  # geom_segment(aes(x = 1.07,
  #                  y = 4,
  #                  yend = 5)) +
  # geom_segment(aes(x = 1.07,
  #                  y = 2,
  #                  yend = 3)) +
  theme_void() +
  theme(legend.position = "none") 

## b. making the figure ---------------------------------------------------

ggplot(data = genuscorrelations,
       aes(x = beeFamily,
           y = estimate,
           color = beeFamily)) +
  geom_hline(yintercept = 0, 
             linetype = 3,
             color = "darkgrey") +
  geom_pointrange(aes(shape = sig_type,
                      ymin = conf.low,
                      ymax = conf.high,
                      lty = sig),
                  position = position_dodge(width = 0.5),
                  size = 1,
                  linewidth = 1,
                  fill = "#FFFFFF") +
  scale_color_manual(values = family_colors,
                     guide = "none") +
  scale_shape_manual(values = c("yes_family" = 16, # closed circles
                                "yes_genus" = 15, # closed squares
                                "no_family" = 21, # open circles
                                "no_genus" = 22), # open squares
                     # new labels
                     label = c("yes_family" = "Family",
                               "yes_genus" = "Genus",
                               "no_family" = "",
                               "no_genus" = "")) +
  # taking out the legend for linetype
  scale_linetype_discrete(guide = "none") +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(x = "Bee family",
       y = "Pearson correlation and 95% CI",
       shape = "Type") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank()) +
  patchwork::inset_element(legend, 0.68, 0.68, 1, 0.98)

## c. saving the figure ---------------------------------------------------

ggsave(width = 16, 
       height = 10,
       units = "cm",
       dpi = 300,
       "genus-and-family-correlation.png")

# 4. dot and whisker plot with family and genus slopes --------------------

## a. wrangling -----------------------------------------------------------

family_slopes <- emtrends(mod, var = "log_diet", specs = "beeFamily") |> 
  as.data.frame() |> 
  mutate(type = "family")

all_slopes <- emtrends(genus_mod, var = "log_diet", specs = "beeFamily") |> 
  as.data.frame() |> 
  mutate(type = "genus") |> 
  bind_rows(family_slopes) |> 
  # complete the different combinations of beeFamily and type
  # for example, there is now a new row for Colletidae with genus correlation,
  # and it is full of NAs
  # this is important for plotting down the line!
  complete(beeFamily, type) |> 
  # adding a new column with categorical p-value (significant or not)
  mutate(sig = case_when(
    lower.CL > 0 ~ "yes",
    lower.CL <= 0 ~ "no"
    #between(0, lower.CL, upper.CL) ~ "no"
  )) |> 
  # creating a new column with significance and type and setting factor order
  mutate(sig_type = paste0(sig, "_", type),
         sig_type = fct_relevel(as.factor(sig_type),
                                "yes_family", "no_family","yes_genus", "no_genus", "NA_genus")) |> 
  # making "yes" come first in factor order in the sig column
  mutate(sig = fct_relevel(as.factor(sig), "yes", "no"))

## b. making the figure ---------------------------------------------------

ggplot(data = all_slopes,
       aes(x = beeFamily,
           y = log_diet.trend,
           color = beeFamily)) +
  geom_hline(yintercept = 0, 
             linetype = 3,
             color = "darkgrey") +
  geom_pointrange(aes(shape = sig_type,
                      ymin = lower.CL,
                      ymax = upper.CL,
                      lty = sig),
                  position = position_dodge(width = 0.5),
                  size = 1,
                  linewidth = 1,
                  fill = "#FFFFFF") +
  scale_color_manual(values = family_colors,
                     guide = "none") +
  scale_shape_manual(values = c("yes_family" = 16, # closed circles
                                "yes_genus" = 15, # closed squares
                                "no_family" = 21, # open circles
                                "no_genus" = 22), # open squares
                     # new labels
                     label = c("yes_family" = "Family",
                               "yes_genus" = "Genus",
                               "no_family" = "",
                               "no_genus" = "")) +
  # taking out the legend for linetype
  scale_linetype_discrete(guide = "none") +
  scale_y_continuous(limits = c(-3.5, 2.5)) +
  labs(x = "Bee family",
       y = "Slope estimate and 95% CI",
       shape = "Type") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank()) +
  patchwork::inset_element(legend, 0.68, 0.68, 1, 0.98)

## c. saving the figure ---------------------------------------------------

ggsave(width = 16, 
       height = 10,
       units = "cm",
       dpi = 300,
       "genus-and-family-slopes.png")

