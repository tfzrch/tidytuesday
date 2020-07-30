# Packages ----
library(tidyverse)
library(here)
library(patchwork)
requireNamespace("tidytuesdayR")
requireNamespace("ggridges")
requireNamespace("magrittr")
requireNamespace("factoextra")
requireNamespace("janitor")

# ggplot theme ----
theme_antarctic <- function() {
  theme_classic() +
    theme(
      plot.background = element_rect(fill = "#ebf4fd", color = "#ebf4fd"),
      panel.background = element_rect(fill = "#ebf4fd"),
      text = element_text(colour = "#486C84", size = 11),
      legend.background = element_rect(fill = "#ebf4fd"),
      axis.text = element_text(colour = "#486C84"),
      axis.line = element_line(colour = "#486C84"),
      axis.ticks.x = element_line(colour = "#486C84"),
      axis.ticks.y = element_line(colour = "#486C84"),
      strip.background = element_rect(fill = NA, color = NA),
      strip.text = element_text(colour = "#486C84", size = 7, face = "bold"),
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 12, face = "italic")
    )
}

theme_set(theme_antarctic())

# TT data ---
latest_tt <- tidytuesdayR::tt_load("2020-07-28")

penguins <- latest_tt$penguins

# Colour Palettes ----
species_palette <- c("#ea7d23", "#f9e94e", "#577bfb")
sex_palette <- c("#D33F6A", "#E2E6BD")

# Plots list ----
plots <- list()

# Scaled and centred Metrics ----
penguins_scaled <- bind_cols(
  penguins %>% 
    select(species, island, sex),
  penguins %>% 
    select(bill_length_mm:flipper_length_mm) %>%
    scale(center = T) %>%
    magrittr::set_colnames(c("bill_length", "bill_depth", "flipper_length")) %>%
    as.data.frame() %>%
    as_tibble()) %>%
  drop_na()

# * PCA ----
penguins_scaled_pca <- prcomp(penguins_scaled[, 4:6]) %>%
  factoextra::get_pca_ind() %>%
  magrittr::extract2("coord") %>%
  as.data.frame() %>%
  janitor::clean_names()

penguins_scaled_pca <- bind_cols(penguins_scaled %>% select(species), penguins_scaled_pca)

# ** PCA Plot ----
plots[["penguins_scaled_pca"]] <- ggplot(penguins_scaled_pca) +
  aes(dim_1, dim_2, fill = species, col = species) +
  geom_point(shape = 21, col = "black", stroke = .5) +
  stat_ellipse(size = .5) +
  scale_fill_manual(values = species_palette) +
  scale_color_manual(values = species_palette) +
  labs(x = "PC1", y = "PC2", fill = "Species", col = "Species",
       title = "PCA of Scaled and Centred Metrics",
       subtitle = "Variables: bill depth and length, and flipper length")

# * Scaled species/sex distributions ----
penguins_scaled_tidy <- penguins_scaled %>%
  pivot_longer(matches("\\w+_"), names_to = "metric") %>%
  mutate(metric = str_replace(metric, "_", " ") %>% str_to_title())

penguins_scaled_mean <- penguins_scaled_tidy %>%
  group_by(metric, species, sex) %>%
  summarise(mean = mean(value, na.rm = T))

# ** Plot ----
plots[["scaled_metrics_species_sex"]] <- ggplot(penguins_scaled_tidy) +
  aes(x = value, y = species, fill = sex) +
  ggridges::geom_density_ridges(col = "black", size = .1, alpha = .8) +
  geom_point(data = penguins_scaled_mean, aes(x = mean), 
             inherit.aes = T, shape = 21, size = 2.5, stroke = .5) +
  scale_fill_manual(values = sex_palette) +
  labs(x = "Scaled Metric", y = "Species", fill = "Sex", col = "Sex",
       title = "Scaled and Centred Metrics\nby Species and Sex (Density)",
       subtitle = "Points show mean for each group") +
  facet_grid(metric~.)
plots$scaled_metrics_species_sex

# Body mass ----
body_mass_clean <- penguins %>% select(species, sex, body_mass_g) %>%
  drop_na()
body_mass_mean <- body_mass_clean %>%
  group_by(species, sex) %>%
  summarise(body_mass_g = mean(body_mass_g))

plots[["body_mass_species_sex"]] <- ggplot(body_mass_clean) +
  aes(x = body_mass_g, fill = sex, y = species) +
  ggridges::geom_density_ridges(alpha = .8, scale = 1, col = "black", size = .1) +
  geom_point(data = body_mass_mean, shape = 21, stroke = .5, size = 2.5) +
  scale_fill_manual(values = sex_palette) +
  labs(x = "Body Mass (g)", y = "Species", fill = "Sex",
       title = "Body Mass by Species and Sex\n(Density)",
       subtitle = "Points show mean for each group") +
  facet_grid()

plots$body_mass_species_sex

# Isotopes ----

# * Clean raw data ----
penguins_isotopes <- latest_tt$penguins_raw %>%
  select(Species, Sex, matches("Delta")) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  mutate(species = str_extract(species, "Adelie|Chinstrap|Gentoo"),
         sex = str_to_lower(sex)) %>%
  rename_with(.fn = function(x) {
    x %>%
      str_replace(., "delta_", "d") %>%
      str_remove_all("_") %>%
      str_remove_all("o")
  })

# * Plot ----
plots[["isotopes"]] <- ggplot(penguins_isotopes) +
  aes(d13c, d15n, fill = species, col = species) +
  geom_point(shape = 21, col = "black", stroke = .5) +
  stat_ellipse(size = .5) +
  scale_colour_manual(values=species_palette) +
  scale_fill_manual(values=species_palette) +
  labs(x = expression(delta^13*"C (\u2030)"),
       y = expression(delta^15*"N (\u2030)"),
       fill = "Species", colour = "Species",
       title = "Dietary Isotopes by Species") +
  annotate("segment", x = -23.75, xend = -23.75, y = 8, yend = 10,
           arrow = arrow(15, length = unit(3, "mm"), type = "closed"),
           color = "#486C84") +
  annotate("text", x = -23.6, y = 9, label = "Trophic Level", angle = -90,
           colour = "#486C84", fontface = "bold")


# Patchwork & Export ----
patches <- plots$penguins_scaled_pca/ plots$isotopes +
  plot_layout(guides = "collect") |
  plots$scaled_metrics_species_sex /
     plots$body_mass_species_sex +
  plot_layout(guides = "collect")

patches <- patches +
  plot_annotation(caption = "Source: Gorman, Horst, and Hill (2020) palmerpenguins: Palmer Archipelago (Antarctica) penguin data. R package version 0.1.0")


png(here("2020", "2020-07-28", "2020-07-28-palmer-penguins-01.png"),
         width = 10, height = 8, units = "in", res = 200)
patches
dev.off()
