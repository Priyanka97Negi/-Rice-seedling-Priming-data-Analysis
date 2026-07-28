###############################################################################
# Project      : Rice Seedling Priming Data Analysis
# Script       : 01_Violin_Boxplots.R
# Description  : Generate publication-quality violin and boxplots for
#                rice seedling traits under different priming treatments.
#
# Author       : Priyanka Negi
# Affiliation  : PMRF Fellow | Department of Agricultural Botany (Plant Physiology)
#                MPKV, Rahuri, Maharashtra, India
#                ICAR-NIASM, Baramati, Maharashtra, India
#
# Input File   : Data/Exp_6_45DAS_Datasheet.csv
# Output Folder: Output/ViolinPlots/
#
# R Version    : 4.3+
###############################################################################

##############################
# Load Required Packages
##############################

library(tidyverse)
library(ggrepel)

##############################
# Create Output Directory
##############################

if (!dir.exists("Output")) {
  dir.create("Output")
}

if (!dir.exists("Output/ViolinPlots")) {
  dir.create("Output/ViolinPlots")
}

##############################
# Import Dataset
##############################

phenotype_data <- read.csv(
  "Data/Exp_6_45DAS_Datasheet.csv",
  stringsAsFactors = FALSE
)

##############################
# Trait Columns
##############################

trait_columns <- c(
  "SL45DAS",
  "SFW45DAS",
  "SDW45DAS",
  "RL45DAS",
  "RFW45DAS",
  "RDW45DAS"
)

##############################
# Calculate Genotype Means
##############################

genotype_means <- phenotype_data %>%
  group_by(Genotype, Treatment) %>%
  summarise(
    across(
      all_of(trait_columns),
      mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

##############################
# Convert to Long Format
##############################

plot_data_long <- genotype_means %>%
  pivot_longer(
    cols = all_of(trait_columns),
    names_to = "Trait",
    values_to = "Value"
  )

##############################
# Identify Best Genotype
##############################

best_genotypes <- plot_data_long %>%
  group_by(Trait, Treatment) %>%
  slice_max(
    order_by = Value,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

##############################
# Maintain Treatment Order
##############################

plot_data_long$Treatment <- factor(
  plot_data_long$Treatment,
  levels = unique(plot_data_long$Treatment)
)

best_genotypes$Treatment <- factor(
  best_genotypes$Treatment,
  levels = levels(plot_data_long$Treatment)
)

##############################
# Generate Publication-Quality
# Violin + Boxplots
##############################

traits <- unique(plot_data_long$Trait)

for (trait in traits) {

  current_data <- plot_data_long %>%
    filter(Trait == trait)

  label_data <- best_genotypes %>%
    filter(Trait == trait)

  figure <- ggplot(
    current_data,
    aes(
      x = Treatment,
      y = Value,
      fill = Treatment
    )
  ) +

    geom_violin(
      trim = FALSE,
      alpha = 0.65,
      colour = "black"
    ) +

    geom_boxplot(
      width = 0.12,
      alpha = 0.80,
      outlier.shape = NA,
      colour = "black"
    ) +

    geom_jitter(
      width = 0.12,
      size = 2.2,
      alpha = 0.65
    ) +

    geom_point(
      data = label_data,
      colour = "red",
      size = 4
    ) +

    geom_text_repel(
      data = label_data,
      aes(label = Genotype),
      colour = "black",
      fontface = "bold",
      size = 4,
      box.padding = 0.6,
      point.padding = 0.4,
      segment.color = "grey50",
      max.overlaps = Inf
    ) +

    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.20))
    ) +

    labs(
      title = paste("Distribution of", trait),
      subtitle = "Rice Genotypes under Different Priming Treatments",
      x = "Priming Treatment",
      y = trait
    ) +

    theme_bw(base_size = 14) +

    theme(
      legend.position = "none",
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        hjust = 0.5
      ),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      panel.grid.minor = element_blank()
    )

  ggsave(
    filename = file.path(
      "Output",
      "ViolinPlots",
      paste0(trait, "_Violin_Boxplot.png")
    ),
    plot = figure,
    width = 8,
    height = 6,
    dpi = 600
  )
}

##############################
# Completion Message
##############################

cat("Violin and boxplots generated successfully.\n")
cat("Output directory:", normalizePath("Output/ViolinPlots"), "\n")

##############################
# Session Information
##############################

sessionInfo()
