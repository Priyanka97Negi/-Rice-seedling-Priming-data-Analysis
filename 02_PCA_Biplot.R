###############################################################################
# Project      : Rice Seedling Priming Data Analysis
# Script       : 02_PCA_Biplot.R
#
# Description  : Principal Component Analysis (PCA) of rice genotypes evaluated under different seed priming treatments.
#
# Author       : Priyanka Negi
# Affiliation  : PMRF Fellow | Department of Agricultural Botany (Plant Physiology)
#                MPKV, Rahuri, Maharashtra, India
#                ICAR-NIASM, Baramati, Maharashtra, India
#
# Input File : Data/Exp_6_45DAS_Datasheet.csv
#
# Output Folder: Output/PCA/PCA_Biplot_Rice_Genotypes.tiff
#
# R Version: 4.3+
###############################################################################

##############################
# Load Required Packages
##############################

library(tidyverse)
library(FactoMineR)
library(factoextra)

##############################
# Create Output Folder
##############################

if (!dir.exists("Output")) {
  dir.create("Output")
}

if (!dir.exists("Output/PCA")) {
  dir.create("Output/PCA")
}

##############################
# Import Dataset
##############################

phenotype_data <- read.csv(
  "Data/Exp_6_45DAS_Datasheet.csv",
  stringsAsFactors = FALSE
)

##############################
# Select Numerical Traits
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
  group_by(Genotype) %>%
  summarise(
    across(
      all_of(trait_columns),
      mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

##############################
# Prepare Data for PCA
##############################

rownames(genotype_means) <- genotype_means$Genotype

pca_input <- genotype_means %>%
  select(-Genotype)

##############################
# Perform Principal Component Analysis
##############################

pca_result <- PCA(
  pca_input,
  scale.unit = TRUE,
  graph = FALSE
)

##############################
# Generate PCA Biplot
##############################

pca_plot <- fviz_pca_biplot(

  pca_result,

  repel = TRUE,

  label = "all",

  col.ind = "blue",

  col.var = "red",

  pointshape = 19,

  pointsize = 2.5,

  arrowsize = 0.8

) +

geom_hline(
  yintercept = 0,
  linetype = "dashed",
  colour = "grey50"
) +

geom_vline(
  xintercept = 0,
  linetype = "dashed",
  colour = "grey50"
) +

labs(

  title = "Principal Component Analysis of Rice Genotypes",

  subtitle = "Based on seedling growth traits under different priming treatments",

  x = "Principal Component 1",

  y = "Principal Component 2"

) +

theme_minimal(base_size = 14) +

theme(

  plot.title = element_text(
    face = "bold",
    hjust = 0.5
  ),

  plot.subtitle = element_text(
    hjust = 0.5
  )

)

##############################
# Save Figure
##############################

ggsave(

  filename = file.path(
    "Output",
    "PCA",
    "PCA_Biplot_Rice_Genotypes.tiff"
  ),

  plot = pca_plot,

  width = 10,

  height = 8,

  dpi = 600,

  compression = "lzw"

)

##############################
# Completion Message
##############################

cat("PCA analysis completed successfully.\n")
cat("Figure saved in Output/PCA/\n")

##############################
# Session Information
##############################

sessionInfo()
