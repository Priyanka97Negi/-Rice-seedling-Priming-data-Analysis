###############################################################################
# Project      : Rice Seedling Priming Data Analysis
# Script       : 03_Hierarchical_Clustering.R
#
# Description  : Performs hierarchical cluster analysis of rice genotypes based on seedling growth traits measured at 45 DAS under different priming treatments.
#
# Author       : Priyanka Negi
# Affiliation  : PMRF Fellow | Department of Agricultural Botany (Plant Physiology)
#                MPKV, Rahuri, Maharashtra, India
#                ICAR-NIASM, Baramati, Maharashtra, India
#
# Input File   : Data/Exp_6_45DAS_Datasheet.csv
#
# Output Folder : Output/Clustering/
#       ├── Hierarchical_Clustering_Rice_Genotypes.tiff
#       └── Cluster_Membership.csv

# R Version: 4.3+
###############################################################################

##############################
# Load Required Packages
##############################

library(tidyverse)

##############################
# Create Output Directory
##############################

if (!dir.exists("Output")) {
  dir.create("Output")
}

if (!dir.exists("Output/Clustering")) {
  dir.create("Output/Clustering")
}

##############################
# Import Dataset
##############################

phenotype_data <- read.csv(
  "Data/Exp_6_45DAS_Datasheet.csv",
  stringsAsFactors = FALSE
)

##############################
# Select Trait Columns
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
# Prepare Matrix for Clustering
##############################

genotype_names <- genotype_means$Genotype

cluster_matrix <- genotype_means %>%
  select(-Genotype)

##############################
# Standardize Trait Values
##############################

cluster_matrix_scaled <- scale(cluster_matrix)

##############################
# Compute Euclidean Distance Matrix
##############################

distance_matrix <- dist(
  cluster_matrix_scaled,
  method = "euclidean"
)

##############################
# Perform Hierarchical Clustering
##############################

cluster_result <- hclust(
  distance_matrix,
  method = "ward.D2"
)

cluster_result$labels <- genotype_names

##############################
# Save Publication-quality Figure
##############################

tiff(

  filename = file.path(
    "Output",
    "Clustering",
    "Hierarchical_Clustering_Rice_Genotypes.tiff"
  ),

  width = 12,
  height = 8,
  units = "in",
  res = 600,
  compression = "lzw"

)

plot(

  cluster_result,

  hang = -1,

  cex = 0.8,

  main = "Hierarchical Clustering of Rice Genotypes",

  sub = "",

  xlab = "",

  ylab = "Euclidean Distance"

)

rect.hclust(

  cluster_result,

  k = 3,

  border = c("red","blue","forestgreen")

)

dev.off()

##############################
# Export Cluster Membership
##############################

cluster_membership <- data.frame(

  Genotype = genotype_names,

  Cluster = cutree(
    cluster_result,
    k = 3
  )

)

write.csv(

  cluster_membership,

  file.path(
    "Output",
    "Clustering",
    "Cluster_Membership.csv"
  ),

  row.names = FALSE

)

##############################
# Completion Message
##############################

cat("Hierarchical clustering completed successfully.\n")

cat("Results saved in Output/Clustering/\n")

##############################
# Session Information
##############################

sessionInfo()
