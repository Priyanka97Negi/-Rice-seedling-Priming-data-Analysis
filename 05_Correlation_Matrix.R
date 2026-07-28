###############################################################################
# Project      : Rice Priming Analysis
# Script       : 05_Correlation_Matrix.R
#
# Description  :
# Computes Pearson correlation coefficients among seedling growth
# traits and generates a publication-quality correlation matrix.
#
# Input
#   Data/Exp_6_45DAS_Datasheet.csv
#
# Output
#   Output/Correlation/Correlation_Matrix.tiff
#
# Author
#   Priyanka Negi
###############################################################################

##############################
# Load Required Packages
##############################

library(tidyverse)
library(corrplot)

##############################
# Create Output Directory
##############################

if (!dir.exists("Output")) dir.create("Output")
if (!dir.exists("Output/Correlation"))
  dir.create("Output/Correlation")

##############################
# Import Dataset
##############################

phenotype_data <- read.csv(
  "Data/Exp_6_45DAS_Datasheet.csv",
  stringsAsFactors = FALSE
)

##############################
# Select Traits
##############################

trait_columns <- c(
  "SL45DAS",
  "SFW45DAS",
  "SDW45DAS",
  "RL45DAS",
  "RFW45DAS",
  "RDW45DAS"
)

correlation_data <- phenotype_data %>%
  select(all_of(trait_columns))

##############################
# Pearson Correlation Matrix
##############################

correlation_matrix <- cor(
  correlation_data,
  use = "pairwise.complete.obs",
  method = "pearson"
)

##############################
# Save Correlation Plot
##############################

tiff(

  file.path(
    "Output",
    "Correlation",
    "Correlation_Matrix.tiff"
  ),

  width = 8,
  height = 8,
  units = "in",
  res = 600,
  compression = "lzw"
)

corrplot(

  correlation_matrix,

  method = "ellipse",

  type = "upper",

  addCoef.col = "black",

  tl.cex = 0.9,

  number.cex = 0.8

)

dev.off()

sessionInfo()
