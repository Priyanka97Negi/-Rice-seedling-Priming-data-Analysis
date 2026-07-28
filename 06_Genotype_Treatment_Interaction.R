###############################################################################
# Project      : Rice Priming Analysis
# Script       : 06_Genotype_Treatment_Interaction.R
#
# Description  :
# Visualizes genotype × treatment interaction
# for root dry weight.
###############################################################################

library(tidyverse)

if (!dir.exists("Output")) dir.create("Output")
if (!dir.exists("Output/Interaction"))
  dir.create("Output/Interaction")

phenotype_data <- read.csv(
  "Data/Exp_6_45DAS_Datasheet.csv",
  stringsAsFactors = FALSE
)

interaction_data <- phenotype_data %>%

  group_by(
    Genotype,
    Treatment
  ) %>%

  summarise(

    RDW45DAS = mean(
      RDW45DAS,
      na.rm = TRUE
    ),

    .groups = "drop"

  )

interaction_plot <-

ggplot(

interaction_data,

aes(

x = Treatment,

y = RDW45DAS,

group = Genotype,

color = Genotype

)

)+

geom_line(

linewidth = 0.8

)+

geom_point(

size = 2

)+

labs(

title = "Genotype × Treatment Interaction",

subtitle = "Root Dry Weight (45 DAS)",

x = "Priming Treatment",

y = "Root Dry Weight"

)+

theme_bw(base_size = 14)+

theme(

legend.position = "none",

plot.title = element_text(

face="bold",

hjust=.5

),

plot.subtitle = element_text(

hjust=.5

)

)

ggsave(

file.path(

"Output",

"Interaction",

"GxT_Interaction_Root_Dry_Weight.tiff"

),

interaction_plot,

width=10,

height=6,

dpi=600,

compression="lzw"

)

sessionInfo()
