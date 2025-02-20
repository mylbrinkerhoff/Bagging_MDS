# File: 15_TonePatterns.R
# Project: Dissertation - Bagging
# Author: Mykel Brinkerhoff
# Date: 2024-12-27 (F)
# Description: This script normalizes speakers 
#
# Usage:
#   Rscript 15_TonePatterns.R
#
# Notes:
#   - Ensure all required packages are installed.
#   - Modify the script as needed for your specific dataset and analysis 
#     requirements.

# loading libraries
library(here)
library(tidyverse) # for data manipulation, graphic, and data wrangling
library(scales) # for rescaling the f0z variable

# loading the data
tone <- read.csv(here("data", "interim", "slz_standardized_plot.csv"), header = TRUE)

# Create a variable for colorblind palette
colorblind <- palette.colors(palette = "Okabe-Ito")

# factorizing the tone column
tone$Tone <- factor(slz_clean$Tone)

# Rescaling the f0z variable to a range of 1 to 5
tone <- tone %>%
  group_by(Speaker) %>%
  mutate(f0z_scaled = rescale(f0z, to = c(1, 5))) %>%
  ungroup()

# Generating tone plot
slz_tones <- tone %>% 
    ggplot(aes(x = Duration, y = f0z, colour = Tone)) +
    geom_smooth(method = "loess") +
    scale_color_manual(values = colorblind, name = "Tone") +
    labs(title = "Tonal contours of SLZ", x = "Measurement Number", y = "F0 (z-score)") +
    # facet_wrap(.~Speaker, ncol = 5) +
    theme_bw()
slz_tones

slz_tones_scaled <- tone %>% 
    ggplot(aes(x = measurement.no, y = f0z_scaled, colour = Tone)) +
    geom_smooth(method = "loess") +
    scale_color_manual(values = colorblind, name = "Tone") +
    scale_x_continuous(breaks = unique(tone$measurement.no)) +
    # scale_y_continuous(breaks = 1:5) +
    labs(title = "Tonal contours of SLZ with Chao numbers", x = "Measurement Number", y = "Tone level") +
    theme_bw()
slz_tones_scaled
1
