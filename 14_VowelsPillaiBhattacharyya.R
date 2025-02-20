# File: 14_VowelsPillaiBhattacharyya.R
# Project: Dissertation - Bagging
# Author: Mykel Brinkerhoff
# Date: 2024-12-19 (Th)
# Description: This script calculates the Pillai and Bhattacharyya Affinity
#   for the vowel space in SLZ to determine if there is a merger of /o/ and /u/.
#
# Usage:
#   Rscript 14_VowelsPillaiBhattacharyya.R
#
# Notes:
#   - Ensure all required packages are installed.
#   - Modify the script as needed for your specific dataset and analysis requirements.

#installing the necessary packages to perform Bhattacharyya Affinity
install.packages("adehabitatHR") # for performing Bhattacharyya's Affinity
install.packages("Cairo") # for saving the plots as .eps files
# install.packages("remotes") # <- if not already installed
remotes::install_github("joeystanley/barktools")

# Load necessary libraries
library(adehabitatHR) # for performing Bhattacharyya's Affinity
library(here) # for creating pathways relative to the top-level directory
library(tidyverse) # for data manipulation, graphic, and data wrangling
library(Cairo)
library(barktools)

# Loading the data
slz <- read.csv(here("data/raw/", "Voice_Master_Split.csv"))
slz_clean <- read.csv("data/processed/slz_normalized.csv", header = TRUE)

# Create a variable for colorblind palette
colorblind <- palette.colors(palette = "Okabe-Ito")

# Vowel overlap function from Joey Stanton at BYU 
# at https://joeystanley.com/blog/vowel-overlap-in-r-advanced-topics/
overlap <- function(F1, F2, vowel, method = "pillai") {
  
  vowel_data <- data.frame(vowel) %>%
    droplevels()
  
  if (nrow(vowel_data) < 1) return(NA) # not zero
  if (length(table(vowel_data)) < 2) return(NA) # both vowels are represented
  if (min(table(vowel_data)) < 5) return(NA) # there are at least 5 of each vowel
  
  if (method == "pillai") {
    summary(manova(cbind(F1, F2) ~ vowel))$stats["vowel", "Pillai"]
  } else if (method == "BA") {
    adehabitatHR::kerneloverlap(sp::SpatialPointsDataFrame(cbind(F1, F2), vowel_data), method='BA')[2,1]
  } else {
    warning("Improper method")
    return(NA)
  }
}

# Generating vowel plot
slz_vowels <- slz_clean %>% 
  ggplot(aes(x = f2, y = f1, colour = Vowel)) +
  geom_point() + 
  scale_x_reverse() + 
  scale_y_reverse() + 
  facet_wrap(.~Speaker) +
  scale_color_manual(values = colorblind, name = "Phonation") +
  theme_bw()
slz_vowels

# Adding vowel means
means <- slz_clean %>%
  summarise(f1 = mean(f1),
            f2 = mean(f2),
            .by = Vowel)
print(means)

ggplot(means, aes(x = f2, y = f1, label = Vowel)) + 
  geom_label() + 
  scale_x_reverse() + 
  scale_y_reverse() + 
  theme_bw()

slz_vowels <- slz_clean %>% 
  ggplot(aes(x = f2, y = f1, colour = Vowel, label = Vowel)) +
  geom_point(alpha = 0.2) +
  stat_ellipse(level = 0.67, linewidth = 1) +
  geom_label(data = means,colour = "black") +
  scale_x_reverse() + 
  scale_y_reverse() + 
  # facet_wrap(.~Speaker) +
  scale_color_manual(values = colorblind, name = "Phonation") +
  guides(color = FALSE) +
  labs(title = "Vowels in SLZ", x = "F2 (Hz)", y = "F1 (Hz)") +
  theme_bw()
slz_vowels

slz_vowels_speaker <- slz_clean %>% 
  ggplot(aes(x = f2, y = f1, colour = Vowel, label = Vowel)) +
  geom_point(alpha = 0.2) +
  stat_ellipse(level = 0.67, linewidth = 1) +
  geom_label(data = means,colour = "black") +
  scale_x_reverse() + 
  scale_y_reverse() + 
  facet_wrap(.~Speaker,  ncol = 5) +
  scale_color_manual(values = colorblind, name = "Phonation") +
  guides(color = FALSE) +
  labs(title = "Vowels in SLZ", x = "F2 (Hz)", y = "F1 (Hz)") +
  theme_bw()
slz_vowels_speaker

# with barktools
slz_vowels_bark <- slz_clean %>% 
  ggplot(aes(x = f2, y = f1, colour = Vowel, label = Vowel)) +
  geom_point(alpha = 0.2) +
  stat_ellipse(level = 0.67, linewidth = 1) +
  geom_label(data = means,colour = "black") +
  scale_x_bark(breaks = c(c(500, 1000, 1500, 2000, 3000)),
               minor_breaks = seq(0, 4000, 100)) +
  scale_y_bark(breaks = c(c(200, 400, 600, 800, 1000, 1500)),
               minor_breaks = seq(0, 3000, 100)) +
  scale_color_manual(values = colorblind, name = "Phonation") +
  guides(color = FALSE) +
  labs(title = "Vowels in SLZ", x = "F2 (Hz)", y = "F1 (Hz)") +
  theme_bw()
slz_vowels_bark

# Splitting the dataframe into male and female
df_f <- slz_clean %>%
  filter(str_detect(Speaker, 'f'))

df_m <- slz_clean %>% 
  filter(str_detect(Speaker, 'm'))

# F1 and F2 means for males and females

means_f <- df_f %>%
  summarise(f1 = mean(f1),
            f2 = mean(f2),
            .by = Vowel)

means_m <- df_m %>%
  summarise(f1 = mean(f1),
            f2 = mean(f2),
            .by = Vowel)

# Plotting the vowels by gender
slz_vowels_f <- df_f %>% 
  ggplot(aes(x = f2, y = f1, colour = Vowel, label = Vowel)) +
  geom_point(alpha = 0.2) +
  stat_ellipse(level = 0.67, linewidth = 1) +
  geom_label(data = means_f,colour = "black") +
  scale_x_reverse() + 
  scale_y_reverse() + 
  scale_color_manual(values = colorblind, name = "Phonation") +
  guides(color = FALSE) +
  labs(title = "Vowel Space for Female SLZ speakers", 
       x = "F2 (Hz)", 
       y = "F1 (Hz)") +
  theme_bw()
slz_vowels_f

slz_vowels_speaker_f <- df_f %>% 
  ggplot(aes(x = f2, y = f1, colour = Vowel, label = Vowel)) +
  geom_point(alpha = 0.2) +
  stat_ellipse(level = 0.67, linewidth = 1) +
  geom_label(data = means_f,colour = "black") +
  scale_x_reverse() + 
  scale_y_reverse() + 
  facet_wrap(.~Speaker) +
  scale_color_manual(values = colorblind, name = "Phonation") +
  guides(color = FALSE) +
  labs(title = "Vowel Space for each Female SLZ speaker", 
       x = "F2 (Hz)", 
       y = "F1 (Hz)") +
  theme_bw()
slz_vowels_speaker_f

slz_vowels_m <- df_m %>% 
  ggplot(aes(x = f2, y = f1, colour = Vowel, label = Vowel)) +
  geom_point(alpha = 0.2) +
  stat_ellipse(level = 0.67, linewidth = 1) +
  geom_label(data = means_m,colour = "black") +
  scale_x_reverse() + 
  scale_y_reverse() + 
  scale_color_manual(values = colorblind, name = "Phonation") +
  guides(color = FALSE) +
  labs(title = "Vowel Space for Male SLZ speakers", 
       x = "F2 (Hz)", 
       y = "F1 (Hz)") +
  theme_bw()
slz_vowels_m

slz_vowels_speaker_m <- df_m %>% 
  ggplot(aes(x = f2, y = f1, colour = Vowel, label = Vowel)) +
  geom_point(alpha = 0.2) +
  stat_ellipse(level = 0.67, linewidth = 1) +
  geom_label(data = means_m,colour = "black") +
  scale_x_reverse() + 
  scale_y_reverse() + 
  facet_wrap(.~Speaker) +
  scale_color_manual(values = colorblind, name = "Phonation") +
  guides(color = FALSE) +
  labs(title = "Vowel Space for each Male SLZ speaker", 
       x = "F2 (Hz)", 
       y = "F1 (Hz)") +
  theme_bw()
slz_vowels_speaker_m

# Creating dataframe for u_o merger
u_o <- slz_clean %>%
  filter(Vowel %in% c("u", "o"))

u_o_females <-df_f %>%
  filter(Vowel %in% c("o", "u"))

u_o_males <- df_m %>% 
  filter(Vowel %in% c("u", "o"))

# Calculating the Pillai scores and Bhattacharyya's Affinity
## mixed total
u_o %>% 
  summarise(pillai = overlap(f1, f2, vowel = Vowel, method = "pillai"),
            bhatt = overlap(f1, f2, vowel = Vowel, method = "BA"))

## females total
u_o_females %>% 
  summarise(pillai = overlap(f1, f2, vowel = Vowel, method = "pillai"),
            bhatt = overlap(f1, f2, vowel = Vowel, method = "BA"))

## Males total
u_o_males %>% 
  summarise(pillai = overlap(f1, f2, vowel = Vowel, method = "pillai"),
            bhatt = overlap(f1, f2, vowel = Vowel, method = "BA"))

## By spekaer
u_o %>% 
  summarise(pillai = overlap(f1, f2, vowel = Vowel, method = "pillai"),
            bhatt = overlap(f1, f2, vowel = Vowel, method = "BA"),
            .by = Speaker)

# saving the plots
ggsave("figs/slz_vowels.eps", 
  plot = slz_vowels, 
  width = 6, height = 3, 
  dpi = 300, units = "in", 
  device = cairo_ps)

ggsave("figs/slz_vowels_speaker.eps",
  plot = slz_vowels_speaker,
  width = 6, height = 3, 
  dpi = 300, units = "in", 
  device = cairo_ps)

ggsave("figs/slz_vowels_f.eps",
  plot = slz_vowels_f,
  width = 6, height = 3, 
  dpi = 300, units = "in", 
  device = cairo_ps)

ggsave("figs/slz_vowels_speaker_f.eps",
  plot = slz_vowels_speaker_f,
  width = 6, height = 3, 
  dpi = 300, units = "in", 
  device = cairo_ps)

ggsave("figs/slz_vowels_m.eps",
  plot = slz_vowels_m,
  width = 6, height = 3, 
  dpi = 300, units = "in", 
  device = cairo_ps)

ggsave("figs/slz_vowels_speaker_m.eps",
  plot = slz_vowels_speaker_m,
  width = 6, height = 3, 
  dpi = 300, units = "in", 
  device = cairo_ps)

# Saving the data
write.csv(u_o, "data/processed/u_o.csv", row.names = FALSE)
write.csv(u_o_females, "data/processed/u_o_females.csv", row.names = FALSE)
write.csv(u_o_males, "data/processed/u_o_males.csv", row.names = FALSE)
write.csv(means, "data/processed/vowel_means.csv", row.names = FALSE)
write.csv(means_f, "data/processed/vowel_means_f.csv", row.names = FALSE)
write.csv(means_m, "data/processed/vowel_means_m.csv", row.names = FALSE)
write.csv(df_f, "data/processed/df_f.csv", row.names = FALSE)
write.csv(df_m, "data/processed/df_m.csv", row.names = FALSE)
