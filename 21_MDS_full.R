# File: 13_MDS.R
# Project: Dissertation - Bagging
# Author: Mykel Brinkerhoff
# Date: 2024-09-12
# Description: This script performs Multidimensional Scaling (MDS) analysis.
#
# Usage:
#   Rscript 13_MDS.R
#
# Notes:
#   - Ensure all required packages are installed.
#   - Modify the script as needed for your specific dataset and analysis requirements.

# Load required libraries
library(dplyr) # For data manipulation
library(vegan) # For MDS analysis
library(ggplot2) # For data visualization
library(xtable) # For creating Latex tables
library(here) # For file path management

# Load the data
slz_plot <- read.csv("data/interim/slz_plot_standardized.csv", header = TRUE)
### convert certain columns into factors.
slz_plot$Phonation <- factor(slz_plot$Phonation, levels = c("modal", 
                                                              "breathy", 
                                                              "checked", 
                                                              "rearticulated"))
slz_plot$Speaker <- slz_plot$Speaker %>% factor()
slz_plot$Word <- slz_plot$Word %>% factor()
slz_plot$Vowel <- slz_plot$Vowel %>% factor()
slz_plot$Tone <- slz_plot$Tone %>% factor()

# Create a variable for colorblind palette
colorblind <- palette.colors(palette = "Okabe-Ito")

# recode the measurement.no as continuous variable
slz_plot$measurement.no <- as.numeric(slz_plot$measurement.no)

# Create a mapping for phonation types to their abbreviations
phonation_abbr <- c("breathy" = "B", "checked" = "C", "modal" = "M", "rearticulated" = "R")

# Create a new column that combines speaker and phonation abbreviation
slz_plot$Phonation_Abbr <- phonation_abbr[slz_plot$Phonation]
slz_plot$Speaker_Phonation <- paste(slz_plot$Speaker, slz_plot$Phonation_Abbr, sep = "_")

# Create a new dataframe with only the columns needed for MDS analysis
attach(slz_plot)
h1h2=tapply(h1h2cz, Speaker_Phonation, mean, na.rm=T)
h2h4=tapply(h2h4cz, Speaker_Phonation, mean, na.rm=T)
h1a1=tapply(h1a1cz, Speaker_Phonation, mean, na.rm=T)
h1a2=tapply(h1a2cz, Speaker_Phonation, mean, na.rm=T)
h1a3=tapply(h1a3cz, Speaker_Phonation, mean, na.rm=T)
cpp=tapply(cppz, Speaker_Phonation, mean, na.rm=T)
energy=tapply(energyz, Speaker_Phonation, mean, na.rm=T)
hnr05=tapply(hnr05z, Speaker_Phonation, mean, na.rm=T)
hnr15=tapply(hnr15z, Speaker_Phonation, mean, na.rm=T)
hnr25=tapply(hnr25z, Speaker_Phonation, mean, na.rm=T)
hnr35=tapply(hnr35z, Speaker_Phonation, mean, na.rm=T)
h1=tapply(h1cz, Speaker_Phonation, mean, na.rm=T)
h2=tapply(h2cz, Speaker_Phonation, mean, na.rm=T)
h4=tapply(h4cz, Speaker_Phonation, mean, na.rm=T)
a1=tapply(a1cz, Speaker_Phonation, mean, na.rm=T)
a2=tapply(a2cz, Speaker_Phonation, mean, na.rm=T)
a3=tapply(a3cz, Speaker_Phonation, mean, na.rm=T)
shr=tapply(shrz, Speaker_Phonation, mean, na.rm=T)
soe=tapply(norm.soe, Speaker_Phonation, mean, na.rm=T)
h42k=tapply(h42Kcz, Speaker_Phonation, mean, na.rm=T)
h2kh5k=tapply(h2Kh5Kcz, Speaker_Phonation, mean, na.rm=T)
h1_resid=tapply(H1c_resid, Speaker_Phonation, mean, na.rm=T)
duration=tapply(durationz, Speaker_Phonation, mean, na.rm=T)

# slz_mds for mds analysis
slz_mds = cbind(h1h2,h2h4,h1a1,h1a2,h1a3,h42k, h2kh5k,h2,h4,a1,a2,a3,cpp,hnr05,hnr15,hnr25,hnr35,soe,shr,energy,h1_resid)

# slz_mds_duration df. Same as slz_mds but with duration added
slz_mds_duration = cbind(h1h2,h2h4,h1a1,h1a2,h1a3,h42k, h2kh5k,h2,h4,a1,a2,a3,cpp,hnr05,hnr15,hnr25,hnr35,soe,shr,energy,h1_resid,duration)

# Find the minimum value in the dataframe
min_value <- min(slz_mds, na.rm = TRUE)
min_value_dur <- min(slz_mds_duration, na.rm = T)

# If the minimum value is negative, add its absolute value to all elements
if (min_value < 0) {
  slz_mds <- slz_mds + abs(min_value)
  slz_mds_duration <-  slz_mds_duration + abs(min_value_dur)
}

# set seed for reproducibility
set.seed(123)

# Perform MDS analysis
mds <- metaMDS(slz_mds, distance = "manhattan", wascores = TRUE, old.wa = TRUE, k = 4)

weights=abs(mds$species)
weights
weights_raw = mds$species
weights_raw

# Create a latex table of the MDS weights
weights_table <- xtable(weights)
weights_table
print(weights_table, file = here("results/", "weights_table_full.tex"))
weights_raw_tb <- xtable(weights_raw)
weights_raw_tb
print(weights_raw_tb, file = here("results/", "weights_raw_table_full.tex"))

# Extract NMDS scores
mds_scores <- scores(mds)

# Convert NMDS scores to a data frame
nmds_scores <- as.data.frame(mds_scores$sites)

# Add the Speaker_Phonation column to the scores data frame
nmds_scores$Speaker_Phonation <- rownames(nmds_scores)

# Add additional metadata for coloring (e.g., Phonation)
nmds_scores$Phonation <- slz_plot$Phonation[match(nmds_scores$Speaker_Phonation, slz_plot$Speaker_Phonation)]

# Create a ggplot2 scatter plot
nmds12 <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Phonation, label = Speaker_Phonation)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5) +
  theme_bw() +
  labs(title = "NMDS Plot (Dimension 1 x Dimension 2)", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = colorblind, name = "Phonation") +
  theme(legend.position = "bottom")
nmds12

nmds13 <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS3, color = Phonation, label = Speaker_Phonation)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5) +
  theme_bw() +
  labs(title = "NMDS Plot (Dimension 1 x Dimension 3)", x = "NMDS1", y = "NMDS3") +
  scale_color_manual(values = colorblind, name = "Phonation") +
  theme(legend.position = "bottom")
nmds13

nmds23 <- ggplot(nmds_scores, aes(x = NMDS2, y = NMDS3, color = Phonation, label = Speaker_Phonation)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5) +
  theme_bw() +
  labs(title = "NMDS Plot (Dimension 2 x Dimension 3)", x = "NMDS2", y = "NMDS3") +
  scale_color_manual(values = colorblind, name = "Phonation") +
  theme(legend.position = "bottom")
nmds23

nmds14 <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS4, color = Phonation, label = Speaker_Phonation)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5) +
  theme_bw() +
  labs(title = "NMDS Plot (Dimension 1 x Dimension 4)", x = "NMDS1", y = "NMDS4") +
  scale_color_manual(values = colorblind, name = "Phonation") +
  theme(legend.position = "bottom")
nmds14

nmds24 <- ggplot(nmds_scores, aes(x = NMDS2, y = NMDS4, color = Phonation, label = Speaker_Phonation)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5) +
  theme_bw() +
  labs(title = "NMDS Plot (Dimension 2 x Dimension 4)", x = "NMDS2", y = "NMDS4") +
  scale_color_manual(values = colorblind, name = "Phonation") +
  theme(legend.position = "bottom")
nmds24

nmds34 <- ggplot(nmds_scores, aes(x = NMDS3, y = NMDS4, color = Phonation, label = Speaker_Phonation)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5) +
  theme_bw() +
  labs(title = "NMDS Plot (Dimension 3 x Dimension 4)", x = "NMDS3", y = "NMDS4") +
  scale_color_manual(values = colorblind, name = "Phonation") + 
  theme(legend.position = "bottom")
nmds34

# Calculate stress values for different dimensions
stress_values <- sapply(1:10, function(k) {
  mds <- metaMDS(slz_mds, distance = "manhattan", k = k, trymax = 100)
  return(mds$stress)
})

# Create a dataframe for plotting
stress_df <- data.frame(Dimensions = 1:10, Stress = stress_values)

# Plot the scree plot
stress_plot <- ggplot(stress_df, aes(x = Dimensions, y = Stress)) +
  geom_point(size = 3) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(2, 10, by = 2)) + 
  labs(title = "Scree Plot for the MDS solution", x = "Number of Dimensions", y = "Stress")
stress_plot


ggsave(filename = "figs/nmds12.eps",
       plot = nmds12,
       width = 6, height = 4, dpi = 300, units = "in")

ggsave(filename = "figs/nmds13.eps",
        plot = nmds13,
        width = 6, height = 4, dpi = 300, units = "in")

ggsave(filename = "figs/nmds23.eps",
        plot = nmds23,
        width = 6, height = 4, dpi = 300, units = "in")

ggsave(filename = "figs/stress_plot.eps",
        plot = stress_plot,
        width = 6, height = 4, dpi = 300, units = "in")

# # Calculating r^2 for each parameter 
# nmds_dim1 <- mds$points[, 1] # dimension 1 
# nmds_dim2 <- mds$points[, 2] # dimension 2
# nmds_dim3 <- mds$points[, 3] # dimension 3

# # Extracting all parameter columns
# parameters <- slz_mds[, 1:21] # selecting all 21 parameters

# # Create an empty df to store correlations
# correlations <- data.frame(Parameter = colnames(parameters),
#                            Dim1 = NA,
#                            Dim2 = NA,
#                            Dim3 = NA)

# # loop through each parameter and calculate the r^2 for each dimension
# for (i in 1:ncol(parameters)) {
#   # calculate r^2 for dim1
#   correlations$Dim1[i] <- cor(parameters[, i], nmds_dim1)
  
#   # calculate r^2 for dim2
#   correlations$Dim2[i] <- cor(parameters[, i], nmds_dim2)
  
#   # calculate r^2 for dim3
#   correlations$Dim3[i] <- cor(parameters[, i], nmds_dim3)
# }

# correlations
# correlations_tb <- xtable(correlations, digits = 3)
# correlations_tb

# # Fit environmental variables
# envfit_result <- envfit(mds, slz_mds, permutations = 999)

# # Print the results
# print("Environmental fit results:")
# print(envfit_result)

# # Plot the NMDS result with environmental vectors
# plot(mds, type = "t")
# plot(envfit_result, p.max = 0.05)  # Only plot significant vectors

# Calculate correlations between original variables and NMDS dimensions
correlations_2 <- cor(slz_mds, mds_scores$sites)

# Print correlations
print("Correlations between original variables and NMDS dimensions:")
print(correlations_2)

### MDS with duration added

# set seed
set.seed(123)

# Determine the correct number for k
# Calculate stress values for different dimensions
stress_values_dur <- sapply(1:10, function(k) {
  mds_dur <- metaMDS(slz_mds_duration, distance = "manhattan", k = k, trymax = 100)
  return(mds_dur$stress)
})

# Create a dataframe for plotting
stress_df_dur <- data.frame(Dimensions = 1:10, Stress = stress_values_dur)

# Plot the scree plot
stress_plot_dur <- ggplot(stress_df_dur, aes(x = Dimensions, y = Stress)) +
  geom_point(size = 3) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(2, 10, by = 2)) + 
  labs(title = "Scree Plot for the MDS solution with Duration added", x = "Number of Dimensions", y = "Stress")
stress_plot_dur

# Perform MDS analysis
mds_dur <- metaMDS(slz_mds_duration, distance = "manhattan", wascores = TRUE, old.wa = TRUE, k = 3)

weights_dur=abs(mds_dur$species)
weights_dur
weights_raw_dur = mds_dur$species
weights_raw_dur

# Create a table of the MDS weights
weights_table_dur <- xtable(weights_dur)
weights_table_dur
print(weights_table_dur, file = here("results/", "weights_table_dur.tex"))
weights_raw_tb_dur <- xtable(weights_raw_dur)
weights_raw_tb_dur

# Extract NMDS scores
mds_scores_dur <- scores(mds_dur)

# Convert NMDS scores to a data frame
nmds_scores_dur <- as.data.frame(mds_scores_dur$sites)

# Add the Speaker_Phonation column to the scores data frame
nmds_scores_dur$Speaker_Phonation <- rownames(nmds_scores_dur)

# Add additional metadata for coloring (e.g., Phonation)
nmds_scores_dur$Phonation <- slz_clean$Phonation[match(nmds_scores_dur$Speaker_Phonation, slz_clean$Speaker_Phonation)]

# Create a ggplot2 scatter plot
nmds12_dur <- ggplot(nmds_scores_dur, aes(x = NMDS1, y = NMDS2, color = Phonation, label = Speaker_Phonation)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5) +
  theme_bw() +
  labs(title = "NMDS Plot with Duration added (Dimension 1 x Dimension 2)", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = colorblind, name = "Phonation")
nmds12_dur

nmds13_dur <- ggplot(nmds_scores_dur, aes(x = NMDS1, y = NMDS3, color = Phonation, label = Speaker_Phonation)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5) +
  theme_bw() +
  labs(title = "NMDS Plot with Duration added (Dimension 1 x Dimension 3)", x = "NMDS1", y = "NMDS3") +
  scale_color_manual(values = colorblind, name = "Phonation")
nmds13_dur

nmds23_dur <- ggplot(nmds_scores, aes(x = NMDS2, y = NMDS3, color = Phonation, label = Speaker_Phonation)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5) +
  theme_bw() +
  labs(title = "NMDS Plot with Duration added (Dimension 2 x Dimension 3)", x = "NMDS2", y = "NMDS3") +
  scale_color_manual(values = colorblind, name = "Phonation")
nmds23_dur

# Saving the plots
ggsave(filename = "figs/nmds12_dur.eps",
       plot = nmds12_dur,
       width = 6, height = 4, dpi = 300, units = "in")

ggsave(filename = "figs/nmds13_dur.eps",
       plot = nmds13_dur,
       width = 6, height = 4, dpi = 300, units = "in")

ggsave(filename = "figs/nmds23_dur.eps",
       plot = nmds23_dur,
       width = 6, height = 4, dpi = 300, units = "in")

ggsave(filename = "figs/stress_plot_dur.eps",
       plot = stress_plot_dur,
       width = 6, height = 4, dpi = 300, units = "in")

# Fit environmental variables
envfit_result_dur <- envfit(mds_dur, slz_mds_duration, permutations = 999, 
                            choices = c(1:3))

# Print the results
print("Environmental fit results:")
print(envfit_result_dur)

# Plot the NMDS result with environmental vectors
plot(mds_dur, type = "t")
plot(envfit_result_dur, p.max = 0.05)  # Only plot significant vectors

# Calculate correlations between original variables and NMDS dimensions
correlations_dur <- cor(slz_mds_duration, mds_scores_dur$sites)

# Print correlations
print("Correlations between original variables and NMDS dimensions:")
print(correlations_dur)
