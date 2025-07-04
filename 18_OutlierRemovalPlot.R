#-------------------------------------------------------------------------------
#
# 19_OutlierRemovalPlot.R
#
# Cleaning and removing outliers
#
# M. Brinkerhoff  * UCSC  * 2024-08-16 (F)
#
#-------------------------------------------------------------------------------

### Remove outliers by F0, Formants, and Energy
#strF0 outlier flag
slz_plot = slz_trans %>%
  group_by(Speaker) %>%
  mutate(F0z = (f0 - mean(f0, na.rm = T)) / sd(f0, na.rm = T)) %>%
  ungroup()

slz_plot = slz_plot %>%
  mutate(str_outlier = if_else(abs(F0z) > 3, "outlier", "OK"))

### Formant outlier flagging
### Calculate Mahalanobis distance for formants
vmahalanobis = function(dat) {
  if (nrow(dat) < 25) {
    dat$zF1F2 = NA
    return(dat)
  }
  means = c(mean(dat$f1, na.rm = T), mean(dat$f2, na.rm = T))
  cov = cov(cbind(dat$f1, dat$f2))

  dat$zF1F2 = mahalanobis(cbind(dat$f1, dat$f2), center = means, cov = cov)
  dat
}

# Distance larger than 6 is considered as outlier
distance_cutoff = 6

# Perform Mahalanobis on dataset
slz_plot = slz_plot %>%
  group_by(Vowel) %>%
  do(vmahalanobis(.)) %>%
  ungroup() %>%
  mutate(formant_outlier = NA)

# Visualize the formants with flagged values
slz_plot %>%
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = f2, y = f1, color = zF1F2 > distance_cutoff)) +
  geom_point(size = 0.6) +
  facet_wrap(. ~ Vowel) +
  scale_y_reverse(limits = c(2000, 0), position = "right") +
  scale_x_reverse(limits = c(3500, 0), position = "top") +
  theme_bw()

for (i in 1:nrow(slz_plot)) {
  if (!is.na(slz_plot$zF1F2[i])) {
    if (slz_plot$zF1F2[i] > distance_cutoff) {
      slz_plot$formant_outlier[i] = "outlier"
    }
  }
}

# Visualize the vowel formant after exclusion
slz_plot %>%
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = f2, y = f1)) +
  geom_point(size = 0.6) +
  #geom_text()+
  facet_wrap(. ~ Vowel) +
  #geom_density_2d() +
  #  scale_color_manual(values=c('#a6611a','#slzc27d','#018571'))+
  scale_y_reverse(limits = c(2000, 0), position = "right") +
  scale_x_reverse(limits = c(3500, 0), position = "top") +
  theme_bw()

slz_plot <- slz_plot %>%
  filter(str_outlier == "OK")

slz_plot <- slz_plot %>%
  filter(is.na(formant_outlier))

# removing energy outliers
slz_plot$energy[slz_plot$energy == 0] <- NA

slz_plot <- slz_plot %>%
  mutate(energy = log10(energy))

slz_plot <- slz_plot %>%
  filter(!is.na(energy))

# removing the columns for outlier removal
slz_plot <- slz_plot %>%
  select(-c(str_outlier, formant_outlier, zF1F2, F0z))

# writing the  filtered dataframe
write.csv(
  slz_plot,
  file = "data/interim/slz_plot.csv",
  row.names = F,
  fileEncoding = "UTF-8"
)
