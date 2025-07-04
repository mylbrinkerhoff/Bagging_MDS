#-------------------------------------------------------------------------------
#
# 20_StandardizationPlotting.R
#
# standardization of the scores. This is all z-score except for soe which
# follows a different normalization based on Garellek, et al. 2020
#
# M. Brinkerhoff  * UCSC  * 2024-08-16 (F)
#
#-------------------------------------------------------------------------------

### Standardization of measurements for ease of comparison across different
### measures

slz_plot <- slz_plot %>%
  group_by(Speaker) %>%
  mutate(
    h1cz = (h1c - mean(h1c, na.rm = T)) / sd(h1c, na.rm = T),
    h2cz = (h2c - mean(h2c, na.rm = T)) / sd(h2c, na.rm = T),
    h4cz = (h4c - mean(h4c, na.rm = T)) / sd(h4c, na.rm = T),
    a1cz = (a1c - mean(a1c, na.rm = T)) / sd(a1c, na.rm = T),
    a2cz = (a2c - mean(a2c, na.rm = T)) / sd(a2c, na.rm = T),
    a3cz = (a3c - mean(a3c, na.rm = T)) / sd(a3c, na.rm = T),
    h1h2cz = (h1h2c - mean(h1h2c, na.rm = T)) / sd(h1h2c, na.rm = T),
    h2h4cz = (h2h4c - mean(h2h4c, na.rm = T)) / sd(h2h4c, na.rm = T),
    h42Kcz = (h42Kc - mean(h42Kc, na.rm = T)) / sd(h42Kc, na.rm = T),
    h2Kh5Kcz = (h2Kh5Kc - mean(h2Kh5Kc, na.rm = T)) / sd(h2Kh5Kc, na.rm = T),
    h1a1cz = (h1a1c - mean(h1a1c, na.rm = T)) / sd(h1a1c, na.rm = T),
    h1a2cz = (h1a2c - mean(h1a2c, na.rm = T)) / sd(h1a2c, na.rm = T),
    h1a3cz = (h1a3c - mean(h1a3c, na.rm = T)) / sd(h1a3c, na.rm = T),
    energyz = (energy - mean(energy, na.rm = T)) / sd(energy, na.rm = T),
    hnr05z = (hnr05 - mean(hnr05, na.rm = T)) / sd(hnr05, na.rm = T),
    hnr15z = (hnr15 - mean(hnr15, na.rm = T)) / sd(hnr15, na.rm = T),
    hnr25z = (hnr25 - mean(hnr25, na.rm = T)) / sd(hnr25, na.rm = T),
    hnr35z = (hnr35 - mean(hnr35, na.rm = T)) / sd(hnr35, na.rm = T),
    cppz = (cpp - mean(cpp, na.rm = T)) / sd(cpp, na.rm = T),
    shrz = (shr - mean(shr, na.rm = T)) / sd(shr, na.rm = T),
    durationz = (Duration - mean(Duration, na.rm = T)) / sd(Duration, na.rm = T)
  ) %>%
  mutate(
    log.soe = log10(soe + 0.001),
    m.log.soe = mean(log.soe, na.rm = T),
    sd.log.soe = sd(log.soe, na.rm = T),
    z.log.soe = (log.soe - m.log.soe) / sd.log.soe,
    max.soe = max(log.soe),
    min.soe = min(log.soe),
  ) %>%
  mutate(norm.soe = (log.soe - min.soe) / (max.soe - min.soe), ) %>%
  select(-c(log.soe, m.log.soe, sd.log.soe, z.log.soe, max.soe, min.soe)) %>%
  ungroup()

### Calculating Residual h1
#### Generate the lmer model for residual h1
model_position_h1c_covariant_plot <- lmer(
  h1cz ~
    energyz +
      (energyz || Speaker),
  data = slz_plot,
  REML = FALSE
)

#### extract the energy factor
energy_factor_plot <- fixef(model_position_h1c_covariant_plot)[2]

#### generate the residual H1 score
slz_plot$H1c_resid = slz_plot$h1cz - slz_plot$energyz * energy_factor_plot


write.csv(
  slz_plot,
  file = "data/interim/slz_plot_standardized.csv",
  row.names = F,
  fileEncoding = "UTF-8"
)
