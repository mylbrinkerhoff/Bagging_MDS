#-------------------------------------------------------------------------------
#
# 04_Residual_H1.R
#
# Creating Residual H1 based on Chai & Garellek (2022)
#
# M. Brinkerhoff  * UCSC  * 2024-08-16 (F)
#
#-------------------------------------------------------------------------------

### Calculating Residual h1
#### Generate the lmer model for residual h1
model_position_h1c_covariant <- lmer(h1cz ~ energyz + 
                                       (energyz||Speaker),
                                     data = slz_normalized,
                                     REML = FALSE)

#### extract the energy factor
energy_factor <- fixef(model_position_h1c_covariant)[2]

#### generate the residual H1 score
slz_normalized$H1c_resid = slz_normalized$h1cz - slz_normalized$energyz * energy_factor

write.csv(slz_normalized, 
          file = "data/processed/slz_normalized.csv", 
          row.names = F, 
          fileEncoding = "UTF-8")

