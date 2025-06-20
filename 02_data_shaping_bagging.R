#-------------------------------------------------------------------------------
#
# 01_data_shaping_b.R
#
# taking only the 5th and 6th measure to create the dataframe
#
# M. Brinkerhoff  * UCSC  * 2024-08-16 (F)
#
#-------------------------------------------------------------------------------

### Rename seg_End
colnames(slz)[colnames(slz) == 'seg_End'] <- 'Duration'

### Rearranging values for analysis and comparison
slz <- slz %>% 
  dplyr::mutate(idnum = row_number())

# Preprocessing the data for bagging
slz_bagging <- slz %>%
  dplyr::mutate(h1c = (H1c_means004 + H1c_means005 + H1c_means006 + H1c_means007)/4,
         h2c = (H2c_means004 + H2c_means005 + H2c_means006 + H2c_means007)/4,
         h4c = (H4c_means004 + H4c_means005 + H4c_means006 + H4c_means007)/4,
         a1c = (A1c_means004 + A1c_means005 + A1c_means006 + A1c_means007)/4,
         a2c = (A2c_means004 + A2c_means005 + A2c_means006 + A2c_means007)/4,
         a3c = (A3c_means004 + A3c_means005 + A3c_means006 + A3c_means007)/4,
         h1h2c = (H1H2c_means004 + H1H2c_means005 + H1H2c_means006 + H1H2c_means007)/4,
         h2h4c = (H2H4c_means004 + H2H4c_means005 + H2H4c_means006 + H2H4c_means007)/4,
         h1a1c = (H1A1c_means004 + H1A1c_means005 + H1A1c_means006 + H1A1c_means007)/4,
         h1a2c = (H1A2c_means004 + H1A2c_means005 + H1A2c_means006 + H1A2c_means007)/4,
         h1a3c = (H1A3c_means004 + H1A3c_means005 + H1A3c_means006 + H1A3c_means007)/4,
         h42Kc = (H42Kc_means004 + H42Kc_means005 + H42Kc_means006 + H42Kc_means007)/4,
         h2Kh5Kc = (H2KH5Kc_means004 + H2KH5Kc_means005 + H2KH5Kc_means006 + H2KH5Kc_means007)/4,
         cpp = (CPP_means004 + CPP_means005 + CPP_means006 + CPP_means007)/4,
         energy = (Energy_means004 + Energy_means005 + Energy_means006 + Energy_means007)/4,
         hnr05 = (HNR05_means004 + HNR05_means005 + HNR05_means006 + HNR05_means007)/4,
         hnr15 = (HNR15_means004 + HNR15_means005 + HNR15_means006 + HNR15_means007)/4,
         hnr25 = (HNR25_means004 + HNR25_means005 + HNR25_means006 + HNR25_means007)/4,
         hnr35 = (HNR35_means004 + HNR35_means005 + HNR35_means006 + HNR35_means007)/4,
         shr = (SHR_means004 + SHR_means005 + SHR_means006 + SHR_means007)/4,
         f0 = (strF0_means004 + strF0_means005 + strF0_means006 + strF0_means007)/4,
         f1 = (sF1_means004 + sF1_means005 + sF1_means006 + sF1_means007)/4,
         f2 = (sF2_means004 + sF2_means005 + sF2_means006 + sF2_means007)/4,
         soe = (soe_means004 + soe_means005 + soe_means006 + soe_means007)/4) %>% 
   select(Speaker,
         Word,
         Iter,
         Vowel,
         Phonation,
         Tone,
         Duration,
         h1c,
         h2c,
         h4c,
         a1c,
         a2c,
         a3c,
         h1h2c,
         h2h4c,
         h1a1c,
         h1a2c,
         h1a3c,
         h42Kc,
         h2Kh5Kc,
         cpp,
         energy,
         hnr05,
         hnr15,
         hnr25,
         hnr35,
         shr,
         soe,
         f0,
         f1,
         f2)

# Saving the file 
write.csv(slz_bagging, 
          file = "data/interim/slz_bagging.csv", 
          row.names = F, 
          fileEncoding = "UTF-8")
