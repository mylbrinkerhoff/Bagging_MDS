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
  mutate(idnum = row_number())

# Preprocessing the data for bagging
slz_bagging <- slz %>%
  mutate(h1c = (H1c_means005 + H1c_means006)/2,
         h2c = (H2c_means005 + H2c_means006)/2,
         h4c = (H4c_means005 + H4c_means006)/2,
         a1c = (A1c_means005 + A1c_means006)/2,
         a2c = (A2c_means005 + A2c_means006)/2,
         a3c = (A3c_means005 + A3c_means006)/2,
         h1h2c = (H1H2c_means005 + H1H2c_means006)/2,
         h2h4c = (H2H4c_means005 + H2H4c_means006)/2,
         h1a1c = (H1A1c_means005 + H1A1c_means006)/2,
         h1a2c = (H1A2c_means005 + H1A2c_means006)/2,
         h1a3c = (H1A3c_means005 + H1A3c_means006)/2,
         h42Kc = (H42Kc_means005 + H42Kc_means006)/2,
         h2Kh5Kc = (H2KH5Kc_means005 + H2KH5Kc_means006)/2,
         cpp = (CPP_means005 + CPP_means006)/2,
         energy = (Energy_means005 + Energy_means006)/2,
         hnr05 = (HNR05_means005 + HNR05_means006)/2,
         hnr15 = (HNR15_means005 + HNR15_means006)/2,
         hnr25 = (HNR25_means005 + HNR25_means006)/2,
         hnr35 = (HNR35_means005 + HNR35_means006)/2,
         shr = (SHR_means005 + SHR_means006)/2,
         f0 = (strF0_means005 + strF0_means006)/2,
         f1 = (sF1_means005 + sF1_means006)/2,
         f2 = (sF2_means005 + sF2_means006)/2,
         soe = (soe_means005 + soe_means006)/2) %>% 
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
