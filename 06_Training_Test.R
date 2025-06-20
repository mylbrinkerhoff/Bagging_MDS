#-------------------------------------------------------------------------------
#
# 06_Training_Test.R
#
# Creating stratified training and test data
#
# M. Brinkerhoff  * UCSC  * 2024-08-16 (F)
#
#-------------------------------------------------------------------------------

# Loading Data
slz_clean <- read.csv("data/processed/slz_normalized.csv", header = TRUE)

### convert certain columns into factors.
slz_clean$Phonation <- factor(slz_clean$Phonation, levels = c("modal", 
                                                              "breathy", 
                                                              "checked", 
                                                              "rearticulated"))
slz_clean$Speaker <- slz_clean$Speaker %>% factor()
slz_clean$Word <- slz_clean$Word %>% factor()
slz_clean$Vowel <- slz_clean$Vowel %>% factor()
slz_clean$Tone <- slz_clean$Tone %>% factor()

# stratified sampling so the training and test sets have similar distributions
table(slz_clean$Phonation) %>% prop.table() # initial distributions of VQ

slz_low <- slz_clean %>% filter(Tone == "L")
table(slz_low$Phonation) %>% prop.table() # distributions of VQ in low tone

# stratified sampling with the rsample package
set.seed(123) # needed for reproducibility

split_strat  <- initial_split(slz_clean, prop = 0.7, 
                              strata = "Phonation")
slz_train  <- training(split_strat)
slz_test   <- testing(split_strat)

# consistent response ratio between train & test
table(slz_train$Phonation) %>% prop.table()
table(slz_test$Phonation) %>% prop.table()

# For low tone
set.seed(123) # needed for reproducibility
split_strat_low  <- initial_split(slz_low, prop = 0.7, 
                                  strata = "Phonation")

slz_train_low  <- training(split_strat_low)
slz_test_low   <- testing(split_strat_low)

# consistent response ratio between train & test
table(slz_train_low$Phonation) %>% prop.table()
table(slz_test_low$Phonation) %>% prop.table()

# save the training and test sets
write.csv(slz_train, "data/processed/slz_train.csv", row.names = FALSE)
write.csv(slz_test, "data/processed/slz_test.csv", row.names = FALSE)

write.csv(slz_train_low, "data/processed/slz_train_low.csv", row.names = FALSE)
write.csv(slz_test_low, "data/processed/slz_test_low.csv", row.names = FALSE)
